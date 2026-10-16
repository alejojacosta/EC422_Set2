library(tidyquant)
library(tidyverse)

# unemp_raw <- tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get  = "economic.data",
#   from = "1976-01-01"
# )
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw= read_rds("data/unemp_raw.rds")


#Cleaning Data
unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region= case_when(
      symbol== "UNRATE" ~ "United States",
      symbol== "ORUR" ~ "Oregon",
      symbol== "CAUR" ~ "California",
      symbol== "TXUR" ~ "Texas",
      symbol== "NYUR" ~ "New York")
  )

#Wrangle Data 
unemp <- unemp |>
  arrange(region,date) |>
  group_by(region) |>
  mutate(
    unemployment_rate= replace(
      unemployment_rate, date== as.Date("2025-10-01"),
      unemployment_rate[date== as.Date("2025-09-01")]
    )
  ) |>
ungroup()

unemp <- unemp |>
  arrange(region,date) |>
  group_by(region) |>
  mutate(unemployment_rate_change= unemployment_rate - lag(unemployment_rate))|>
  ungroup()

unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))

unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate =mean(unemployment_rate, na.rm= TRUE),
    min_rate =min(unemployment_rate, na.rm= TRUE),
    max_rate =max(unemployment_rate, na.rm= TRUE))

unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate =mean(unemployment_rate, na.rm= TRUE),
    min_rate =min(unemployment_rate, na.rm= TRUE),
    max_rate =max(unemployment_rate, na.rm= TRUE))

#Extremes
unemp_top_increases <- unemp |>
  slice_max(unemployment_rate_change, n=5)

unemp_top_decreases <- unemp |>
  slice_min(unemployment_rate_change, n=5)

#Omit 2020
unemp_top_increases_no_covid <- unemp|>
  filter(date < as.Date("2020-01-01")| date > as.Date("2020-12-31")) |>
  slice_max(unemployment_rate_change, n=5)


unemp_top_decreases_no_covid <- unemp|>
  filter(date < as.Date("2020-01-01")| date > as.Date("2020-12-31")) |>
  slice_min(unemployment_rate_change, n=5) 

#Visualize Data
ggplot( 
  data= unemp,
  mapping= aes(x=date, y=unemployment_rate, color=region)
) +
  geom_line() +
  labs(
    title="Unemployment Rate by Region",
    x="Date",
    y="Unemployment Rate (%)",
    color="Region"
  ) +
  theme_minimal()

ggplot( 
  data= unemp,
  mapping= aes(x=date, y=unemployment_rate, color=region)
) +
  geom_line() +
facet_wrap(~region, ncol=1) +
  labs(
    title="Unemployment Rate by Region",
    x="Date",
    y="Unemployment Rate (%)"
  ) +
  theme_minimal()

ggplot( 
  data= unemp_summary,
  mapping= aes(fct_reorder(region,mean_rate), y=mean_rate)) +
  geom_col() +
  labs(
    title="Mean Unemployment Rate by Region",
    x="Region",
    y="Mean Unemployment Rate (%)"
  ) +
  theme_minimal()

ggplot( 
  data= unemp_recent,
  mapping= aes(x=date, y=unemployment_rate, color=region)
) +
  geom_line() +
  facet_wrap(~region) +
  labs(
    title="Unemployment Rate by Region",
    x="Date",
    y="Unemployment Rate (%)"
  ) +
  theme_minimal()

#I plotted the recent unemployment rate by region to see the effects of the
#COVID-19 pandemic on unemployment rates. The plot shows a significant spike in unemployment rates
#around the year 2020 with a decrease in the following years.



  
