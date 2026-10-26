library(pacman)
p_load(tidyverse, fpp3)

# 2.2-2.4: Setup ----
retail_raw <- read_csv("data/retail_sales_raw.csv")

head(retail_raw)

retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

head(retail_ts)
has_gaps(retail_ts)

# 2.5: Load Canadian Gas ---- 
key_vars(canadian_gas)
index_var(canadian_gas)

can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

head(can_gas)


# 2.6: Edits to Canadian Gas ----
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

# December has the highest average production (17.1), June has the lowest (14.9)

can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

# We sum the monthly values rather than averaging them because this gives us 
# the production over the entire quarter, which is more useful for analysis than 
# the monthly average across the quarter. Yes, the quarterly data still does appear
# to be seasonal as the production is typically high in Q1, falls off through Q2 and 
# Q3, and then rises again in Q4. 






# 2.7: Plotting with Autoplot ----
retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

# Visually, the season adjustment greatly smooths out the series. Retail sales
# see a spike every year in November and December due to Christmas, however 
# this spike is not present in the seasonally adjusted series. Overall they 
# follow the same trend, and highs/lows are present in both series, arguable 
# easier to see in the adjusted series. 

can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Monthly Natural Gas Production",
    x = "Month", y = "Volume (Billions of Cubic Meters)", color = NULL
  ) +
  theme_minimal()

retail_ts |>
  autoplot(sales) +
  geom_smooth() +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

# I added a trend line with geom_smooth() to see if the recessions caused 
# noticeable dips in the overall upward trend of the series 







# 3.1-3.2: Log Transformations ----

# The swings get bigger roughly between 1975-1990. Before that they are 
# relatively constant in size, and constant after as well, although they are 
# choppier, seemingly meaning that the swings happen faster. 

can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

can_gas |> autoplot(log_volume)

# After logging, the size differences in the swings are more obvious. They 
# start out relatively large, get smaller around 1973, then get bigger 
# between 1975-1990, and shrink considerably after 1990. After logging, the
# trend also shows much faster growth early on, and then slows down around 
# 1975. Based on this, I feel like logging is a good option for this series
# because it makes the trend more clear and the changes in the swing size more
# obvious. 

retail_ts <- retail_ts |> 
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))





