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
# see a spike every year in December due to Christmas, however this spike is 
# not present in the seasonally adjusted series. Overall they follow the same 
# trend, and the recessions are obvious in both series, arguably easier to see 
# in the adjusted series. 

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








# 3.1: Log Transformations ----

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
# 1975. Based on this, I don't think logging is a great option for this series
# because the size of the swings don't grow with the level, and instead grow
# and shrink in the middle of the series. 

retail_log_nsa <- retail_ts |> 
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

retail_log_nsa |> autoplot(log_sales)
retail_log_nsa |> autoplot(sales)

# In my opinion, the case for logging is stronger for the retail series. 
# The swings fan out over time for the non-logged series, and taking the log
# reveals a more consistent swing size over time. 






# 3.2: Growth Rates ----
can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |> autoplot(growth_direct) 

can_gas <- can_gas |> 
  mutate(growth_gap = abs(growth_direct - growth_logdiff))

can_gas |>autoplot(growth_gap)

# The log difference approach does alright in this case, being within 1% of 
# the exact growth rate for most of the series, but I would prefer the exact 
# growth rate. 



# 3.3: SA vs. NSA Growth Rates ----
retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

# As expected, the seasonally adjusted series has a much smoother MoM 
# growth rate. The December spikes in the non-adjusted series create big 
# growth rate spikes which are not present in the adjusted series. 






# 4.1-4.2: gg_season() and gg_subseries() -----
retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Retail Sales with Seasonal Adjustment",
    y = "Millions of dollars"
  ) +
  theme_minimal()

# The NSA plot very clearly shows the pattern of sales increasing in December. 
# The seasonally adjusted plot has removed this pattern, making sales
# throughout the year much more flat. 

can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Patterns in Canadian Natural Gas Production",
    y = "Volume"
  ) +
  theme_minimal()

# The seasonal pattern depends on weather conditions in this case, since 
# natural gas is often used for heating. It tends to be highest in December 
# and January when it is coldest outside, and lowest in June, July, and August
# during the peak of summer heat. 

can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )








# 4.3: X-13ARIMA-SEATS ---- 
retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

# The seasonal factor is highest in December of 1993, with a seasonal value of
# 1.213, meaning that sales in this month were 21.3% higher than the trend. 
# The size of the seasonal swings appears to get smaller over time. 

sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()

# Overall, my seasonal adjustment matches the Census Bureau's well, with the 
# difference being less than 1% for most of the series. The difference is  
# largest during the "irregular" periods from my model, meaning that the 
# Census Bureau must adjust for outliers different than the base model I used.


# 4.4: Linear Trend + Seasonal Dummies ----
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

# The estimated average monthly growth rate of gas production is 0.4%, and 
# June has the most significant seasonal affect, holding the largest absolute 
# value of it's coefficient relative to January. 

can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

# The line does a pretty poor job of tracking the series. It misses most of the 
# actual series between 1960-1985, and only tracks it well after this point 
# (other than the falloff starting in the 2000s).

can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)

# No, the residuals definitely do not look like white noise. They have an 
# obvious structure and we can even see seasonal swings in the residuals 
# starting around 1995. The Ljung-Box test confirms this at the 5% level, 
# with a statistic of 7774 and a p-value of essentially 0. 

# 5.1: Expanding Series TS-CV ----

gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

# The first forecast is evaluated on Jan 1980, and the last is on March 2005.

# model   MAE    RMSE
# 
# snaive  0.562  0.702
# tslm    4.01   4.36 

# The seasonal random walk model reports more accurately, as both the MAE and 
# RMSE are smaller. 

  





# 5.2: Diebold-Mariano Test ----
errs <- gas_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(can_gas) |> select(Month, actual = Volume),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)

# DM Statistic: -31.279, p-value: < 2.2e-16 (Basically 0)
# The large negative DM stat value indicates that the seasonal random walk
# model has statistically significantly lower forecast errors than the linear 
# trend + seasonal dummies model.


# 5.3: Rolling Window ----
gas_cv_fc_rolling <- can_gas |>
  select(Month, Volume) |>
  slide_tsibble(.size = 240, .step = 1) |> 
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc_rolling |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

# model   MAE    RMSE
# 
# snaive  0.562  0.702
# tslm    2.09   2.42 

# The linear trend + seasonal dummies model had a bigger change in accuracy
# when changing from an expanding window to a rolling window, with the MAE cut
# roughly in half and the RMSE cut almost in half. One reason why this might 
# be is because the growth rate of gas production has slowed down over time, 
# so using a rolling window allows the model to stop considering the older, 
# faster growth rate, and adjust to the slower growth rate later on. 




