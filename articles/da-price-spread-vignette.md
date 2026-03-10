# Day-Ahead Price Spread Vignette

``` r
library(entsoeapi)
library(dplyr) |> suppressPackageStartupMessages()
library(lubridate) |> suppressPackageStartupMessages()
library(cli)
library(ggplot2)
```

### Look for the Polish market EIC and set the start and the end of scope dates

``` r
pl_eic <- all_approved_eic() |>
  filter(EicLongName == "Poland") |>
  select("EicCode") |>
  unlist()
#> ℹ downloading X_eicCodes.csv file ...
#> ℹ downloading Y_eicCodes.csv file ...
#> ℹ downloading Z_eicCodes.csv file ...
#> ℹ downloading T_eicCodes.csv file ...
#> ℹ downloading V_eicCodes.csv file ...
#> ℹ downloading W_eicCodes.csv file ...
#> ℹ downloading A_eicCodes.csv file ...

from_ts <- force_tz(time = as.Date("2026-01-01"), tzone = "CET")
till_ts <- from_ts + weeks(x = 1L)

cli_inform("Polish EIC: '{pl_eic}'")
#> Polish EIC: '10YPL-AREA-----S'
cli_inform("from: {from_ts}")
#> from: 2026-01-01
cli_inform("till: {till_ts}")
#> till: 2026-01-08
```

### Query the Polish DA prices within the pre-set period

``` r
da_prices <- entsoeapi::day_ahead_prices(
  eic = pl_eic,
  period_start = from_ts,
  period_end = till_ts,
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A44&in_Domain=10YPL-AREA-----S&out_Domain=10YPL-AREA-----S&periodStart=202512312300&periodEnd=202601072300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 10 Mar 2026 17:33:24 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Energy_Prices_202512312300-202601072300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <-
#> ✔ response has arrived
#> ℹ pulling Y_eicCodes.csv file from cache
#> ℹ No additional definitions added!
glimpse(da_prices)
#> Rows: 672
#> Columns: 22
#> $ ts_in_domain_mrid                     <chr> "10YPL-AREA-----S", "10YPL-AREA-----S", "10YPL-AREA-----S", "10YPL-AREA-…
#> $ ts_in_domain_name                     <chr> "Poland", "Poland", "Poland", "Poland", "Poland", "Poland", "Poland", "P…
#> $ ts_out_domain_mrid                    <chr> "10YPL-AREA-----S", "10YPL-AREA-----S", "10YPL-AREA-----S", "10YPL-AREA-…
#> $ ts_out_domain_name                    <chr> "Poland", "Poland", "Poland", "Poland", "Poland", "Poland", "Poland", "P…
#> $ type                                  <chr> "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A…
#> $ type_def                              <chr> "Price Document", "Price Document", "Price Document", "Price Document", …
#> $ ts_contract_market_agreement_type     <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A…
#> $ ts_contract_market_agreement_type_def <chr> "Daily contract", "Daily contract", "Daily contract", "Daily contract", …
#> $ ts_auction_type                       <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A…
#> $ ts_auction_type_def                   <chr> "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", …
#> $ ts_business_type                      <chr> "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A…
#> $ ts_business_type_def                  <chr> "Spot price", "Spot price", "Spot price", "Spot price", "Spot price", "S…
#> $ created_date_time                     <dttm> 2026-03-10 17:33:24, 2026-03-10 17:33:24, 2026-03-10 17:33:24, 2026-03-…
#> $ revision_number                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_resolution                         <chr> "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", …
#> $ ts_time_interval_start                <dttm> 2025-12-31 23:00:00, 2025-12-31 23:00:00, 2025-12-31 23:00:00, 2025-12-…
#> $ ts_time_interval_end                  <dttm> 2026-01-01 23:00:00, 2026-01-01 23:00:00, 2026-01-01 23:00:00, 2026-01-…
#> $ ts_mrid                               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_point_dt_start                     <dttm> 2025-12-31 23:00:00, 2025-12-31 23:15:00, 2025-12-31 23:30:00, 2025-12-…
#> $ ts_point_price_amount                 <dbl> 109.53, 107.86, 101.61, 80.43, 107.86, 96.77, 75.70, 64.00, 80.43, 82.78…
#> $ ts_currency_unit_name                 <chr> "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "E…
#> $ ts_price_measure_unit_name            <chr> "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "M…
```

### Calculate the daily minimum and maximum prices and the spread

``` r
da_spreads <- da_prices |>
  select(c(ts_point_dt_start, ts_point_price_amount)) |>
  mutate(
    ts_point_dt_start = with_tz(time = ts_point_dt_start, tzone = "CET")
  ) |>
  mutate(ts_point_date = as.Date(x = ts_point_dt_start, tz = "CET")) |>
  group_by(ts_point_date) |>
  mutate(
    max_price = max(ts_point_price_amount, na.rm = TRUE),
    min_price = min(ts_point_price_amount, na.rm = TRUE)
  ) |>
  mutate(
    price_spread = max_price - min_price
  ) |>
  select(c(ts_point_date, min_price, max_price, price_spread)) |>
  unique()
```

### Plot the daily minimum and maximum prices and the spread

``` r
ggplot(data = da_spreads) +
  geom_segment(
    mapping = aes(
      x = ts_point_date,
      xend = ts_point_date,
      y = min_price,
      yend = max_price
    ),
    linewidth = 10,
    color = "tomato"
  ) +
  geom_text(
    mapping = aes(
      x = ts_point_date,
      y = max_price + 10,
      label = price_spread
    ),
    size = 4
  ) +
  scale_x_date(
    breaks = da_spreads$ts_point_date,
    date_labels = "%Y-%m-%d"
  ) +
  theme_minimal() +
  labs(
    title = "Polish Daily Spread of Day-Ahead Prices",
    subtitle = paste("PT15M,", from_ts, till_ts),
    x = "delivery date",
    y = "day-ahead price spread"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
```

![](da-price-spread-vignette_files/figure-html/plot%20the%20daily%20spreads-1.png)
