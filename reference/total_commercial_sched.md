# Get Total Commercial Schedules (12.1.F)

Aggregated capacity nominated for all time horizons (including
Intra-Day) corresponding to implicit and explicit allocations after each
nomination process.

## Usage

``` r
total_commercial_sched(
  eic_in = NULL,
  eic_out = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic_in:

  Energy Identification Code of in domain

- eic_out:

  Energy Identification Code of out domain

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- tidy_output:

  Defaults to TRUE. If TRUE, then flatten nested tables.

- security_token:

  Security token for ENTSO-E transparency platform

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with the queried data.

## See also

Other transmission endpoints:
[`costs_of_congestion_management()`](https://krose.github.io/entsoeapi/reference/costs_of_congestion_management.md),
[`countertrading()`](https://krose.github.io/entsoeapi/reference/countertrading.md),
[`cross_border_physical_flows()`](https://krose.github.io/entsoeapi/reference/cross_border_physical_flows.md),
[`day_ahead_commercial_sched()`](https://krose.github.io/entsoeapi/reference/day_ahead_commercial_sched.md),
[`expansion_and_dismantling_project()`](https://krose.github.io/entsoeapi/reference/expansion_and_dismantling_project.md),
[`forecasted_transfer_capacities()`](https://krose.github.io/entsoeapi/reference/forecasted_transfer_capacities.md),
[`intraday_cross_border_transfer_limits()`](https://krose.github.io/entsoeapi/reference/intraday_cross_border_transfer_limits.md),
[`net_transfer_capacities()`](https://krose.github.io/entsoeapi/reference/net_transfer_capacities.md),
[`redispatching_cross_border()`](https://krose.github.io/entsoeapi/reference/redispatching_cross_border.md),
[`redispatching_internal()`](https://krose.github.io/entsoeapi/reference/redispatching_internal.md)

## Examples

``` r
if (FALSE) { # there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
df1 <- entsoeapi::total_commercial_sched(
  eic_in = "10YCZ-CEPS-----N",
  eic_out = "10YSK-SEPS-----K",
  period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2019-12-01", tz = "CET"),
  tidy_output = TRUE
)

dplyr::glimpse(df1)

df2 <- entsoeapi::total_commercial_sched(
  eic_in = "10YDK-1--------W",
  eic_out = "10Y1001A1001A82H",
  period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2019-12-01", tz = "CET"),
  tidy_output = TRUE
)

dplyr::glimpse(df2)
}
```
