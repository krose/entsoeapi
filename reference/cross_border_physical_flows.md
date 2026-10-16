# Get Cross-Border Physical Flows (12.1.G)

It is the measured real flow of energy between the neighbouring areas on
the cross borders.

## Usage

``` r
cross_border_physical_flows(
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

  POSIXct or YYYY-MM-DD HH:MM:SS format Minimum time interval in query
  response is an MTU period, but 1 year range limit applies.

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
[`day_ahead_commercial_sched()`](https://krose.github.io/entsoeapi/reference/day_ahead_commercial_sched.md),
[`expansion_and_dismantling_project()`](https://krose.github.io/entsoeapi/reference/expansion_and_dismantling_project.md),
[`forecasted_transfer_capacities()`](https://krose.github.io/entsoeapi/reference/forecasted_transfer_capacities.md),
[`intraday_cross_border_transfer_limits()`](https://krose.github.io/entsoeapi/reference/intraday_cross_border_transfer_limits.md),
[`net_transfer_capacities()`](https://krose.github.io/entsoeapi/reference/net_transfer_capacities.md),
[`redispatching_cross_border()`](https://krose.github.io/entsoeapi/reference/redispatching_cross_border.md),
[`redispatching_internal()`](https://krose.github.io/entsoeapi/reference/redispatching_internal.md),
[`total_commercial_sched()`](https://krose.github.io/entsoeapi/reference/total_commercial_sched.md)

## Examples

``` r
if (FALSE) { # there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
df1 <- entsoeapi::cross_border_physical_flows(
  eic_in = "10Y1001A1001A83F",
  eic_out = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2020-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2020-01-02", tz = "CET"),
  tidy_output = TRUE
)

dplyr::glimpse(df1)

df2 <- entsoeapi::cross_border_physical_flows(
  eic_in = "10YCZ-CEPS-----N",
  eic_out = "10Y1001A1001A83F",
  period_start = lubridate::ymd(x = "2020-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2020-01-02", tz = "CET"),
  tidy_output = TRUE
)

dplyr::glimpse(df2)
}
```
