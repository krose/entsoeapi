# title Get Countertrading (13.1.B)

Buying or cancelling generation on different side of the border to
relieve congested cross-border lines that exceeds its capacity. The time
interval in the query response depends on duration of matching counter
trades 100 documents limit applies!!

## Usage

``` r
countertrading(
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

  Energy Identification Code of the control area/bidding zone

- eic_out:

  Energy Identification Code of the control area/bidding zone

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format

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
[`cross_border_physical_flows()`](https://krose.github.io/entsoeapi/reference/cross_border_physical_flows.md),
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
# Counter trading between Germany and Denmark.
df <- entsoeapi::countertrading(
  eic_in = "10Y1001A1001A82H",
  eic_out = "10YDK-1--------W",
  period_start = lubridate::ymd(x = "2024-09-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-10-01", tz = "CET"),
  tidy_output = TRUE
)

dplyr::glimpse(df)
}
```
