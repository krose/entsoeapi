# Get Net Transfer Capacities — Week/Month/Year Ahead (11.1)

Estimated net transfer capacities for the week-ahead, month-ahead, or
year-ahead time frames. For day-ahead NTC, use
[`forecasted_transfer_capacities()`](https://krose.github.io/entsoeapi/reference/forecasted_transfer_capacities.md)
instead.

## Usage

``` r
net_transfer_capacities(
  eic_in = NULL,
  eic_out = NULL,
  contract_type = "A02",
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

- contract_type:

  Contract market agreement type: "A02" = Week ahead "A03" = Month ahead
  "A04" = Year ahead Defaults to "A02" (Week ahead)

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
[`redispatching_cross_border()`](https://krose.github.io/entsoeapi/reference/redispatching_cross_border.md),
[`redispatching_internal()`](https://krose.github.io/entsoeapi/reference/redispatching_internal.md),
[`total_commercial_sched()`](https://krose.github.io/entsoeapi/reference/total_commercial_sched.md)

## Examples

``` r
df <- entsoeapi::net_transfer_capacities(
  eic_in = "10YCZ-CEPS-----N",
  eic_out = "10YSK-SEPS-----K",
  contract_type = "A02",
  period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-02-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A61&contract_MarketAgreement.Type=A02&in_Domain=10YCZ-CEPS-----N&out_Domain=10YSK-SEPS-----K&periodStart=202312312300&periodEnd=202401312300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 31 Mar 2026 06:58:23 GMT
#> <- content-type: text/xml
#> <- content-length: 987
#> <- content-disposition: inline; filename="acknowledgement.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional definitions have been added!

dplyr::glimpse(df)
#> Rows: 1
#> Columns: 3
#> $ created_date_time <dttm> 2026-03-31 06:58:23
#> $ reason_code       <chr> "999"
#> $ reason_text       <chr> "No matching data found for Data item FORECASTED_TRANSFER_CAPACITIES_EXPLICIT [11.1] (10YCZ…
```
