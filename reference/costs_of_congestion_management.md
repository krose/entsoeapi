# Get Costs of Congestion Management (13.1.C)

Costs of TSO for redispatching and counter trading together with costs
for any other remedial actions taken to relieve congested lines in
transmission grid.

## Usage

``` r
costs_of_congestion_management(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 31L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  event_nature = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the control area

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- event_nature:

  "A46" for system Operator redispatching "B03" for counter trade "B04"
  for congestion costs Defaults to NULL which means both of them.

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
[`countertrading()`](https://krose.github.io/entsoeapi/reference/countertrading.md),
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
# Belgium's Costs of Congestion Management
df <- entsoeapi::costs_of_congestion_management(
  eic = "10YBE----------2",
  period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2016-12-31", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A92&in_Domain=10YBE----------2&out_Domain=10YBE----------2&periodStart=201512312300&periodEnd=201612302300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 31 Mar 2026 06:57:13 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Cost of Congestion Management_201512312300-201612302300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!

dplyr::glimpse(df)
#> Rows: 36
#> Columns: 18
#> $ ts_in_domain_mrid      <chr> "10YBE----------2", "10YBE----------2", "10YBE----------2", "10YBE----------2", "10YBE-…
#> $ ts_in_domain_name      <chr> "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Belgium",…
#> $ ts_out_domain_mrid     <chr> "10YBE----------2", "10YBE----------2", "10YBE----------2", "10YBE----------2", "10YBE-…
#> $ ts_out_domain_name     <chr> "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Belgium",…
#> $ type                   <chr> "A92", "A92", "A92", "A92", "A92", "A92", "A92", "A92", "A92", "A92", "A92", "A92", "A9…
#> $ type_def               <chr> "Congestion costs", "Congestion costs", "Congestion costs", "Congestion costs", "Conges…
#> $ process_type           <chr> "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A1…
#> $ process_type_def       <chr> "Realised", "Realised", "Realised", "Realised", "Realised", "Realised", "Realised", "Re…
#> $ ts_business_type       <chr> "B04", "B04", "B04", "B04", "B04", "B04", "B04", "B04", "B04", "B04", "B04", "B04", "A4…
#> $ ts_business_type_def   <chr> "Congestion costs", "Congestion costs", "Congestion costs", "Congestion costs", "Conges…
#> $ created_date_time      <dttm> 2026-03-31 06:57:13, 2026-03-31 06:57:13, 2026-03-31 06:57:13, 2026-03-31 06:57:13, 20…
#> $ revision_number        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_resolution          <chr> "P1M", "P1M", "P1M", "P1M", "P1M", "P1M", "P1M", "P1M", "P1M", "P1M", "P1M", "P1M", "P1…
#> $ ts_time_interval_start <dttm> 2015-12-31 23:00:00, 2015-12-31 23:00:00, 2015-12-31 23:00:00, 2015-12-31 23:00:00, 20…
#> $ ts_time_interval_end   <dttm> 2016-12-31 23:00:00, 2016-12-31 23:00:00, 2016-12-31 23:00:00, 2016-12-31 23:00:00, 20…
#> $ ts_mrid                <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, …
#> $ ts_point_dt_start      <dttm> 2015-12-31 23:00:00, 2016-01-31 23:00:00, 2016-03-02 23:00:00, 2016-03-31 23:00:00, 20…
#> $ ts_currency_unit_name  <chr> "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EU…
```
