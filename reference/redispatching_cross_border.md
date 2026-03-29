# title Get Redispatching Cross Border (13.1.A)

Changes in production and load (increase or decrease) to relieve
congested internal lines that exceeds its capacity. 100 documents limit
applies!!

## Usage

``` r
redispatching_cross_border(
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

  Energy Identification Code of the control area

- eic_out:

  Energy Identification Code of the control area

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
[`countertrading()`](https://krose.github.io/entsoeapi/reference/countertrading.md),
[`cross_border_physical_flows()`](https://krose.github.io/entsoeapi/reference/cross_border_physical_flows.md),
[`day_ahead_commercial_sched()`](https://krose.github.io/entsoeapi/reference/day_ahead_commercial_sched.md),
[`expansion_and_dismantling_project()`](https://krose.github.io/entsoeapi/reference/expansion_and_dismantling_project.md),
[`forecasted_transfer_capacities()`](https://krose.github.io/entsoeapi/reference/forecasted_transfer_capacities.md),
[`intraday_cross_border_transfer_limits()`](https://krose.github.io/entsoeapi/reference/intraday_cross_border_transfer_limits.md),
[`net_transfer_capacities()`](https://krose.github.io/entsoeapi/reference/net_transfer_capacities.md),
[`redispatching_internal()`](https://krose.github.io/entsoeapi/reference/redispatching_internal.md),
[`total_commercial_sched()`](https://krose.github.io/entsoeapi/reference/total_commercial_sched.md)

## Examples

``` r
# Germany's cross-border redispatching between TenneT and 50Hertz TSO.
df <- entsoeapi::redispatching_cross_border(
  eic_in = "10YDE-EON------1",
  eic_out = "10YDE-VE-------2",
  period_start = lubridate::ymd(x = "2024-09-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-10-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A63&businessType=A46&in_Domain=10YDE-EON------1&out_Domain=10YDE-VE-------2&periodStart=202408312200&periodEnd=202409302200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Sun, 29 Mar 2026 15:52:08 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Redispatching_Cross_Border_202409010900-202409291445.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!

dplyr::glimpse(df)
#> Rows: 604
#> Columns: 25
#> $ ts_in_domain_mrid             <chr> "10YDE-EON------1", "10YDE-EON------1", "10YDE-EON------1", "10YDE-EON------1", …
#> $ ts_in_domain_name             <chr> "Germany TenneT DE", "Germany TenneT DE", "Germany TenneT DE", "Germany TenneT D…
#> $ ts_out_domain_mrid            <chr> "10YDE-VE-------2", "10YDE-VE-------2", "10YDE-VE-------2", "10YDE-VE-------2", …
#> $ ts_out_domain_name            <chr> "Germany 50Hertz", "Germany 50Hertz", "Germany 50Hertz", "Germany 50Hertz", "Ger…
#> $ type                          <chr> "A63", "A63", "A63", "A63", "A63", "A63", "A63", "A63", "A63", "A63", "A63", "A6…
#> $ type_def                      <chr> "Redispatch notice", "Redispatch notice", "Redispatch notice", "Redispatch notic…
#> $ process_type                  <chr> "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A1…
#> $ process_type_def              <chr> "Realised", "Realised", "Realised", "Realised", "Realised", "Realised", "Realise…
#> $ ts_flow_direction             <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A0…
#> $ ts_flow_direction_def         <chr> "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "U…
#> $ ts_business_type              <chr> "A46", "A46", "A46", "A46", "A46", "A46", "A46", "A46", "A46", "A46", "A46", "A4…
#> $ ts_business_type_def          <chr> "System Operator redispatching", "System Operator redispatching", "System Operat…
#> $ ts_mkt_psr_type               <chr> "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A0…
#> $ ts_mkt_psr_type_def           <chr> "Generation", "Generation", "Generation", "Generation", "Generation", "Generatio…
#> $ created_date_time             <dttm> 2026-03-29 15:52:08, 2026-03-29 15:52:08, 2026-03-29 15:52:08, 2026-03-29 15:52…
#> $ ts_reason_code                <chr> "B24", "B24", "B24", "B24", "B24", "B24", "B24", "B24", "B24", "B24", "B24", "B2…
#> $ ts_reason_text                <chr> "Load flow overload", "Load flow overload", "Load flow overload", "Load flow ove…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution                 <chr> "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M",…
#> $ ts_time_interval_start        <dttm> 2024-09-01 09:00:00, 2024-09-01 09:00:00, 2024-09-01 09:00:00, 2024-09-01 09:00…
#> $ ts_time_interval_end          <dttm> 2024-09-01 15:00:00, 2024-09-01 15:00:00, 2024-09-01 15:00:00, 2024-09-01 15:00…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2,…
#> $ ts_point_dt_start             <dttm> 2024-09-01 09:00:00, 2024-09-01 09:15:00, 2024-09-01 09:30:00, 2024-09-01 09:45…
#> $ ts_point_quantity             <dbl> 25.00, 51.25, 51.25, 51.25, 51.25, 70.00, 51.25, 51.25, 51.25, 51.25, 51.25, 51.…
#> $ ts_quantity_measure_unit_name <chr> "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MW…
```
