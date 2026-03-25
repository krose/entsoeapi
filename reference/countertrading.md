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
# Counter trading between Germany and Denmark.
df <- entsoeapi::countertrading(
  eic_in = "10Y1001A1001A82H",
  eic_out = "10YDK-1--------W",
  period_start = lubridate::ymd(x = "2024-09-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-10-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A91&in_Domain=10Y1001A1001A82H&out_Domain=10YDK-1--------W&periodStart=202408312200&periodEnd=202409302200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 25 Mar 2026 19:09:05 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Countertrading_202409041000-202409221200.xml"
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
#> Rows: 20
#> Columns: 23
#> $ ts_in_domain_mrid             <chr> "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A82H", …
#> $ ts_in_domain_name             <chr> "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Luxembur…
#> $ ts_out_domain_mrid            <chr> "10YDK-1--------W", "10YDK-1--------W", "10YDK-1--------W", "10YDK-1--------W", …
#> $ ts_out_domain_name            <chr> "Denmark DK1", "Denmark DK1", "Denmark DK1", "Denmark DK1", "Denmark DK1", "Denm…
#> $ type                          <chr> "A91", "A91", "A91", "A91", "A91", "A91", "A91", "A91", "A91", "A91", "A91", "A9…
#> $ type_def                      <chr> "Counter trade notice", "Counter trade notice", "Counter trade notice", "Counter…
#> $ process_type                  <chr> "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A1…
#> $ process_type_def              <chr> "Realised", "Realised", "Realised", "Realised", "Realised", "Realised", "Realise…
#> $ ts_flow_direction             <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A0…
#> $ ts_flow_direction_def         <chr> "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "U…
#> $ ts_business_type              <chr> "B03", "B03", "B03", "B03", "B03", "B03", "B03", "B03", "B03", "B03", "B03", "B0…
#> $ ts_business_type_def          <chr> "Counter trade", "Counter trade", "Counter trade", "Counter trade", "Counter tra…
#> $ created_date_time             <dttm> 2026-03-25 19:09:05, 2026-03-25 19:09:05, 2026-03-25 19:09:05, 2026-03-25 19:09…
#> $ ts_reason_code                <chr> "B24", "B24", "B24", "B24", "B24", "B24", "B24", "B24", "B24", "B24", "B24", "B2…
#> $ ts_reason_text                <chr> "Load flow overload", "Load flow overload", "Load flow overload", "Load flow ove…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_resolution                 <chr> "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M",…
#> $ ts_time_interval_start        <dttm> 2024-09-04 10:00:00, 2024-09-04 10:00:00, 2024-09-04 10:00:00, 2024-09-04 10:00:…
#> $ ts_time_interval_end          <dttm> 2024-09-04 14:00:00, 2024-09-04 14:00:00, 2024-09-04 14:00:00, 2024-09-04 14:00…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2
#> $ ts_point_dt_start             <dttm> 2024-09-04 10:00:00, 2024-09-04 10:15:00, 2024-09-04 10:30:00, 2024-09-04 10:45…
#> $ ts_point_quantity             <dbl> 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, …
#> $ ts_quantity_measure_unit_name <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "M…
```
