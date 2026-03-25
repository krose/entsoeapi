# Get Intraday Cross-Border Transfer Limits (11.3)

intraday cross-border transfer limits of DC links

## Usage

``` r
intraday_cross_border_transfer_limits(
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

  the starting date of the in-scope period in POSIXct or YYYY-MM-DD
  HH:MM:SS format One year range limit applies

- period_end:

  the ending date of the outage in-scope period in POSIXct or YYYY-MM-DD
  HH:MM:SS format One year range limit applies

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
[`net_transfer_capacities()`](https://krose.github.io/entsoeapi/reference/net_transfer_capacities.md),
[`redispatching_cross_border()`](https://krose.github.io/entsoeapi/reference/redispatching_cross_border.md),
[`redispatching_internal()`](https://krose.github.io/entsoeapi/reference/redispatching_internal.md),
[`total_commercial_sched()`](https://krose.github.io/entsoeapi/reference/total_commercial_sched.md)

## Examples

``` r
df <- entsoeapi::intraday_cross_border_transfer_limits(
  eic_in = "10YFR-RTE------C",
  eic_out = "11Y0-0000-0265-K",
  period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
  period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A93&in_Domain=10YFR-RTE------C&out_Domain=11Y0-0000-0265-K&periodStart=202308152200&periodEnd=202308162200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 25 Mar 2026 19:09:42 GMT
#> <- content-type: text/xml
#> <- content-length: 1452
#> <- content-disposition: inline; filename="Cross-Border Capacity for DC Links Intraday_202308152200-202308162200.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!

dplyr::glimpse(df)
#> Rows: 24
#> Columns: 17
#> $ ts_in_domain_mrid             <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", …
#> $ ts_in_domain_name             <chr> "France", "France", "France", "France", "France", "France", "France", "France", …
#> $ ts_out_domain_mrid            <chr> "11Y0-0000-0265-K", "11Y0-0000-0265-K", "11Y0-0000-0265-K", "11Y0-0000-0265-K", …
#> $ ts_out_domain_name            <chr> "UK optimization area GB1A", "UK optimization area GB1A", "UK optimization area …
#> $ type                          <chr> "A93", "A93", "A93", "A93", "A93", "A93", "A93", "A93", "A93", "A93", "A93", "A9…
#> $ type_def                      <chr> "DC link capacity", "DC link capacity", "DC link capacity", "DC link capacity", …
#> $ ts_business_type              <chr> "B06", "B06", "B06", "B06", "B06", "B06", "B06", "B06", "B06", "B06", "B06", "B0…
#> $ ts_business_type_def          <chr> "DC link constraint", "DC link constraint", "DC link constraint", "DC link const…
#> $ created_date_time             <dttm> 2026-03-25 19:09:42, 2026-03-25 19:09:42, 2026-03-25 19:09:42, 2026-03-25 19:09…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_resolution                 <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M",…
#> $ ts_time_interval_start        <dttm> 2023-08-15 22:00:00, 2023-08-15 22:00:00, 2023-08-15 22:00:00, 2023-08-15 22:00:…
#> $ ts_time_interval_end          <dttm> 2023-08-16 22:00:00, 2023-08-16 22:00:00, 2023-08-16 22:00:00, 2023-08-16 22:00…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_point_dt_start             <dttm> 2023-08-15 22:00:00, 2023-08-15 23:00:00, 2023-08-16 00:00:00, 2023-08-16 01:00…
#> $ ts_point_quantity             <dbl> 1014, 1014, 1014, 1014, 1014, 1014, 1014, 1014, 1014, 1014, 1014, 1014, 1014, 10…
#> $ ts_quantity_measure_unit_name <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "M…
```
