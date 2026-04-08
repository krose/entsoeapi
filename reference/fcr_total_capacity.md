# Get FCR Total Capacity (SO GL 187.2)

Total Frequency Containment Reserve capacity.

## Usage

``` r
fcr_total_capacity(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the area domain

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- tidy_output:

  Defaults to TRUE. flatten nested tables

- security_token:

  Security token for ENTSO-E transparency platform

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with the queried data.

## See also

Other balancing endpoints:
[`activated_balancing_prices()`](https://krose.github.io/entsoeapi/reference/activated_balancing_prices.md),
[`aggregated_balancing_energy_bids()`](https://krose.github.io/entsoeapi/reference/aggregated_balancing_energy_bids.md),
[`allocation_of_cross_zonal_balancing_cap()`](https://krose.github.io/entsoeapi/reference/allocation_of_cross_zonal_balancing_cap.md),
[`balancing_border_cap_limit()`](https://krose.github.io/entsoeapi/reference/balancing_border_cap_limit.md),
[`balancing_energy_bids()`](https://krose.github.io/entsoeapi/reference/balancing_energy_bids.md),
[`changes_to_bid_availability()`](https://krose.github.io/entsoeapi/reference/changes_to_bid_availability.md),
[`contracted_reserves()`](https://krose.github.io/entsoeapi/reference/contracted_reserves.md),
[`current_balancing_state()`](https://krose.github.io/entsoeapi/reference/current_balancing_state.md),
[`elastic_demands()`](https://krose.github.io/entsoeapi/reference/elastic_demands.md),
[`exchanged_volumes()`](https://krose.github.io/entsoeapi/reference/exchanged_volumes.md),
[`exchanged_volumes_per_border()`](https://krose.github.io/entsoeapi/reference/exchanged_volumes_per_border.md),
[`financial_expenses_and_income()`](https://krose.github.io/entsoeapi/reference/financial_expenses_and_income.md),
[`hvdc_link_constrains()`](https://krose.github.io/entsoeapi/reference/hvdc_link_constrains.md),
[`imbalance_prices()`](https://krose.github.io/entsoeapi/reference/imbalance_prices.md),
[`imbalance_volumes()`](https://krose.github.io/entsoeapi/reference/imbalance_volumes.md),
[`netted_volumes()`](https://krose.github.io/entsoeapi/reference/netted_volumes.md),
[`netted_volumes_per_border()`](https://krose.github.io/entsoeapi/reference/netted_volumes_per_border.md),
[`procured_balancing_capacity()`](https://krose.github.io/entsoeapi/reference/procured_balancing_capacity.md),
[`rr_and_frr_actual_capacity()`](https://krose.github.io/entsoeapi/reference/rr_and_frr_actual_capacity.md),
[`shares_of_fcr_capacity()`](https://krose.github.io/entsoeapi/reference/shares_of_fcr_capacity.md),
[`sharing_of_rr_and_frr_capacity()`](https://krose.github.io/entsoeapi/reference/sharing_of_rr_and_frr_capacity.md)

## Examples

``` r
df <- entsoeapi::fcr_total_capacity(
  eic = "10YEU-CONT-SYNC0",
  period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-12-31", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A26&businessType=A25&area_Domain=10YEU-CONT-SYNC0&periodStart=202312312300&periodEnd=202412302300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 08 Apr 2026 13:14:22 GMT
#> <- content-type: text/xml
#> <- content-length: 1679
#> <- content-disposition: inline; filename="FCR Total Capacity_202312312300-202412302300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!

dplyr::glimpse(df)
#> Rows: 1
#> Columns: 19
#> $ area_domain_mrid              <chr> "10YEU-CONT-SYNC0"
#> $ area_domain_name              <chr> "Synchronous Zone of Continental Europe"
#> $ type                          <chr> "A26"
#> $ type_def                      <chr> "Capacity document"
#> $ process_type                  <chr> "A52"
#> $ process_type_def              <chr> "Frequency containment reserve"
#> $ ts_flow_direction             <chr> "A03"
#> $ ts_flow_direction_def         <chr> "UP and DOWN"
#> $ ts_business_type              <chr> "A25"
#> $ ts_business_type_def          <chr> "General Capacity Information"
#> $ created_date_time             <dttm> 2026-04-08 13:14:22
#> $ revision_number               <dbl> 1
#> $ ts_resolution                 <chr> "P1Y"
#> $ ts_time_interval_start        <dttm> 2023-12-31 23:00:00
#> $ ts_time_interval_end          <dttm> 2024-12-31 23:00:00
#> $ ts_mrid                       <dbl> 1
#> $ ts_point_dt_start             <dttm> 2023-12-31 23:00:00
#> $ ts_point_quantity             <dbl> 3000
#> $ ts_quantity_measure_unit_name <chr> "MAW"
```
