# Get RR and FRR Actual Capacity (SO GL 188.4 & 189.3)

Actual capacity of Frequency Restoration Reserve (FRR) or Replacement
Reserve (RR).

## Usage

``` r
rr_and_frr_actual_capacity(
  eic = NULL,
  process_type = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the area domain

- process_type:

  type of reserve "A56" FRR "A46" RR

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format

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
[`fcr_total_capacity()`](https://krose.github.io/entsoeapi/reference/fcr_total_capacity.md),
[`financial_expenses_and_income()`](https://krose.github.io/entsoeapi/reference/financial_expenses_and_income.md),
[`hvdc_link_constrains()`](https://krose.github.io/entsoeapi/reference/hvdc_link_constrains.md),
[`imbalance_prices()`](https://krose.github.io/entsoeapi/reference/imbalance_prices.md),
[`imbalance_volumes()`](https://krose.github.io/entsoeapi/reference/imbalance_volumes.md),
[`netted_volumes()`](https://krose.github.io/entsoeapi/reference/netted_volumes.md),
[`netted_volumes_per_border()`](https://krose.github.io/entsoeapi/reference/netted_volumes_per_border.md),
[`procured_balancing_capacity()`](https://krose.github.io/entsoeapi/reference/procured_balancing_capacity.md),
[`shares_of_fcr_capacity()`](https://krose.github.io/entsoeapi/reference/shares_of_fcr_capacity.md),
[`sharing_of_rr_and_frr_capacity()`](https://krose.github.io/entsoeapi/reference/sharing_of_rr_and_frr_capacity.md)

## Examples

``` r
df1 <- entsoeapi::rr_and_frr_actual_capacity(
  eic = "10YFR-RTE------C",
  process_type = "A46",
  period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-04-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A26&processType=A46&businessType=C78&Area_Domain=10YFR-RTE------C&periodStart=202312312300&periodEnd=202403312200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 31 Mar 2026 07:09:16 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="FRR and RR Actual Capacity_202312312300-202403312200.xml"
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

dplyr::glimpse(df1)
#> Rows: 6
#> Columns: 19
#> $ area_domain_mrid              <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", …
#> $ area_domain_name              <chr> "France", "France", "France", "France", "France", "France"
#> $ type                          <chr> "A26", "A26", "A26", "A26", "A26", "A26"
#> $ type_def                      <chr> "Capacity document", "Capacity document", "Capacity document", "Capacity documen…
#> $ process_type                  <chr> "A46", "A46", "A46", "A46", "A46", "A46"
#> $ process_type_def              <chr> "Replacement reserve", "Replacement reserve", "Replacement reserve", "Replacemen…
#> $ ts_flow_direction             <chr> "A02", "A02", "A01", "A02", "A01", "A01"
#> $ ts_flow_direction_def         <chr> "DOWN", "DOWN", "UP", "DOWN", "UP", "UP"
#> $ ts_business_type              <chr> "C77", "C79", "C78", "C78", "C77", "C79"
#> $ ts_business_type_def          <chr> "Minimum available capacity", "Maximum available capacity", "Average available c…
#> $ created_date_time             <dttm> 2026-03-31 07:09:16, 2026-03-31 07:09:16, 2026-03-31 07:09:16, 2026-03-31 07:09:…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1
#> $ ts_resolution                 <chr> "P3M", "P3M", "P3M", "P3M", "P3M", "P3M"
#> $ ts_time_interval_start        <dttm> 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00:…
#> $ ts_time_interval_end          <dttm> 2024-03-31 22:00:00, 2024-03-31 22:00:00, 2024-03-31 22:00:00, 2024-03-31 22:00:…
#> $ ts_mrid                       <dbl> 1, 2, 3, 4, 5, 6
#> $ ts_point_dt_start             <dttm> 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00:…
#> $ ts_point_quantity             <dbl> 2462, 27827, 9967, 14423, 2776, 21779
#> $ ts_quantity_measure_unit_name <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"

df2 <- entsoeapi::rr_and_frr_actual_capacity(
  eic = "10YFR-RTE------C",
  process_type = "A56",
  period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-04-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A26&processType=A56&businessType=C78&Area_Domain=10YFR-RTE------C&periodStart=202312312300&periodEnd=202403312200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 31 Mar 2026 07:09:20 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="FRR and RR Actual Capacity_202312312300-202403312200.xml"
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

dplyr::glimpse(df2)
#> Rows: 6
#> Columns: 19
#> $ area_domain_mrid              <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", …
#> $ area_domain_name              <chr> "France", "France", "France", "France", "France", "France"
#> $ type                          <chr> "A26", "A26", "A26", "A26", "A26", "A26"
#> $ type_def                      <chr> "Capacity document", "Capacity document", "Capacity document", "Capacity documen…
#> $ process_type                  <chr> "A56", "A56", "A56", "A56", "A56", "A56"
#> $ process_type_def              <chr> "Frequency restoration reserve", "Frequency restoration reserve", "Frequency res…
#> $ ts_flow_direction             <chr> "A02", "A02", "A01", "A02", "A01", "A01"
#> $ ts_flow_direction_def         <chr> "DOWN", "DOWN", "UP", "DOWN", "UP", "UP"
#> $ ts_business_type              <chr> "C77", "C79", "C78", "C78", "C77", "C79"
#> $ ts_business_type_def          <chr> "Minimum available capacity", "Maximum available capacity", "Average available c…
#> $ created_date_time             <dttm> 2026-03-31 07:09:20, 2026-03-31 07:09:20, 2026-03-31 07:09:20, 2026-03-31 07:09:…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1
#> $ ts_resolution                 <chr> "P3M", "P3M", "P3M", "P3M", "P3M", "P3M"
#> $ ts_time_interval_start        <dttm> 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00:…
#> $ ts_time_interval_end          <dttm> 2024-03-31 22:00:00, 2024-03-31 22:00:00, 2024-03-31 22:00:00, 2024-03-31 22:00:…
#> $ ts_mrid                       <dbl> 1, 2, 3, 4, 5, 6
#> $ ts_point_dt_start             <dttm> 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00:…
#> $ ts_point_quantity             <dbl> 381, 9151, 4604, 2931, 1173, 9348
#> $ ts_quantity_measure_unit_name <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"
```
