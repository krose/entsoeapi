# Get Financial Expenses and Income for Balancing (17.1.I)

Financial expenses and income for balancing of the control area.

## Usage

``` r
financial_expenses_and_income(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the control area domain

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
[`fcr_total_capacity()`](https://krose.github.io/entsoeapi/reference/fcr_total_capacity.md),
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
df <- entsoeapi::financial_expenses_and_income(
  eic = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2022-02-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2022-03-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A87&controlArea_Domain=10YCZ-CEPS-----N&periodStart=202201312300&periodEnd=202202282300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 08 Apr 2026 13:08:25 GMT
#> <- content-type: application/zip
#> <- content-length: 897
#> <- content-disposition: attachment; filename="Financial_expenses_and_income_for_balancing_202201312300-202202282300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpnNDu8x/001-FINANCIAL_EXPENSES_AND_INCOME_FOR_BALANCING_R3202201312300-202202282300.xml has been read in
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!

dplyr::glimpse(df)
#> Rows: 2
#> Columns: 21
#> $ control_area_domain_mrid      <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N"
#> $ control_area_domain_name      <chr> "Czech Republic", "Czech Republic"
#> $ doc_status_value              <chr> "A01", "A01"
#> $ doc_status                    <chr> "Balance responsible schedule", "Balance responsible schedule"
#> $ type                          <chr> "A87", "A87"
#> $ type_def                      <chr> "Financial situation", "Financial situation"
#> $ process_type                  <chr> "A16", "A16"
#> $ process_type_def              <chr> "Realised", "Realised"
#> $ financial_price_direction     <chr> "A01", "A02"
#> $ financial_price_direction_def <chr> "UP", "DOWN"
#> $ ts_business_type              <chr> "A99", "A99"
#> $ ts_business_type_def          <chr> "Financial information", "Financial information"
#> $ created_date_time             <dttm> 2026-04-08 13:08:25, 2026-04-08 13:08:25
#> $ revision_number               <dbl> 1, 1
#> $ ts_resolution                 <chr> "P1M", "P1M"
#> $ ts_time_interval_start        <dttm> 2022-01-31 23:00:00, 2022-01-31 23:00:00
#> $ ts_time_interval_end          <dttm> 2022-02-28 23:00:00, 2022-02-28 23:00:00
#> $ ts_mrid                       <dbl> 1, 1
#> $ ts_point_dt_start             <dttm> 2022-01-31 23:00:00, 2022-01-31 23:00:00
#> $ financial_price_amount        <dbl> 1087522143, 32564550
#> $ ts_currency_unit_name         <chr> "CZK", "CZK"
```
