# Get Imbalance Prices (17.1.G)

Imbalance prices of the control area.

## Usage

``` r
imbalance_prices(
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
[`financial_expenses_and_income()`](https://krose.github.io/entsoeapi/reference/financial_expenses_and_income.md),
[`hvdc_link_constrains()`](https://krose.github.io/entsoeapi/reference/hvdc_link_constrains.md),
[`imbalance_volumes()`](https://krose.github.io/entsoeapi/reference/imbalance_volumes.md),
[`netted_volumes()`](https://krose.github.io/entsoeapi/reference/netted_volumes.md),
[`netted_volumes_per_border()`](https://krose.github.io/entsoeapi/reference/netted_volumes_per_border.md),
[`procured_balancing_capacity()`](https://krose.github.io/entsoeapi/reference/procured_balancing_capacity.md),
[`rr_and_frr_actual_capacity()`](https://krose.github.io/entsoeapi/reference/rr_and_frr_actual_capacity.md),
[`shares_of_fcr_capacity()`](https://krose.github.io/entsoeapi/reference/shares_of_fcr_capacity.md),
[`sharing_of_rr_and_frr_capacity()`](https://krose.github.io/entsoeapi/reference/sharing_of_rr_and_frr_capacity.md)

## Examples

``` r
df <- entsoeapi::imbalance_prices(
  eic = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A85&controlArea_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 08 Apr 2026 13:08:44 GMT
#> <- content-type: application/zip
#> <- content-length: 1564
#> <- content-disposition: attachment; filename="Imbalance Prices_202312312300-202401012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpnNDu8x/001-IMBALANCE_PRICES_R3_202312312300-202401012300.xml has been read in
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!

dplyr::glimpse(df)
#> Rows: 72
#> Columns: 25
#> $ area_domain_mrid                    <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS---…
#> $ area_domain_name                    <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", "C…
#> $ doc_status_value                    <chr> "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02…
#> $ doc_status                          <chr> "Allocated capacity schedule", "Allocated capacity schedule", "Allocated c…
#> $ type                                <chr> "A85", "A85", "A85", "A85", "A85", "A85", "A85", "A85", "A85", "A85", "A85…
#> $ type_def                            <chr> "Imbalance prices", "Imbalance prices", "Imbalance prices", "Imbalance pri…
#> $ process_type                        <chr> "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16…
#> $ process_type_def                    <chr> "Realised", "Realised", "Realised", "Realised", "Realised", "Realised", "R…
#> $ ts_business_type                    <chr> "A19", "A19", "A19", "A19", "A19", "A19", "A19", "A19", "A19", "A19", "A19…
#> $ ts_business_type_def                <chr> "Balance energy deviation", "Balance energy deviation", "Balance energy de…
#> $ created_date_time                   <dttm> 2026-04-08 13:08:44, 2026-04-08 13:08:44, 2026-04-08 13:08:44, 2026-04-08…
#> $ revision_number                     <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution                       <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "P…
#> $ ts_time_interval_start              <dttm> 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31…
#> $ ts_time_interval_end                <dttm> 2024-01-01 23:00:00, 2024-01-01 23:00:00, 2024-01-01 23:00:00, 2024-01-01…
#> $ ts_mrid                             <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start                   <dttm> 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2024-01-01 00:00:00, 2024-01-01…
#> $ imbalance_price_amount              <dbl> -911.63, -911.63, -491.43, -491.43, -1237.16, -1237.16, -1977.85, -1977.85…
#> $ financial_price_amount              <dbl> -709.83, 0.00, -35.73, 0.00, -697.36, 0.00, -687.15, 0.00, -376.95, 0.00, …
#> $ ts_currency_unit_name               <chr> "CZK", "CZK", "CZK", "CZK", "CZK", "CZK", "CZK", "CZK", "CZK", "CZK", "CZK…
#> $ ts_price_measure_unit_name          <chr> "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH…
#> $ imbalance_price_category            <chr> "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04…
#> $ imbalance_price_category_def        <chr> "Excess balance", "Excess balance", "Excess balance", "Excess balance", "E…
#> $ financial_price_descriptor_type     <chr> "A02", "A03", "A02", "A03", "A02", "A03", "A02", "A03", "A02", "A03", "A02…
#> $ financial_price_descriptor_type_def <chr> "Incentive", "Financial neutrality", "Incentive", "Financial neutrality", …
```
