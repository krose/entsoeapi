# Get Netted Volumes (IFs IN 3.10)

The net position informs whether the given area imports or exports
energy. Those rows which hold the queried eic value in the
'ts_connecting_domain_mrid' column show the export value. Those rows
which hold the queried eic value in the 'ts_acquiring_domain_mrid'
column show the import value.

## Usage

``` r
netted_volumes(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the area

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format One day range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format One day range limit applies

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
[`netted_volumes_per_border()`](https://krose.github.io/entsoeapi/reference/netted_volumes_per_border.md),
[`procured_balancing_capacity()`](https://krose.github.io/entsoeapi/reference/procured_balancing_capacity.md),
[`rr_and_frr_actual_capacity()`](https://krose.github.io/entsoeapi/reference/rr_and_frr_actual_capacity.md),
[`shares_of_fcr_capacity()`](https://krose.github.io/entsoeapi/reference/shares_of_fcr_capacity.md),
[`sharing_of_rr_and_frr_capacity()`](https://krose.github.io/entsoeapi/reference/sharing_of_rr_and_frr_capacity.md)

## Examples

``` r
df <- entsoeapi::netted_volumes(
  eic = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2022-08-16", tz = "CET"),
  period_end = lubridate::ymd(x = "2022-08-17", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=B17&processType=A63&Acquiring_domain=10YCZ-CEPS-----N&Connecting_Domain=10YCZ-CEPS-----N&periodStart=202208152200&periodEnd=202208162200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Sun, 29 Mar 2026 15:49:56 GMT
#> <- content-type: application/zip
#> <- content-length: 1786
#> <- content-disposition: attachment; filename="Netted and Exchanged Volumes [IFs IN 3.10, mFRR 3.17, aFRR 3.16]_202208152200-202208162200.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/Rtmp8QGEwR/001-NETTED_AND_EXCHANGED_VOLUMES_202208152200-202208162200.xml has been read in
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!

dplyr::glimpse(df)
#> Rows: 96
#> Columns: 23
#> $ area_domain_mrid              <chr> "10Y1001C--00119X", "10Y1001C--00119X", "10Y1001C--00119X", "10Y1001C--00119X", …
#> $ area_domain_name              <chr> "Imbalance netting region", "Imbalance netting region", "Imbalance netting regio…
#> $ ts_connecting_domain_mrid     <chr> "10Y1001C--00119X", "10Y1001C--00119X", "10Y1001C--00119X", "10Y1001C--00119X", …
#> $ ts_connecting_domain_name     <chr> "Imbalance netting region", "Imbalance netting region", "Imbalance netting regio…
#> $ ts_acquiring_domain_mrid      <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", …
#> $ ts_acquiring_domain_name      <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", "Czech R…
#> $ doc_status_value              <chr> "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A0…
#> $ doc_status                    <chr> "Allocated capacity schedule", "Allocated capacity schedule", "Allocated capacit…
#> $ type                          <chr> "B17", "B17", "B17", "B17", "B17", "B17", "B17", "B17", "B17", "B17", "B17", "B1…
#> $ type_def                      <chr> "Aggregated netted external TSO schedule document", "Aggregated netted external …
#> $ process_type                  <chr> "A63", "A63", "A63", "A63", "A63", "A63", "A63", "A63", "A63", "A63", "A63", "A6…
#> $ process_type_def              <chr> "Imbalance Netting", "Imbalance Netting", "Imbalance Netting", "Imbalance Nettin…
#> $ ts_business_type              <chr> "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B0…
#> $ ts_business_type_def          <chr> "Net position", "Net position", "Net position", "Net position", "Net position", …
#> $ created_date_time             <dttm> 2026-03-29 15:49:56, 2026-03-29 15:49:56, 2026-03-29 15:49:56, 2026-03-29 15:49…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution                 <chr> "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M",…
#> $ ts_time_interval_start        <dttm> 2022-08-15 22:00:00, 2022-08-15 22:00:00, 2022-08-15 22:00:00, 2022-08-15 22:00…
#> $ ts_time_interval_end          <dttm> 2022-08-15 23:45:00, 2022-08-15 23:45:00, 2022-08-15 23:45:00, 2022-08-15 23:45…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start             <dttm> 2022-08-15 22:00:00, 2022-08-15 22:15:00, 2022-08-15 22:30:00, 2022-08-15 22:45…
#> $ ts_point_quantity             <dbl> 1.1357, 3.7387, 9.6364, 13.7141, 5.7463, 3.1041, 5.5129, 1.2547, 2.4147, 6.8177,…
#> $ ts_quantity_measure_unit_name <chr> "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MW…
```
