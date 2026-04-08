# Get Exchanged Volumes (aFRR 3.16, mFRR 3.17)

The net position informs whether the given area imports or exports
energy. Those rows which hold the queried eic value in the
'ts_connecting_domain_mrid' column show the export value. Those rows
which hold the queried eic value in the 'ts_acquiring_domain_mrid'
column show the import value.

## Usage

``` r
exchanged_volumes(
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

  Energy Identification Code of the area

- process_type:

  type of frequency restoration reserve "A51" aFRR "A60" mFRR with
  scheduled activation "A61" mFRR with direct activation

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
[`exchanged_volumes_per_border()`](https://krose.github.io/entsoeapi/reference/exchanged_volumes_per_border.md),
[`fcr_total_capacity()`](https://krose.github.io/entsoeapi/reference/fcr_total_capacity.md),
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
df <- entsoeapi::exchanged_volumes(
  eic = "10YCZ-CEPS-----N",
  process_type = "A51",
  period_start = lubridate::ymd(x = "2022-08-16", tz = "CET"),
  period_end = lubridate::ymd_hm(x = "2022-08-16 02:00", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=B17&processType=A51&Acquiring_domain=10YCZ-CEPS-----N&Connecting_Domain=10YCZ-CEPS-----N&periodStart=202208152200&periodEnd=202208160000&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 08 Apr 2026 13:08:18 GMT
#> <- content-type: application/zip
#> <- content-length: 17783
#> <- content-disposition: attachment; filename="Netted and Exchanged Volumes [IFs IN 3.10, mFRR 3.17, aFRR 3.16]_202208152200-202208160000.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpnNDu8x/001-NETTED_AND_EXCHANGED_VOLUMES_202208152200-202208160000.xml has been read in
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!

dplyr::glimpse(df)
#> Rows: 3,600
#> Columns: 23
#> $ area_domain_mrid              <chr> "10Y1001C--00090V", "10Y1001C--00090V", "10Y1001C--00090V", "10Y1001C--00090V", …
#> $ area_domain_name              <chr> "aFRR Region", "aFRR Region", "aFRR Region", "aFRR Region", "aFRR Region", "aFRR…
#> $ ts_connecting_domain_mrid     <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", …
#> $ ts_connecting_domain_name     <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", "Czech R…
#> $ ts_acquiring_domain_mrid      <chr> "10Y1001C--00090V", "10Y1001C--00090V", "10Y1001C--00090V", "10Y1001C--00090V", …
#> $ ts_acquiring_domain_name      <chr> "aFRR Region", "aFRR Region", "aFRR Region", "aFRR Region", "aFRR Region", "aFRR…
#> $ doc_status_value              <chr> "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A0…
#> $ doc_status                    <chr> "Allocated capacity schedule", "Allocated capacity schedule", "Allocated capacit…
#> $ type                          <chr> "B17", "B17", "B17", "B17", "B17", "B17", "B17", "B17", "B17", "B17", "B17", "B1…
#> $ type_def                      <chr> "Aggregated netted external TSO schedule document", "Aggregated netted external …
#> $ process_type                  <chr> "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A5…
#> $ process_type_def              <chr> "Automatic frequency restoration reserve", "Automatic frequency restoration rese…
#> $ ts_business_type              <chr> "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B0…
#> $ ts_business_type_def          <chr> "Net position", "Net position", "Net position", "Net position", "Net position", …
#> $ created_date_time             <dttm> 2026-04-08 13:08:18, 2026-04-08 13:08:18, 2026-04-08 13:08:18, 2026-04-08 13:08…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution                 <chr> "PT4S", "PT4S", "PT4S", "PT4S", "PT4S", "PT4S", "PT4S", "PT4S", "PT4S", "PT4S", …
#> $ ts_time_interval_start        <dttm> 2022-08-15 22:00:00, 2022-08-15 22:00:00, 2022-08-15 22:00:00, 2022-08-15 22:00…
#> $ ts_time_interval_end          <dttm> 2022-08-16, 2022-08-16, 2022-08-16, 2022-08-16, 2022-08-16, 2022-08-16, 2022-08…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start             <dttm> 2022-08-15 22:00:00, 2022-08-15 22:00:04, 2022-08-15 22:00:08, 2022-08-15 22:00…
#> $ ts_point_quantity             <dbl> 0.0011, 0.0011, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0040, 0.0098, 0.0105, …
#> $ ts_quantity_measure_unit_name <chr> "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MW…
```
