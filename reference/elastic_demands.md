# Get Elastic Demands (IFs aFRR 3.4 & mFRR 3.4)

Elastic demands for scheduled activation of standard aFRR/mFRR product.

## Usage

``` r
elastic_demands(
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

  Energy Identification Code of the scheduling area

- process_type:

  type of frequency restoration reserve "A47" mFRR "A51" aFRR

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
[`rr_and_frr_actual_capacity()`](https://krose.github.io/entsoeapi/reference/rr_and_frr_actual_capacity.md),
[`shares_of_fcr_capacity()`](https://krose.github.io/entsoeapi/reference/shares_of_fcr_capacity.md),
[`sharing_of_rr_and_frr_capacity()`](https://krose.github.io/entsoeapi/reference/sharing_of_rr_and_frr_capacity.md)

## Examples

``` r
df <- entsoeapi::elastic_demands(
  eic = "10YCZ-CEPS-----N",
  process_type = "A47",
  period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-08-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A37&processType=A47&businessType=B75&Acquiring_domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202407312200&securityToken=<...>
#> <- HTTP/2 400 
#> <- date: Wed, 08 Apr 2026 13:08:13 GMT
#> <- content-type: text/xml
#> <- content-length: 905
#> <- content-disposition: inline; filename="acknowledgement.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ℹ *** The request has been rephrased. ***
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A37&processType=A47&businessType=B75&Acquiring_domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202407312200&offset=0&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 08 Apr 2026 13:08:13 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Elastic Demands [IF mFRR 3.4, aFRR 3.5]_202401090930-202403141000.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A37&processType=A47&businessType=B75&Acquiring_domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202407312200&offset=100&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 08 Apr 2026 13:08:14 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Elastic Demands [IF mFRR 3.4, aFRR 3.5]_202403150500-202403300815.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A37&processType=A47&businessType=B75&Acquiring_domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202407312200&offset=200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 08 Apr 2026 13:08:14 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Elastic Demands [IF mFRR 3.4, aFRR 3.5]_202403300815-202404300500.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A37&processType=A47&businessType=B75&Acquiring_domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202407312200&offset=300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 08 Apr 2026 13:08:15 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Elastic Demands [IF mFRR 3.4, aFRR 3.5]_202405021230-202405310445.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A37&processType=A47&businessType=B75&Acquiring_domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202407312200&offset=400&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 08 Apr 2026 13:08:15 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Elastic Demands [IF mFRR 3.4, aFRR 3.5]_202405310445-202406270400.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A37&processType=A47&businessType=B75&Acquiring_domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202407312200&offset=500&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 08 Apr 2026 13:08:15 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Elastic Demands [IF mFRR 3.4, aFRR 3.5]_202406270400-202407311045.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!

dplyr::glimpse(df)
#> Rows: 539
#> Columns: 28
#> $ domain_mrid                                     <chr> "10Y1001C--00085O", "10Y1001C--00085O", "10Y1001C--00085O", "1…
#> $ domain_name                                     <chr> "mFRR region", "mFRR region", "mFRR region", "mFRR region", "m…
#> $ bid_ts_connecting_domain_mrid                   <chr> "10Y1001C--00085O", "10Y1001C--00085O", "10Y1001C--00085O", "1…
#> $ bid_ts_connecting_domain_name                   <chr> "mFRR region", "mFRR region", "mFRR region", "mFRR region", "m…
#> $ bid_ts_acquiring_domain_mrid                    <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "1…
#> $ bid_ts_acquiring_domain_name                    <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech R…
#> $ bid_ts_mrid                                     <chr> "TS_NEED_CEPS_UP", "TS_NEED_CEPS_UP", "TS_NEED_CEPS_UP", "TS_N…
#> $ bid_ts_auction_mrid                             <chr> "AUCTION-mFRR", "AUCTION-mFRR", "AUCTION-mFRR", "AUCTION-mFRR"…
#> $ subject_market_participant_market_role_type     <chr> "A35", "A35", "A35", "A35", "A35", "A35", "A35", "A35", "A35",…
#> $ subject_market_participant_market_role_type_def <chr> "MOL Responsible", "MOL Responsible", "MOL Responsible", "MOL …
#> $ type                                            <chr> "A37", "A37", "A37", "A37", "A37", "A37", "A37", "A37", "A37",…
#> $ type_def                                        <chr> "Reserve tender document", "Reserve tender document", "Reserve…
#> $ process_type                                    <chr> "A47", "A47", "A47", "A47", "A47", "A47", "A47", "A47", "A47",…
#> $ process_type_def                                <chr> "Manual frequency restoration reserve", "Manual frequency rest…
#> $ bid_ts_flow_direction                           <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01",…
#> $ bid_ts_flow_direction_def                       <chr> "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "DOWN", …
#> $ created_date_time                               <dttm> 2026-04-08 13:08:13, 2026-04-08 13:08:13, 2026-04-08 13:08:13…
#> $ revision_number                                 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ reserve_bid_period_time_interval_start          <dttm> 2024-01-09 09:30:00, 2024-01-09 09:30:00, 2024-01-09 09:30:00…
#> $ reserve_bid_period_time_interval_end            <dttm> 2024-03-14 10:00:00, 2024-03-14 10:00:00, 2024-03-14 10:00:00…
#> $ bid_ts_resolution                               <chr> "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M",…
#> $ bid_ts_time_interval_start                      <dttm> 2024-01-09 09:30:00, 2024-01-10 07:45:00, 2024-01-17 06:30:00…
#> $ bid_ts_time_interval_end                        <dttm> 2024-01-09 09:45:00, 2024-01-10 08:00:00, 2024-01-17 06:45:00…
#> $ bid_ts_point_dt_start                           <dttm> 2024-01-09 09:30:00, 2024-01-10 07:45:00, 2024-01-17 06:30:00…
#> $ bid_ts_point_quantity                           <dbl> 85, 100, 100, 100, 110, 110, 200, 200, 100, 50, 50, 50, 150, 5…
#> $ bid_ts_point_energy_price_amount                <dbl> 720, 999, 1000, 1000, 1300, 1300, 1300, 1300, 1000, -100, 450,…
#> $ bid_ts_currency_unit_name                       <chr> "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR",…
#> $ bid_ts_quantity_measure_unit_name               <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW",…
```
