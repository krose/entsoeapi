# Get Procured Balancing Capacity (GL EB 12.3.F)

Procured balancing capacity by TSOs. 100 documents limit applies; paging
is handled transparently.

## Usage

``` r
procured_balancing_capacity(
  eic = NULL,
  process_type = NULL,
  market_agreement_type = NULL,
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

  type of frequency restoration reserve "A51" aFRR "A52" FCR "A47" mFRR

- market_agreement_type:

  Optional market agreement type code "A01" Daily "A02" Weekly "A03"
  Monthly "A04" Yearly "A05" Total "A06" Long term "A07" Intraday "A13"
  = Hourly

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
[`rr_and_frr_actual_capacity()`](https://krose.github.io/entsoeapi/reference/rr_and_frr_actual_capacity.md),
[`shares_of_fcr_capacity()`](https://krose.github.io/entsoeapi/reference/shares_of_fcr_capacity.md),
[`sharing_of_rr_and_frr_capacity()`](https://krose.github.io/entsoeapi/reference/sharing_of_rr_and_frr_capacity.md)

## Examples

``` r
df <- entsoeapi::procured_balancing_capacity(
  eic = "10YCZ-CEPS-----N",
  process_type = "A51",
  period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A15&processType=A51&area_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&securityToken=<...>
#> <- HTTP/2 400 
#> <- date: Sun, 29 Mar 2026 15:51:36 GMT
#> <- content-type: text/xml
#> <- content-length: 920
#> <- content-disposition: inline; filename="acknowledgement.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ℹ *** The request has been rephrased. ***
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A15&processType=A51&area_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&offset=0&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Sun, 29 Mar 2026 15:51:41 GMT
#> <- content-type: application/zip
#> <- content-length: 2589
#> <- content-disposition: attachment; filename="Procured Balancing Capacity_202312312300-202401012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/Rtmp8QGEwR/001-PROCURED_BALANCING_CAPACITY_R3_202312312300-202401012300.xml has been read in
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A15&processType=A51&area_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&offset=100&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Sun, 29 Mar 2026 15:51:46 GMT
#> <- content-type: application/zip
#> <- content-length: 2501
#> <- content-disposition: attachment; filename="Procured Balancing Capacity_202312312300-202401012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/Rtmp8QGEwR/001-PROCURED_BALANCING_CAPACITY_R3_202401010500-202401011800.xml has been read in
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A15&processType=A51&area_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&offset=200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Sun, 29 Mar 2026 15:51:52 GMT
#> <- content-type: application/zip
#> <- content-length: 2492
#> <- content-disposition: attachment; filename="Procured Balancing Capacity_202312312300-202401012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/Rtmp8QGEwR/001-PROCURED_BALANCING_CAPACITY_R3_202401011400-202401011800.xml has been read in
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A15&processType=A51&area_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&offset=300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Sun, 29 Mar 2026 15:51:57 GMT
#> <- content-type: application/zip
#> <- content-length: 2009
#> <- content-disposition: attachment; filename="Procured Balancing Capacity_202312312300-202401012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/Rtmp8QGEwR/001-PROCURED_BALANCING_CAPACITY_R3_202401011600-202401012300.xml has been read in
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!

dplyr::glimpse(df)
#> Rows: 1,868
#> Columns: 26
#> $ area_domain_mrid              <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", …
#> $ area_domain_name              <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", "Czech R…
#> $ type                          <chr> "A15", "A15", "A15", "A15", "A15", "A15", "A15", "A15", "A15", "A15", "A15", "A1…
#> $ type_def                      <chr> "Acquiring System Operator Reserve Schedule", "Acquiring System Operator Reserve…
#> $ process_type                  <chr> "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A5…
#> $ process_type_def              <chr> "Automatic frequency restoration reserve", "Automatic frequency restoration rese…
#> $ market_agreement_type         <chr> "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A0…
#> $ market_agreement_type_def     <chr> "Yearly contract", "Yearly contract", "Yearly contract", "Yearly contract", "Yea…
#> $ ts_flow_direction             <chr> "A02", "A02", "A02", "A02", "A02", "A02", "A01", "A01", "A01", "A01", "A01", "A0…
#> $ ts_flow_direction_def         <chr> "DOWN", "DOWN", "DOWN", "DOWN", "DOWN", "DOWN", "UP", "UP", "UP", "UP", "UP", "U…
#> $ ts_business_type              <chr> "B95", "B95", "B95", "B95", "B95", "B95", "B95", "B95", "B95", "B95", "B95", "B9…
#> $ ts_business_type_def          <chr> "Procured capacity", "Procured capacity", "Procured capacity", "Procured capacit…
#> $ ts_mkt_psr_type               <chr> "A03", "A03", "A03", "A03", "A03", "A03", "A03", "A03", "A03", "A03", "A03", "A0…
#> $ ts_mkt_psr_type_def           <chr> "Resource Object", "Resource Object", "Resource Object", "Resource Object", "Res…
#> $ created_date_time             <dttm> 2026-03-29 15:51:41, 2026-03-29 15:51:41, 2026-03-29 15:51:41, 2026-03-29 15:51…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution                 <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M",…
#> $ ts_time_interval_start        <dttm> 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00…
#> $ ts_time_interval_end          <dttm> 2024-01-01 05:00:00, 2024-01-01 05:00:00, 2024-01-01 05:00:00, 2024-01-01 05:00…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5,…
#> $ ts_point_dt_start             <dttm> 2023-12-31 23:00:00, 2024-01-01 00:00:00, 2024-01-01 01:00:00, 2024-01-01 02:00…
#> $ ts_point_quantity             <dbl> 4, 4, 4, 4, 4, 4, 13, 13, 13, 13, 13, 13, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 5,…
#> $ procurement_price_amount      <dbl> 13.8, 13.8, 13.8, 13.8, 13.8, 13.8, 27.0, 27.0, 27.0, 27.0, 27.0, 27.0, 12.0, 12…
#> $ ts_currency_unit_name         <chr> "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EU…
#> $ ts_price_measure_unit_name    <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MA…
#> $ ts_quantity_measure_unit_name <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MA…
```
