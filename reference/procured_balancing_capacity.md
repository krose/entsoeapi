# Get Procured Balancing Capacity (GL EB 12.3.F)

Procured balancing capacity by TSOs. 100 documents limit applies; paging
is handled transparently.

## Usage

``` r
procured_balancing_capacity(
  eic = NULL,
  process_type = NULL,
  type_market_agreement = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the area domain

- process_type:

  type of frequency restoration reserve "A51" aFRR "A52" FCR "A47" mFRR

- type_market_agreement:

  Optional market agreement type code

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format

- tidy_output:

  Defaults to TRUE. flatten nested tables

- security_token:

  Security token for ENTSO-E transparency platform

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
#> <- date: Mon, 09 Mar 2026 20:05:48 GMT
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
#> <- date: Mon, 09 Mar 2026 20:05:49 GMT
#> <- content-type: application/zip
#> <- content-length: 2592
#> <- content-disposition: attachment; filename="Procured Balancing Capacity_202312312300-202401012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/Rtmp1NLnYK/001-PROCURED_BALANCING_CAPACITY_R3_202312312300-202401012300.xml has been read in
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A15&processType=A51&area_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&offset=100&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 09 Mar 2026 20:05:49 GMT
#> <- content-type: application/zip
#> <- content-length: 2500
#> <- content-disposition: attachment; filename="Procured Balancing Capacity_202312312300-202401012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/Rtmp1NLnYK/001-PROCURED_BALANCING_CAPACITY_R3_202401010500-202401011800.xml has been read in
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A15&processType=A51&area_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&offset=200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 09 Mar 2026 20:05:50 GMT
#> <- content-type: application/zip
#> <- content-length: 2491
#> <- content-disposition: attachment; filename="Procured Balancing Capacity_202312312300-202401012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/Rtmp1NLnYK/001-PROCURED_BALANCING_CAPACITY_R3_202401011400-202401011800.xml has been read in
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A15&processType=A51&area_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&offset=300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 09 Mar 2026 20:05:50 GMT
#> <- content-type: application/zip
#> <- content-length: 2009
#> <- content-disposition: attachment; filename="Procured Balancing Capacity_202312312300-202401012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/Rtmp1NLnYK/001-PROCURED_BALANCING_CAPACITY_R3_202401011600-202401012300.xml has been read in
#> processing xml list ■■■■■■■■■■■■■■■■                  50% | ETA:  1s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■           75% | ETA:  1s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s

dplyr::glimpse(df)
#> Rows: 1,868
#> Columns: 23
#> $ area_domain_mrid              <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", …
#> $ area_domain_name              <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", "Czech R…
#> $ type                          <chr> "A15", "A15", "A15", "A15", "A15", "A15", "A15", "A15", "A15", "A15", "A15", "A1…
#> $ type_def                      <chr> "Acquiring System Operator Reserve Schedule", "Acquiring System Operator Reserve…
#> $ process_type                  <chr> "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A5…
#> $ process_type_def              <chr> "Automatic frequency restoration reserve", "Automatic frequency restoration rese…
#> $ ts_flow_direction             <chr> "A02", "A02", "A02", "A02", "A02", "A02", "A01", "A01", "A01", "A01", "A01", "A0…
#> $ ts_flow_direction_def         <chr> "DOWN", "DOWN", "DOWN", "DOWN", "DOWN", "DOWN", "UP", "UP", "UP", "UP", "UP", "U…
#> $ ts_business_type              <chr> "B95", "B95", "B95", "B95", "B95", "B95", "B95", "B95", "B95", "B95", "B95", "B9…
#> $ ts_business_type_def          <chr> "Procured capacity", "Procured capacity", "Procured capacity", "Procured capacit…
#> $ ts_mkt_psr_type               <chr> "A03", "A03", "A03", "A03", "A03", "A03", "A03", "A03", "A03", "A03", "A03", "A0…
#> $ ts_mkt_psr_type_def           <chr> "Resource Object", "Resource Object", "Resource Object", "Resource Object", "Res…
#> $ created_date_time             <dttm> 2026-03-09 20:05:49, 2026-03-09 20:05:49, 2026-03-09 20:05:49, 2026-03-09 20:05…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution                 <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M",…
#> $ ts_time_interval_start        <dttm> 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00…
#> $ ts_time_interval_end          <dttm> 2024-01-01 05:00:00, 2024-01-01 05:00:00, 2024-01-01 05:00:00, 2024-01-01 05:00…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5,…
#> $ ts_point_dt_start             <dttm> 2023-12-31 23:00:00, 2024-01-01 00:00:00, 2024-01-01 01:00:00, 2024-01-01 02:00…
#> $ ts_point_quantity             <dbl> 4, 4, 4, 4, 4, 4, 13, 13, 13, 13, 13, 13, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 5,…
#> $ ts_currency_unit_name         <chr> "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EU…
#> $ ts_price_measure_unit_name    <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MA…
#> $ ts_quantity_measure_unit_name <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MA…
```
