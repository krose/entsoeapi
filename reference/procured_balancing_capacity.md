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
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A15&processType=A51&area_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&securityToken=<...>
#> <- HTTP/2 400 
#> <- date: Wed, 04 Mar 2026 22:09:51 GMT
#> <- content-type: text/xml
#> <- content-length: 920
#> <- content-disposition: inline; filename="acknowledgement.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ℹ *** The request has been rephrased. ***
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A15&processType=A51&area_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&offset=0&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:09:51 GMT
#> <- content-type: application/zip
#> <- content-length: 2588
#> <- content-disposition: attachment; filename="Procured Balancing Capacity_202312312300-202401012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/Rtmpb5sMb4/001-PROCURED_BALANCING_CAPACITY_R3_202312312300-202401012300.xml has been read in
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A15&processType=A51&area_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&offset=100&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:09:52 GMT
#> <- content-type: application/zip
#> <- content-length: 2501
#> <- content-disposition: attachment; filename="Procured Balancing Capacity_202312312300-202401012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/Rtmpb5sMb4/001-PROCURED_BALANCING_CAPACITY_R3_202401010500-202401011800.xml has been read in
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A15&processType=A51&area_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&offset=200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:09:52 GMT
#> <- content-type: application/zip
#> <- content-length: 2493
#> <- content-disposition: attachment; filename="Procured Balancing Capacity_202312312300-202401012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/Rtmpb5sMb4/001-PROCURED_BALANCING_CAPACITY_R3_202401011400-202401011800.xml has been read in
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A15&processType=A51&area_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&offset=300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:09:52 GMT
#> <- content-type: application/zip
#> <- content-length: 2009
#> <- content-disposition: attachment; filename="Procured Balancing Capacity_202312312300-202401012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/Rtmpb5sMb4/001-PROCURED_BALANCING_CAPACITY_R3_202401011600-202401012300.xml has been read in
#> processing xml list ■■■■■■■■■■■■■■■■                  50% | ETA:  1s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s

str(df)
#> tibble [1,868 × 23] (S3: tbl_df/tbl/data.frame)
#>  $ area_domain_mrid             : chr [1:1868] "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" ...
#>  $ area_domain_name             : chr [1:1868] "Czech Republic" "Czech Republic" "Czech Republic" "Czech Republic" ...
#>  $ type                         : chr [1:1868] "A15" "A15" "A15" "A15" ...
#>  $ type_def                     : chr [1:1868] "Acquiring System Operator Reserve Schedule" "Acquiring System Operator Reserve Schedule" "Acquiring System Operator Reserve Schedule" "Acquiring System Operator Reserve Schedule" ...
#>  $ process_type                 : chr [1:1868] "A51" "A51" "A51" "A51" ...
#>  $ process_type_def             : chr [1:1868] "Automatic frequency restoration reserve" "Automatic frequency restoration reserve" "Automatic frequency restoration reserve" "Automatic frequency restoration reserve" ...
#>  $ ts_flow_direction            : chr [1:1868] "A02" "A02" "A02" "A02" ...
#>  $ ts_flow_direction_def        : chr [1:1868] "DOWN" "DOWN" "DOWN" "DOWN" ...
#>  $ ts_business_type             : chr [1:1868] "B95" "B95" "B95" "B95" ...
#>  $ ts_business_type_def         : chr [1:1868] "Procured capacity" "Procured capacity" "Procured capacity" "Procured capacity" ...
#>  $ ts_mkt_psr_type              : chr [1:1868] "A03" "A03" "A03" "A03" ...
#>  $ ts_mkt_psr_type_def          : chr [1:1868] "Resource Object" "Resource Object" "Resource Object" "Resource Object" ...
#>  $ created_date_time            : POSIXct[1:1868], format: "2026-03-04 22:09:51" "2026-03-04 22:09:51" ...
#>  $ revision_number              : num [1:1868] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution                : chr [1:1868] "PT60M" "PT60M" "PT60M" "PT60M" ...
#>  $ ts_time_interval_start       : POSIXct[1:1868], format: "2023-12-31 23:00:00" "2023-12-31 23:00:00" ...
#>  $ ts_time_interval_end         : POSIXct[1:1868], format: "2024-01-01 05:00:00" "2024-01-01 05:00:00" ...
#>  $ ts_mrid                      : num [1:1868] 1 1 1 1 1 1 2 2 2 2 ...
#>  $ ts_point_dt_start            : POSIXct[1:1868], format: "2023-12-31 23:00:00" "2024-01-01 00:00:00" ...
#>  $ ts_point_quantity            : num [1:1868] 4 4 4 4 4 4 13 13 13 13 ...
#>  $ ts_currency_unit_name        : chr [1:1868] "EUR" "EUR" "EUR" "EUR" ...
#>  $ ts_price_measure_unit_name   : chr [1:1868] "MAW" "MAW" "MAW" "MAW" ...
#>  $ ts_quantity_measure_unit_name: chr [1:1868] "MAW" "MAW" "MAW" "MAW" ...
```
