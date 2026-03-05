# Get Elastic Demands (IFs aFRR 3.4 & mFRR 3.4)

Elastic demands for scheduled activation of standard aFRR/mFRR product.

## Usage

``` r
elastic_demands(
  eic = NULL,
  process_type = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
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

## Examples

``` r
df <- entsoeapi::elastic_demands(
  eic = "10YCZ-CEPS-----N",
  process_type = "A47",
  period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-11-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A37&processType=A47&businessType=B75&Acquiring_domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202410312300&securityToken=<...>
#> <- HTTP/2 400 
#> <- date: Thu, 05 Mar 2026 16:12:04 GMT
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
#> → https://web-api.tp.entsoe.eu/api?documentType=A37&processType=A47&businessType=B75&Acquiring_domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202410312300&offset=0&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 05 Mar 2026 16:12:04 GMT
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
#> → https://web-api.tp.entsoe.eu/api?documentType=A37&processType=A47&businessType=B75&Acquiring_domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202410312300&offset=100&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 05 Mar 2026 16:12:04 GMT
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
#> → https://web-api.tp.entsoe.eu/api?documentType=A37&processType=A47&businessType=B75&Acquiring_domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202410312300&offset=200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 05 Mar 2026 16:12:05 GMT
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
#> → https://web-api.tp.entsoe.eu/api?documentType=A37&processType=A47&businessType=B75&Acquiring_domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202410312300&offset=300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 05 Mar 2026 16:12:05 GMT
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
#> → https://web-api.tp.entsoe.eu/api?documentType=A37&processType=A47&businessType=B75&Acquiring_domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202410312300&offset=400&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 05 Mar 2026 16:12:05 GMT
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
#> → https://web-api.tp.entsoe.eu/api?documentType=A37&processType=A47&businessType=B75&Acquiring_domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202410312300&offset=500&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 05 Mar 2026 16:12:05 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Elastic Demands [IF mFRR 3.4, aFRR 3.5]_202406270400-202410220545.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ No additional definitions added!
#> processing xml list ■■■■■■■■■■■                       33% | ETA:  2s
#> ℹ No additional definitions added!
#> processing xml list ■■■■■■■■■■■                       33% | ETA:  2s
#> ℹ No additional definitions added!
#> processing xml list ■■■■■■■■■■■                       33% | ETA:  2s
#> ℹ No additional definitions added!
#> processing xml list ■■■■■■■■■■■                       33% | ETA:  2s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        83% | ETA:  1s
#> ℹ No additional definitions added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        83% | ETA:  1s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ℹ No additional definitions added!

str(df)
#> tibble [585 × 27] (S3: tbl_df/tbl/data.frame)
#>  $ domain_mrid                                    : chr [1:585] "10Y1001C--00085O" "10Y1001C--00085O" "10Y1001C--00085O" "10Y1001C--00085O" ...
#>  $ domain_name                                    : chr [1:585] "mFRR region" "mFRR region" "mFRR region" "mFRR region" ...
#>  $ bid_ts_connecting_domain_mrid                  : chr [1:585] "10Y1001C--00085O" "10Y1001C--00085O" "10Y1001C--00085O" "10Y1001C--00085O" ...
#>  $ bid_ts_connecting_domain_name                  : chr [1:585] "mFRR region" "mFRR region" "mFRR region" "mFRR region" ...
#>  $ bid_ts_acquiring_domain_mrid                   : chr [1:585] "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" ...
#>  $ bid_ts_acquiring_domain_name                   : chr [1:585] "Czech Republic" "Czech Republic" "Czech Republic" "Czech Republic" ...
#>  $ bid_ts_mrid                                    : chr [1:585] "TS_NEED_CEPS_UP" "TS_NEED_CEPS_UP" "TS_NEED_CEPS_UP" "TS_NEED_CEPS_UP" ...
#>  $ bid_ts_auction_mrid                            : chr [1:585] "AUCTION-mFRR" "AUCTION-mFRR" "AUCTION-mFRR" "AUCTION-mFRR" ...
#>  $ subject_market_participant_market_role_type    : chr [1:585] "A35" "A35" "A35" "A35" ...
#>  $ subject_market_participant_market_role_type_def: chr [1:585] "MOL Responsible" "MOL Responsible" "MOL Responsible" "MOL Responsible" ...
#>  $ type                                           : chr [1:585] "A37" "A37" "A37" "A37" ...
#>  $ type_def                                       : chr [1:585] "Reserve tender document" "Reserve tender document" "Reserve tender document" "Reserve tender document" ...
#>  $ process_type                                   : chr [1:585] "A47" "A47" "A47" "A47" ...
#>  $ process_type_def                               : chr [1:585] "Manual frequency restoration reserve" "Manual frequency restoration reserve" "Manual frequency restoration reserve" "Manual frequency restoration reserve" ...
#>  $ bid_ts_flow_direction                          : chr [1:585] "A01" "A01" "A01" "A01" ...
#>  $ bid_ts_flow_direction_def                      : chr [1:585] "UP" "UP" "UP" "UP" ...
#>  $ created_date_time                              : POSIXct[1:585], format: "2026-03-05 16:12:04" "2026-03-05 16:12:04" "2026-03-05 16:12:04" "2026-03-05 16:12:04" ...
#>  $ revision_number                                : num [1:585] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ reserve_bid_period_time_interval_start         : POSIXct[1:585], format: "2024-01-09 09:30:00" "2024-01-09 09:30:00" "2024-01-09 09:30:00" "2024-01-09 09:30:00" ...
#>  $ reserve_bid_period_time_interval_end           : POSIXct[1:585], format: "2024-03-14 10:00:00" "2024-03-14 10:00:00" "2024-03-14 10:00:00" "2024-03-14 10:00:00" ...
#>  $ bid_ts_resolution                              : chr [1:585] "PT15M" "PT15M" "PT15M" "PT15M" ...
#>  $ bid_ts_time_interval_start                     : POSIXct[1:585], format: "2024-01-09 09:30:00" "2024-01-10 07:45:00" "2024-01-17 06:30:00" "2024-01-17 06:45:00" ...
#>  $ bid_ts_time_interval_end                       : POSIXct[1:585], format: "2024-01-09 09:45:00" "2024-01-10 08:00:00" "2024-01-17 06:45:00" "2024-01-17 07:00:00" ...
#>  $ bid_ts_point_dt_start                          : POSIXct[1:585], format: "2024-01-09 09:30:00" "2024-01-10 07:45:00" "2024-01-17 06:30:00" "2024-01-17 06:45:00" ...
#>  $ bid_ts_point_quantity                          : num [1:585] 85 100 100 100 110 110 200 200 100 50 ...
#>  $ bid_ts_point_energy_price_amount               : num [1:585] 720 999 1000 1000 1300 1300 1300 1300 1000 450 ...
#>  $ bid_ts_currency_unit_name                      : chr [1:585] "EUR" "EUR" "EUR" "EUR" ...
```
