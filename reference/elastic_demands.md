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

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with the queried data, or `NULL` if no data is available for the given
parameters.

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
#> <- date: Thu, 12 Mar 2026 10:21:35 GMT
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
#> <- date: Thu, 12 Mar 2026 10:21:35 GMT
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
#> <- date: Thu, 12 Mar 2026 10:21:36 GMT
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
#> <- date: Thu, 12 Mar 2026 10:21:36 GMT
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
#> <- date: Thu, 12 Mar 2026 10:21:36 GMT
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
#> <- date: Thu, 12 Mar 2026 10:21:36 GMT
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
#> <- date: Thu, 12 Mar 2026 10:21:37 GMT
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
#> processing xml list ■■■■■■■■■■■■■■■■                  50% | ETA:  2s
#> ℹ No additional definitions added!
#> processing xml list ■■■■■■■■■■■■■■■■                  50% | ETA:  2s
#> ℹ No additional definitions added!
#> processing xml list ■■■■■■■■■■■■■■■■                  50% | ETA:  2s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        83% | ETA:  1s
#> ℹ No additional definitions added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        83% | ETA:  1s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ℹ No additional definitions added!

dplyr::glimpse(df)
#> Rows: 585
#> Columns: 27
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
#> $ bid_ts_flow_direction_def                       <chr> "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "U…
#> $ created_date_time                               <dttm> 2026-03-12 10:21:35, 2026-03-12 10:21:35, 2026-03-12 10:21:35…
#> $ revision_number                                 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ reserve_bid_period_time_interval_start          <dttm> 2024-01-09 09:30:00, 2024-01-09 09:30:00, 2024-01-09 09:30:00…
#> $ reserve_bid_period_time_interval_end            <dttm> 2024-03-14 10:00:00, 2024-03-14 10:00:00, 2024-03-14 10:00:00…
#> $ bid_ts_resolution                               <chr> "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M",…
#> $ bid_ts_time_interval_start                      <dttm> 2024-01-09 09:30:00, 2024-01-10 07:45:00, 2024-01-17 06:30:00…
#> $ bid_ts_time_interval_end                        <dttm> 2024-01-09 09:45:00, 2024-01-10 08:00:00, 2024-01-17 06:45:00…
#> $ bid_ts_point_dt_start                           <dttm> 2024-01-09 09:30:00, 2024-01-10 07:45:00, 2024-01-17 06:30:00…
#> $ bid_ts_point_quantity                           <dbl> 85, 100, 100, 100, 110, 110, 200, 200, 100, 50, 150, 50, 50, 1…
#> $ bid_ts_point_energy_price_amount                <dbl> 720, 999, 1000, 1000, 1300, 1300, 1300, 1300, 1000, 450, 1000,…
#> $ bid_ts_currency_unit_name                       <chr> "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR",…
```
