# Get Total Commercial Schedules (12.1.F)

Aggregated capacity nominated for all time horizons (including
Intra-Day) corresponding to implicit and explicit allocations after each
nomination process.

## Usage

``` r
total_commercial_sched(
  eic_in = NULL,
  eic_out = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic_in:

  Energy Identification Code of in domain

- eic_out:

  Energy Identification Code of out domain

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- tidy_output:

  Defaults to TRUE. If TRUE, then flatten nested tables.

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df1 <- entsoeapi::total_commercial_sched(
  eic_in = "10YCZ-CEPS-----N",
  eic_out = "10YSK-SEPS-----K",
  period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2019-12-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A09&contract_MarketAgreement.Type=A05&in_Domain=10YCZ-CEPS-----N&out_Domain=10YSK-SEPS-----K&periodStart=201910312300&periodEnd=201911302300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:14:32 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Commercial schedules_201910312300-201911302300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ No additional definitions added!
str(df1)
#> tibble [720 × 19] (S3: tbl_df/tbl/data.frame)
#>  $ ts_in_domain_mrid                    : chr [1:720] "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" ...
#>  $ ts_in_domain_name                    : chr [1:720] "Czech Republic" "Czech Republic" "Czech Republic" "Czech Republic" ...
#>  $ ts_out_domain_mrid                   : chr [1:720] "10YSK-SEPS-----K" "10YSK-SEPS-----K" "10YSK-SEPS-----K" "10YSK-SEPS-----K" ...
#>  $ ts_out_domain_name                   : chr [1:720] "Slovak Republic" "Slovak Republic" "Slovak Republic" "Slovak Republic" ...
#>  $ type                                 : chr [1:720] "A09" "A09" "A09" "A09" ...
#>  $ type_def                             : chr [1:720] "Finalised schedule" "Finalised schedule" "Finalised schedule" "Finalised schedule" ...
#>  $ ts_contract_market_agreement_type    : chr [1:720] "A05" "A05" "A05" "A05" ...
#>  $ ts_contract_market_agreement_type_def: chr [1:720] "Total contract" "Total contract" "Total contract" "Total contract" ...
#>  $ ts_business_type                     : chr [1:720] "A06" "A06" "A06" "A06" ...
#>  $ ts_business_type_def                 : chr [1:720] "External trade without explicit capacity" "External trade without explicit capacity" "External trade without explicit capacity" "External trade without explicit capacity" ...
#>  $ created_date_time                    : POSIXct[1:720], format: "2026-03-04 22:14:32" "2026-03-04 22:14:32" ...
#>  $ revision_number                      : num [1:720] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution                        : chr [1:720] "PT60M" "PT60M" "PT60M" "PT60M" ...
#>  $ ts_time_interval_start               : POSIXct[1:720], format: "2019-10-31 23:00:00" "2019-10-31 23:00:00" ...
#>  $ ts_time_interval_end                 : POSIXct[1:720], format: "2019-11-30 23:00:00" "2019-11-30 23:00:00" ...
#>  $ ts_mrid                              : num [1:720] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start                    : POSIXct[1:720], format: "2019-10-31 23:00:00" "2019-11-01 00:00:00" ...
#>  $ ts_point_quantity                    : num [1:720] 21 16 638 713 35 33 34 22 35 55 ...
#>  $ ts_quantity_measure_unit_name        : chr [1:720] "MAW" "MAW" "MAW" "MAW" ...

df2 <- entsoeapi::total_commercial_sched(
  eic_in = "10YDK-1--------W",
  eic_out = "10Y1001A1001A82H",
  period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2019-12-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A09&contract_MarketAgreement.Type=A05&in_Domain=10YDK-1--------W&out_Domain=10Y1001A1001A82H&periodStart=201910312300&periodEnd=201911302300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:14:33 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Commercial schedules_201910312300-201911302300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ No additional definitions added!
str(df2)
#> tibble [720 × 19] (S3: tbl_df/tbl/data.frame)
#>  $ ts_in_domain_mrid                    : chr [1:720] "10YDK-1--------W" "10YDK-1--------W" "10YDK-1--------W" "10YDK-1--------W" ...
#>  $ ts_in_domain_name                    : chr [1:720] "Denmark DK1" "Denmark DK1" "Denmark DK1" "Denmark DK1" ...
#>  $ ts_out_domain_mrid                   : chr [1:720] "10Y1001A1001A82H" "10Y1001A1001A82H" "10Y1001A1001A82H" "10Y1001A1001A82H" ...
#>  $ ts_out_domain_name                   : chr [1:720] "Germany_Luxemburg" "Germany_Luxemburg" "Germany_Luxemburg" "Germany_Luxemburg" ...
#>  $ type                                 : chr [1:720] "A09" "A09" "A09" "A09" ...
#>  $ type_def                             : chr [1:720] "Finalised schedule" "Finalised schedule" "Finalised schedule" "Finalised schedule" ...
#>  $ ts_contract_market_agreement_type    : chr [1:720] "A05" "A05" "A05" "A05" ...
#>  $ ts_contract_market_agreement_type_def: chr [1:720] "Total contract" "Total contract" "Total contract" "Total contract" ...
#>  $ ts_business_type                     : chr [1:720] "A06" "A06" "A06" "A06" ...
#>  $ ts_business_type_def                 : chr [1:720] "External trade without explicit capacity" "External trade without explicit capacity" "External trade without explicit capacity" "External trade without explicit capacity" ...
#>  $ created_date_time                    : POSIXct[1:720], format: "2026-03-04 22:14:33" "2026-03-04 22:14:33" ...
#>  $ revision_number                      : num [1:720] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution                        : chr [1:720] "PT60M" "PT60M" "PT60M" "PT60M" ...
#>  $ ts_time_interval_start               : POSIXct[1:720], format: "2019-10-31 23:00:00" "2019-10-31 23:00:00" ...
#>  $ ts_time_interval_end                 : POSIXct[1:720], format: "2019-11-30 23:00:00" "2019-11-30 23:00:00" ...
#>  $ ts_mrid                              : num [1:720] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start                    : POSIXct[1:720], format: "2019-10-31 23:00:00" "2019-11-01 00:00:00" ...
#>  $ ts_point_quantity                    : num [1:720] 777 1275 1183 1197 1177 ...
#>  $ ts_quantity_measure_unit_name        : chr [1:720] "MAW" "MAW" "MAW" "MAW" ...
```
