# Get Cross-Border Physical Flows (12.1.G)

It is the measured real flow of energy between the neighbouring areas on
the cross borders.

## Usage

``` r
cross_border_physical_flows(
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

  POSIXct or YYYY-MM-DD HH:MM:SS format Minimum time interval in query
  response is an MTU period, but 1 year range limit applies.

- tidy_output:

  Defaults to TRUE. If TRUE, then flatten nested tables.

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df1 <- entsoeapi::cross_border_physical_flows(
  eic_in = "10Y1001A1001A83F",
  eic_out = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2020-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2020-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A11&in_Domain=10Y1001A1001A83F&out_Domain=10YCZ-CEPS-----N&periodStart=201912312300&periodEnd=202001012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:08:05 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Physical Flows_201912312300-202001012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ No additional definitions added!

str(df1)
#> tibble [24 × 17] (S3: tbl_df/tbl/data.frame)
#>  $ ts_in_domain_mrid            : chr [1:24] "10Y1001A1001A83F" "10Y1001A1001A83F" "10Y1001A1001A83F" "10Y1001A1001A83F" ...
#>  $ ts_in_domain_name            : chr [1:24] "Germany" "Germany" "Germany" "Germany" ...
#>  $ ts_out_domain_mrid           : chr [1:24] "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" ...
#>  $ ts_out_domain_name           : chr [1:24] "Czech Republic" "Czech Republic" "Czech Republic" "Czech Republic" ...
#>  $ type                         : chr [1:24] "A11" "A11" "A11" "A11" ...
#>  $ type_def                     : chr [1:24] "Aggregated energy data report" "Aggregated energy data report" "Aggregated energy data report" "Aggregated energy data report" ...
#>  $ ts_business_type             : chr [1:24] "A66" "A66" "A66" "A66" ...
#>  $ ts_business_type_def         : chr [1:24] "Energy flow" "Energy flow" "Energy flow" "Energy flow" ...
#>  $ created_date_time            : POSIXct[1:24], format: "2026-03-04 22:08:05" "2026-03-04 22:08:05" ...
#>  $ revision_number              : num [1:24] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution                : chr [1:24] "PT60M" "PT60M" "PT60M" "PT60M" ...
#>  $ ts_time_interval_start       : POSIXct[1:24], format: "2019-12-31 23:00:00" "2019-12-31 23:00:00" ...
#>  $ ts_time_interval_end         : POSIXct[1:24], format: "2020-01-01 23:00:00" "2020-01-01 23:00:00" ...
#>  $ ts_mrid                      : num [1:24] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start            : POSIXct[1:24], format: "2019-12-31 23:00:00" "2020-01-01 00:00:00" ...
#>  $ ts_point_quantity            : num [1:24] 1077 1172 1504 975 715 ...
#>  $ ts_quantity_measure_unit_name: chr [1:24] "MAW" "MAW" "MAW" "MAW" ...

df2 <- entsoeapi::cross_border_physical_flows(
  eic_in = "10YCZ-CEPS-----N",
  eic_out = "10Y1001A1001A83F",
  period_start = lubridate::ymd(x = "2020-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2020-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A11&in_Domain=10YCZ-CEPS-----N&out_Domain=10Y1001A1001A83F&periodStart=201912312300&periodEnd=202001012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:08:05 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Physical Flows_201912312300-202001012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ No additional definitions added!

str(df2)
#> tibble [24 × 17] (S3: tbl_df/tbl/data.frame)
#>  $ ts_in_domain_mrid            : chr [1:24] "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" ...
#>  $ ts_in_domain_name            : chr [1:24] "Czech Republic" "Czech Republic" "Czech Republic" "Czech Republic" ...
#>  $ ts_out_domain_mrid           : chr [1:24] "10Y1001A1001A83F" "10Y1001A1001A83F" "10Y1001A1001A83F" "10Y1001A1001A83F" ...
#>  $ ts_out_domain_name           : chr [1:24] "Germany" "Germany" "Germany" "Germany" ...
#>  $ type                         : chr [1:24] "A11" "A11" "A11" "A11" ...
#>  $ type_def                     : chr [1:24] "Aggregated energy data report" "Aggregated energy data report" "Aggregated energy data report" "Aggregated energy data report" ...
#>  $ ts_business_type             : chr [1:24] "A66" "A66" "A66" "A66" ...
#>  $ ts_business_type_def         : chr [1:24] "Energy flow" "Energy flow" "Energy flow" "Energy flow" ...
#>  $ created_date_time            : POSIXct[1:24], format: "2026-03-04 22:08:05" "2026-03-04 22:08:05" ...
#>  $ revision_number              : num [1:24] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution                : chr [1:24] "PT60M" "PT60M" "PT60M" "PT60M" ...
#>  $ ts_time_interval_start       : POSIXct[1:24], format: "2019-12-31 23:00:00" "2019-12-31 23:00:00" ...
#>  $ ts_time_interval_end         : POSIXct[1:24], format: "2020-01-01 23:00:00" "2020-01-01 23:00:00" ...
#>  $ ts_mrid                      : num [1:24] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start            : POSIXct[1:24], format: "2019-12-31 23:00:00" "2020-01-01 00:00:00" ...
#>  $ ts_point_quantity            : num [1:24] 57.52 0.32 0 0 26.5 ...
#>  $ ts_quantity_measure_unit_name: chr [1:24] "MAW" "MAW" "MAW" "MAW" ...
```
