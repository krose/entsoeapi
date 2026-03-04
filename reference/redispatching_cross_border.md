# title Get Redispatching Cross Border (13.1.A)

Changes in production and load (increase or decrease) to relieve
congested internal lines that exceeds its capacity. 100 documents limit
applies!!

## Usage

``` r
redispatching_cross_border(
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

  Energy Identification Code of the control area

- eic_out:

  Energy Identification Code of the control area

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format

- tidy_output:

  Defaults to TRUE. If TRUE, then flatten nested tables.

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
# Germany's cross-border redispatching between TenneT and 50Hertz TSO.
df <- entsoeapi::redispatching_cross_border(
  eic_in = "10YDE-EON------1",
  eic_out = "10YDE-VE-------2",
  period_start = lubridate::ymd(x = "2024-09-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-10-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A63&businessType=A46&in_Domain=10YDE-EON------1&out_Domain=10YDE-VE-------2&periodStart=202408312200&periodEnd=202409302200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:14:27 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Redispatching_Cross_Border_202409010900-202409291445.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived

str(df)
#> tibble [524 × 25] (S3: tbl_df/tbl/data.frame)
#>  $ ts_in_domain_mrid            : chr [1:524] "10YDE-EON------1" "10YDE-EON------1" "10YDE-EON------1" "10YDE-EON------1" ...
#>  $ ts_in_domain_name            : chr [1:524] "Germany TenneT DE" "Germany TenneT DE" "Germany TenneT DE" "Germany TenneT DE" ...
#>  $ ts_out_domain_mrid           : chr [1:524] "10YDE-VE-------2" "10YDE-VE-------2" "10YDE-VE-------2" "10YDE-VE-------2" ...
#>  $ ts_out_domain_name           : chr [1:524] "Germany 50Hertz" "Germany 50Hertz" "Germany 50Hertz" "Germany 50Hertz" ...
#>  $ type                         : chr [1:524] "A63" "A63" "A63" "A63" ...
#>  $ type_def                     : chr [1:524] "Redispatch notice" "Redispatch notice" "Redispatch notice" "Redispatch notice" ...
#>  $ process_type                 : chr [1:524] "A16" "A16" "A16" "A16" ...
#>  $ process_type_def             : chr [1:524] "Realised" "Realised" "Realised" "Realised" ...
#>  $ ts_flow_direction            : chr [1:524] "A01" "A01" "A01" "A01" ...
#>  $ ts_flow_direction_def        : chr [1:524] "UP" "UP" "UP" "UP" ...
#>  $ ts_business_type             : chr [1:524] "A46" "A46" "A46" "A46" ...
#>  $ ts_business_type_def         : chr [1:524] "System Operator redispatching" "System Operator redispatching" "System Operator redispatching" "System Operator redispatching" ...
#>  $ ts_mkt_psr_type              : chr [1:524] "A04" "A04" "A04" "A04" ...
#>  $ ts_mkt_psr_type_def          : chr [1:524] "Generation" "Generation" "Generation" "Generation" ...
#>  $ created_date_time            : POSIXct[1:524], format: "2026-03-04 22:14:27" "2026-03-04 22:14:27" ...
#>  $ ts_reason_code               : chr [1:524] "B24" "B24" "B24" "B24" ...
#>  $ ts_reason_text               : chr [1:524] "Load flow overload" "Load flow overload" "Load flow overload" "Load flow overload" ...
#>  $ revision_number              : num [1:524] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution                : chr [1:524] "PT15M" "PT15M" "PT15M" "PT15M" ...
#>  $ ts_time_interval_start       : POSIXct[1:524], format: "2024-09-01 09:00:00" "2024-09-01 09:00:00" ...
#>  $ ts_time_interval_end         : POSIXct[1:524], format: "2024-09-01 15:00:00" "2024-09-01 15:00:00" ...
#>  $ ts_mrid                      : num [1:524] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start            : POSIXct[1:524], format: "2024-09-01 09:00:00" "2024-09-01 09:15:00" ...
#>  $ ts_point_quantity            : num [1:524] 25 25 25 25 25 70 70 70 70 70 ...
#>  $ ts_quantity_measure_unit_name: chr [1:524] "MWH" "MWH" "MWH" "MWH" ...
```
