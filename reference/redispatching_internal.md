# Get Redispatching Internal (13.1.A)

Changes in production and load (increase or decrease) to relieve
internal congestion lines that exceeds its capacity. 100 documents limit
applies!!

## Usage

``` r
redispatching_internal(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

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
# Netherlands' internal redispatching.
df <- entsoeapi::redispatching_internal(
  eic = "10YNL----------L",
  period_start = lubridate::ymd(x = "2023-11-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2023-12-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A63&businessType=A85&in_Domain=10YNL----------L&out_Domain=10YNL----------L&periodStart=202310312300&periodEnd=202311302300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:14:28 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Redispatching_Internal_202310312300-202311052300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived

str(df)
#> tibble [384 × 28] (S3: tbl_df/tbl/data.frame)
#>  $ ts_in_domain_mrid            : chr [1:384] "10YNL----------L" "10YNL----------L" "10YNL----------L" "10YNL----------L" ...
#>  $ ts_in_domain_name            : chr [1:384] "Netherlands" "Netherlands" "Netherlands" "Netherlands" ...
#>  $ ts_out_domain_mrid           : chr [1:384] "10YNL----------L" "10YNL----------L" "10YNL----------L" "10YNL----------L" ...
#>  $ ts_out_domain_name           : chr [1:384] "Netherlands" "Netherlands" "Netherlands" "Netherlands" ...
#>  $ ts_asset_location_name       : chr [1:384] "Hardenberg - Ommen Dante wit 110 kV" "Hardenberg - Ommen Dante wit 110 kV" "Hardenberg - Ommen Dante wit 110 kV" "Hardenberg - Ommen Dante wit 110 kV" ...
#>  $ ts_asset_mrid                : chr [1:384] "49T000000000436O" "49T000000000436O" "49T000000000436O" "49T000000000436O" ...
#>  $ type                         : chr [1:384] "A63" "A63" "A63" "A63" ...
#>  $ type_def                     : chr [1:384] "Redispatch notice" "Redispatch notice" "Redispatch notice" "Redispatch notice" ...
#>  $ process_type                 : chr [1:384] "A16" "A16" "A16" "A16" ...
#>  $ process_type_def             : chr [1:384] "Realised" "Realised" "Realised" "Realised" ...
#>  $ ts_flow_direction            : chr [1:384] "A01" "A01" "A01" "A01" ...
#>  $ ts_flow_direction_def        : chr [1:384] "UP" "UP" "UP" "UP" ...
#>  $ ts_business_type             : chr [1:384] "A85" "A85" "A85" "A85" ...
#>  $ ts_business_type_def         : chr [1:384] "Internal redispatch" "Internal redispatch" "Internal redispatch" "Internal redispatch" ...
#>  $ ts_mkt_psr_type              : chr [1:384] "A05" "A05" "A05" "A05" ...
#>  $ ts_mkt_psr_type_def          : chr [1:384] "Load" "Load" "Load" "Load" ...
#>  $ ts_psr_type                  : chr [1:384] "B21" "B21" "B21" "B21" ...
#>  $ created_date_time            : POSIXct[1:384], format: "2026-03-04 22:14:28" "2026-03-04 22:14:28" ...
#>  $ ts_reason_code               : chr [1:384] "B24" "B24" "B24" "B24" ...
#>  $ ts_reason_text               : chr [1:384] "Load flow overload" "Load flow overload" "Load flow overload" "Load flow overload" ...
#>  $ revision_number              : num [1:384] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution                : chr [1:384] "PT15M" "PT15M" "PT15M" "PT15M" ...
#>  $ ts_time_interval_start       : POSIXct[1:384], format: "2023-10-31 23:00:00" "2023-10-31 23:00:00" ...
#>  $ ts_time_interval_end         : POSIXct[1:384], format: "2023-11-01 23:00:00" "2023-11-01 23:00:00" ...
#>  $ ts_mrid                      : num [1:384] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start            : POSIXct[1:384], format: "2023-10-31 23:00:00" "2023-10-31 23:15:00" ...
#>  $ ts_point_quantity            : num [1:384] 25 25 25 25 25 25 25 25 25 25 ...
#>  $ ts_quantity_measure_unit_name: chr [1:384] "MWH" "MWH" "MWH" "MWH" ...
```
