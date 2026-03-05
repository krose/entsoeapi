# title Get Countertrading (13.1.B)

Buying or cancelling generation on different side of the border to
relieve congested cross-border lines that exceeds its capacity. The time
interval in the query response depends on duration of matching counter
trades 100 documents limit applies!!

## Usage

``` r
countertrading(
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

  Energy Identification Code of the control area/bidding zone

- eic_out:

  Energy Identification Code of the control area/bidding zone

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
# Counter trading between Germany and Denmark.
df <- entsoeapi::countertrading(
  eic_in = "10Y1001A1001A82H",
  eic_out = "10YDK-1--------W",
  period_start = lubridate::ymd(x = "2024-09-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-10-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A91&in_Domain=10Y1001A1001A82H&out_Domain=10YDK-1--------W&periodStart=202408312200&periodEnd=202409302200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 05 Mar 2026 16:11:55 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Countertrading_202409041000-202409221200.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived

str(df)
#> tibble [20 × 23] (S3: tbl_df/tbl/data.frame)
#>  $ ts_in_domain_mrid            : chr [1:20] "10Y1001A1001A82H" "10Y1001A1001A82H" "10Y1001A1001A82H" "10Y1001A1001A82H" ...
#>  $ ts_in_domain_name            : chr [1:20] "Germany_Luxemburg" "Germany_Luxemburg" "Germany_Luxemburg" "Germany_Luxemburg" ...
#>  $ ts_out_domain_mrid           : chr [1:20] "10YDK-1--------W" "10YDK-1--------W" "10YDK-1--------W" "10YDK-1--------W" ...
#>  $ ts_out_domain_name           : chr [1:20] "Denmark DK1" "Denmark DK1" "Denmark DK1" "Denmark DK1" ...
#>  $ type                         : chr [1:20] "A91" "A91" "A91" "A91" ...
#>  $ type_def                     : chr [1:20] "Counter trade notice" "Counter trade notice" "Counter trade notice" "Counter trade notice" ...
#>  $ process_type                 : chr [1:20] "A16" "A16" "A16" "A16" ...
#>  $ process_type_def             : chr [1:20] "Realised" "Realised" "Realised" "Realised" ...
#>  $ ts_flow_direction            : chr [1:20] "A01" "A01" "A01" "A01" ...
#>  $ ts_flow_direction_def        : chr [1:20] "UP" "UP" "UP" "UP" ...
#>  $ ts_business_type             : chr [1:20] "B03" "B03" "B03" "B03" ...
#>  $ ts_business_type_def         : chr [1:20] "Counter trade" "Counter trade" "Counter trade" "Counter trade" ...
#>  $ created_date_time            : POSIXct[1:20], format: "2026-03-05 16:11:55" "2026-03-05 16:11:55" "2026-03-05 16:11:55" "2026-03-05 16:11:55" ...
#>  $ ts_reason_code               : chr [1:20] "B24" "B24" "B24" "B24" ...
#>  $ ts_reason_text               : chr [1:20] "Load flow overload" "Load flow overload" "Load flow overload" "Load flow overload" ...
#>  $ revision_number              : num [1:20] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution                : chr [1:20] "PT15M" "PT15M" "PT15M" "PT15M" ...
#>  $ ts_time_interval_start       : POSIXct[1:20], format: "2024-09-04 10:00:00" "2024-09-04 10:00:00" "2024-09-04 10:00:00" "2024-09-04 10:00:00" ...
#>  $ ts_time_interval_end         : POSIXct[1:20], format: "2024-09-04 14:00:00" "2024-09-04 14:00:00" "2024-09-04 14:00:00" "2024-09-04 14:00:00" ...
#>  $ ts_mrid                      : num [1:20] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start            : POSIXct[1:20], format: "2024-09-04 10:00:00" "2024-09-04 10:15:00" "2024-09-04 10:30:00" "2024-09-04 10:45:00" ...
#>  $ ts_point_quantity            : num [1:20] 300 300 300 300 300 300 300 300 300 300 ...
#>  $ ts_quantity_measure_unit_name: chr [1:20] "MAW" "MAW" "MAW" "MAW" ...
```
