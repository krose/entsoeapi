# Get Aggregated Balancing Energy Bids (GL EB 12.3.E)

Aggregated balancing energy bids. One year range limit applies.

## Usage

``` r
aggregated_balancing_energy_bids(
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

  Energy Identification Code of the area domain

- process_type:

  type of frequency restoration reserve "A51" aFRR "A46" RR "A47" mFRR
  "A60" mFRR with scheduled activation "A61" mFRR with direct activation

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
df <- entsoeapi::aggregated_balancing_energy_bids(
  eic = "10YCZ-CEPS-----N",
  process_type = "A51",
  period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A24&processType=A51&area_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:12:25 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="AGGREGATED_BALANCING_ENERGY_BIDS_R3_202312312300-202401012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived

str(df)
#> tibble [192 × 19] (S3: tbl_df/tbl/data.frame)
#>  $ area_domain_mrid             : chr [1:192] "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" ...
#>  $ area_domain_name             : chr [1:192] "Czech Republic" "Czech Republic" "Czech Republic" "Czech Republic" ...
#>  $ type                         : chr [1:192] "A24" "A24" "A24" "A24" ...
#>  $ type_def                     : chr [1:192] "Bid document" "Bid document" "Bid document" "Bid document" ...
#>  $ process_type                 : chr [1:192] "A51" "A51" "A51" "A51" ...
#>  $ process_type_def             : chr [1:192] "Automatic frequency restoration reserve" "Automatic frequency restoration reserve" "Automatic frequency restoration reserve" "Automatic frequency restoration reserve" ...
#>  $ ts_flow_direction            : chr [1:192] "A02" "A02" "A02" "A02" ...
#>  $ ts_flow_direction_def        : chr [1:192] "DOWN" "DOWN" "DOWN" "DOWN" ...
#>  $ ts_business_type             : chr [1:192] "A14" "A14" "A14" "A14" ...
#>  $ ts_business_type_def         : chr [1:192] "Aggregated energy data" "Aggregated energy data" "Aggregated energy data" "Aggregated energy data" ...
#>  $ created_date_time            : POSIXct[1:192], format: "2026-03-04 22:12:25" "2026-03-04 22:12:25" ...
#>  $ revision_number              : num [1:192] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution                : chr [1:192] "PT15M" "PT15M" "PT15M" "PT15M" ...
#>  $ ts_time_interval_start       : POSIXct[1:192], format: "2023-12-31 23:00:00" "2023-12-31 23:00:00" ...
#>  $ ts_time_interval_end         : POSIXct[1:192], format: "2024-01-01 23:00:00" "2024-01-01 23:00:00" ...
#>  $ ts_mrid                      : num [1:192] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start            : POSIXct[1:192], format: "2023-12-31 23:00:00" "2023-12-31 23:15:00" ...
#>  $ ts_point_quantity            : num [1:192] 184 184 184 184 184 184 184 184 184 184 ...
#>  $ ts_quantity_measure_unit_name: chr [1:192] "MAW" "MAW" "MAW" "MAW" ...
```
