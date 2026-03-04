# Get Current Balancing State (GL EB 12.3.A)

Current balancing state of the control area. 100 day range limit
applies.

## Usage

``` r
current_balancing_state(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the area domain

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format 100 day range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format 100 day range limit applies

- tidy_output:

  Defaults to TRUE. flatten nested tables

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df <- entsoeapi::current_balancing_state(
  eic = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A86&businessType=B33&Area_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:12:48 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Current_balancing_state_202312312300-202401012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived

str(df)
#> tibble [1,440 × 19] (S3: tbl_df/tbl/data.frame)
#>  $ area_domain_mrid             : chr [1:1440] "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" ...
#>  $ area_domain_name             : chr [1:1440] "Czech Republic" "Czech Republic" "Czech Republic" "Czech Republic" ...
#>  $ type                         : chr [1:1440] "A86" "A86" "A86" "A86" ...
#>  $ type_def                     : chr [1:1440] "Imbalance volume" "Imbalance volume" "Imbalance volume" "Imbalance volume" ...
#>  $ process_type                 : chr [1:1440] "A16" "A16" "A16" "A16" ...
#>  $ process_type_def             : chr [1:1440] "Realised" "Realised" "Realised" "Realised" ...
#>  $ ts_flow_direction            : chr [1:1440] "A01" "A01" "A01" "A01" ...
#>  $ ts_flow_direction_def        : chr [1:1440] "UP" "UP" "UP" "UP" ...
#>  $ ts_business_type             : chr [1:1440] "B33" "B33" "B33" "B33" ...
#>  $ ts_business_type_def         : chr [1:1440] "Area Control error (ACE)" "Area Control error (ACE)" "Area Control error (ACE)" "Area Control error (ACE)" ...
#>  $ created_date_time            : POSIXct[1:1440], format: "2026-03-04 22:12:48" "2026-03-04 22:12:48" ...
#>  $ revision_number              : num [1:1440] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution                : chr [1:1440] "PT1M" "PT1M" "PT1M" "PT1M" ...
#>  $ ts_time_interval_start       : POSIXct[1:1440], format: "2023-12-31 23:00:00" "2023-12-31 23:00:00" ...
#>  $ ts_time_interval_end         : POSIXct[1:1440], format: "2023-12-31 23:16:00" "2023-12-31 23:16:00" ...
#>  $ ts_mrid                      : num [1:1440] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start            : POSIXct[1:1440], format: "2023-12-31 23:00:00" "2023-12-31 23:01:00" ...
#>  $ ts_point_quantity            : num [1:1440] 44.6 80 76.1 75.2 51.5 ...
#>  $ ts_quantity_measure_unit_name: chr [1:1440] "MAW" "MAW" "MAW" "MAW" ...
```
