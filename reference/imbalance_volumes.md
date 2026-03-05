# Get Imbalance Volumes (17.1.H)

Total imbalance volumes of the control area. One year range limit
applies.

## Usage

``` r
imbalance_volumes(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the control area domain

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
df <- entsoeapi::imbalance_volumes(
  eic = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A86&controlArea_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 05 Mar 2026 16:12:52 GMT
#> <- content-type: application/zip
#> <- content-length: 1097
#> <- content-disposition: attachment; filename="Total_Imbalance_Volumes_r3_202312312300-202401012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpksiKAO/001-TOTAL_IMBALANCE_VOLUMES_R3_202312312300-202401012300.xml has been read in

str(df)
#> tibble [24 × 21] (S3: tbl_df/tbl/data.frame)
#>  $ control_area_domain_mrid     : chr [1:24] "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" ...
#>  $ control_area_domain_name     : chr [1:24] "Czech Republic" "Czech Republic" "Czech Republic" "Czech Republic" ...
#>  $ doc_status_value             : chr [1:24] "A02" "A02" "A02" "A02" ...
#>  $ doc_status                   : chr [1:24] "Allocated capacity schedule" "Allocated capacity schedule" "Allocated capacity schedule" "Allocated capacity schedule" ...
#>  $ type                         : chr [1:24] "A86" "A86" "A86" "A86" ...
#>  $ type_def                     : chr [1:24] "Imbalance volume" "Imbalance volume" "Imbalance volume" "Imbalance volume" ...
#>  $ process_type                 : chr [1:24] "A16" "A16" "A16" "A16" ...
#>  $ process_type_def             : chr [1:24] "Realised" "Realised" "Realised" "Realised" ...
#>  $ ts_flow_direction            : chr [1:24] "A01" "A01" "A01" "A01" ...
#>  $ ts_flow_direction_def        : chr [1:24] "UP" "UP" "UP" "UP" ...
#>  $ ts_business_type             : chr [1:24] "A19" "A19" "A19" "A19" ...
#>  $ ts_business_type_def         : chr [1:24] "Balance energy deviation" "Balance energy deviation" "Balance energy deviation" "Balance energy deviation" ...
#>  $ created_date_time            : POSIXct[1:24], format: "2026-03-05 16:12:52" "2026-03-05 16:12:52" "2026-03-05 16:12:52" "2026-03-05 16:12:52" ...
#>  $ revision_number              : num [1:24] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution                : chr [1:24] "PT60M" "PT60M" "PT60M" "PT60M" ...
#>  $ ts_time_interval_start       : POSIXct[1:24], format: "2023-12-31 23:00:00" "2023-12-31 23:00:00" "2023-12-31 23:00:00" "2023-12-31 23:00:00" ...
#>  $ ts_time_interval_end         : POSIXct[1:24], format: "2024-01-01 11:00:00" "2024-01-01 11:00:00" "2024-01-01 11:00:00" "2024-01-01 11:00:00" ...
#>  $ ts_mrid                      : num [1:24] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start            : POSIXct[1:24], format: "2023-12-31 23:00:00" "2024-01-01 00:00:00" "2024-01-01 01:00:00" "2024-01-01 02:00:00" ...
#>  $ ts_point_quantity            : num [1:24] 206 203 199 196 198 ...
#>  $ ts_quantity_measure_unit_name: chr [1:24] "MWH" "MWH" "MWH" "MWH" ...
```
