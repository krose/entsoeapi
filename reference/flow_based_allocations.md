# Get Flow Based Allocations (11.1.B)

Flow based capacity allocated, for all time horizons.

## Usage

``` r
flow_based_allocations(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  process_type = "A43",
  archive = FALSE,
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the area

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- process_type:

  Contract market agreement type, valid values can be checked from
  process_types table; "A32" = Month-ahead "A33" = Year-ahead "A43" =
  Day ahead "A44" = Intraday Defaults to "A43" (Day ahead)

- archive:

  Defaults to FALSE, set to TRUE if archives to be queried.

- tidy_output:

  Defaults to TRUE. If TRUE, then flatten nested tables.

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df1 <- entsoeapi::flow_based_allocations(
  eic = "10Y1001A1001A91G",
  period_start = lubridate::ymd(x = "2025-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2026-01-01", tz = "CET"),
  process_type = "A43",
  archive = FALSE,
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=B09&processType=A43&in_Domain=10Y1001A1001A91G&out_Domain=10Y1001A1001A91G&periodStart=202412312300&periodEnd=202512312300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:08:38 GMT
#> <- content-type: text/xml
#> <- content-length: 972
#> <- content-disposition: inline; filename="acknowledgement.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ No additional type names added!
#> ℹ No additional eic names added!

str(df1)
#> tibble [1 × 3] (S3: tbl_df/tbl/data.frame)
#>  $ created_date_time: POSIXct[1:1], format: "2026-03-04 22:08:38"
#>  $ reason_code      : chr "999"
#>  $ reason_text      : chr "No matching data found for Data item FLOW_BASED_ALLOCATIONS [11.1.B] (10Y1001A1001A91G, 10Y1001A1001A91G) and i"| __truncated__

df2 <- entsoeapi::flow_based_allocations(
  eic = "10YDOM-REGION-1V",
  period_start = lubridate::ymd(x = "2018-12-31", tz = "CET"),
  period_end = lubridate::ymd(x = "2019-01-01", tz = "CET"),
  process_type = "A32",
  archive = TRUE,
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=B09&processType=A32&StorageType=archive&in_Domain=10YDOM-REGION-1V&out_Domain=10YDOM-REGION-1V&periodStart=201812302300&periodEnd=201812312300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:08:39 GMT
#> <- content-type: application/zip
#> <- content-disposition: attachment; filename="DayAhead_CWE_20181230T2300Z_20181231T2300Z.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/Rtmpb5sMb4/DayAhead_CWE_20181230T2300Z_20181231T2300Z.xml has been read in
#> Warning: Item 4 has 24 rows but longest item has 9440; recycled with remainder.
#> ℹ No additional definitions added!

str(df2)
#> tibble [9,440 × 21] (S3: tbl_df/tbl/data.frame)
#>  $ domain_mrid                                                    : chr [1:9440] "10YDOM-REGION-1V" "10YDOM-REGION-1V" "10YDOM-REGION-1V" "10YDOM-REGION-1V" ...
#>  $ domain_name                                                    : chr [1:9440] "CWE Region" "CWE Region" "CWE Region" "CWE Region" ...
#>  $ type                                                           : chr [1:9440] "B11" "B11" "B11" "B11" ...
#>  $ type_def                                                       : chr [1:9440] "Anonymized flow based parameters publication" "Anonymized flow based parameters publication" "Anonymized flow based parameters publication" "Anonymized flow based parameters publication" ...
#>  $ process_type                                                   : chr [1:9440] "A01" "A01" "A01" "A01" ...
#>  $ process_type_def                                               : chr [1:9440] "Day ahead" "Day ahead" "Day ahead" "Day ahead" ...
#>  $ ts_business_type                                               : chr [1:9440] "B39" "B39" "B39" "B39" ...
#>  $ ts_business_type_def                                           : chr [1:9440] "Flow based domain adjusted to long term schedules" "Flow based domain adjusted to long term schedules" "Flow based domain adjusted to long term schedules" "Flow based domain adjusted to long term schedules" ...
#>  $ created_date_time                                              : POSIXct[1:9440], format: "2024-11-25 14:44:31" "2024-11-25 14:44:31" ...
#>  $ revision_number                                                : num [1:9440] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ time_period_time_interval_start                                : POSIXct[1:9440], format: "2018-12-30 23:00:00" "2018-12-30 23:00:00" ...
#>  $ time_period_time_interval_end                                  : POSIXct[1:9440], format: "2018-12-31 23:00:00" "2018-12-31 23:00:00" ...
#>  $ ts_resolution                                                  : chr [1:9440] "PT60M" "PT60M" "PT60M" "PT60M" ...
#>  $ ts_time_interval_start                                         : POSIXct[1:9440], format: "2018-12-30 23:00:00" "2018-12-30 23:00:00" ...
#>  $ ts_time_interval_end                                           : POSIXct[1:9440], format: "2018-12-31 23:00:00" "2018-12-31 23:00:00" ...
#>  $ ts_mrid                                                        : num [1:9440] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start                                              : POSIXct[1:9440], format: "2018-12-30 23:00:00" "2018-12-30 23:00:00" ...
#>  $ constraint_ts_monitored_ptdf_domain_mrid                       : chr [1:9440] "10Y1001A1001A82H" "10Y1001A1001A82H" "10Y1001A1001A82H" "10Y1001A1001A82H" ...
#>  $ constraint_ts_monitored_ptdf_domain_name                       : chr [1:9440] "Germany_Luxemburg" "Germany_Luxemburg" "Germany_Luxemburg" "Germany_Luxemburg" ...
#>  $ constraint_ts_monitored_ptdf_domain_quantity                   : num [1:9440] 0.01671 -0.07213 0 0.00551 -0.01502 ...
#>  $ constraint_ts_monitored_flow_based_study_domain_margin_quantity: num [1:9440] 541 602 590 877 889 ...
```
