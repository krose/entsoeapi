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

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with the queried data, or `NULL` if no data is available for the given
parameters.

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
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=B09&processType=A43&in_Domain=10Y1001A1001A91G&out_Domain=10Y1001A1001A91G&periodStart=202412312300&periodEnd=202512312300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 17 Mar 2026 22:16:35 GMT
#> <- content-type: text/xml
#> <- content-length: 972
#> <- content-disposition: inline; filename="acknowledgement.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ No additional type names added!
#> ℹ pulling area_eic_name table from cache
#> ℹ No additional eic names added!

dplyr::glimpse(df1)
#> Rows: 1
#> Columns: 3
#> $ created_date_time <dttm> 2026-03-17 22:16:35
#> $ reason_code       <chr> "999"
#> $ reason_text       <chr> "No matching data found for Data item FLOW_BASED_ALLOCATIONS [11.1.B] (10Y1001A1001A91G, 10…

df2 <- entsoeapi::flow_based_allocations(
  eic = "10YDOM-REGION-1V",
  period_start = lubridate::ymd(x = "2018-12-31", tz = "CET"),
  period_end = lubridate::ymd(x = "2019-01-01", tz = "CET"),
  process_type = "A32",
  archive = TRUE,
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=B09&processType=A32&StorageType=archive&in_Domain=10YDOM-REGION-1V&out_Domain=10YDOM-REGION-1V&periodStart=201812302300&periodEnd=201812312300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 17 Mar 2026 22:16:36 GMT
#> <- content-type: application/zip
#> <- content-disposition: attachment; filename="DayAhead_CWE_20181230T2300Z_20181231T2300Z.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpCEPnwY/DayAhead_CWE_20181230T2300Z_20181231T2300Z.xml has been read in
#> Warning: XML column lengths are not exact multiples of the maximum (9440); recycling with truncation.
#> ℹ pulling area_eic_name table from cache
#> ℹ No additional definitions added!

dplyr::glimpse(df2)
#> Rows: 9,440
#> Columns: 21
#> $ domain_mrid                                                     <chr> "10YDOM-REGION-1V", "10YDOM-REGION-1V", "10YDO…
#> $ domain_name                                                     <chr> "CWE Region", "CWE Region", "CWE Region", "CWE…
#> $ type                                                            <chr> "B11", "B11", "B11", "B11", "B11", "B11", "B11…
#> $ type_def                                                        <chr> "Anonymized flow based parameters publication"…
#> $ process_type                                                    <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01…
#> $ process_type_def                                                <chr> "Day ahead", "Day ahead", "Day ahead", "Day ah…
#> $ ts_business_type                                                <chr> "B39", "B39", "B39", "B39", "B39", "B39", "B39…
#> $ ts_business_type_def                                            <chr> "Flow based domain adjusted to long term sched…
#> $ created_date_time                                               <dttm> 2024-11-25 14:44:31, 2024-11-25 14:44:31, 202…
#> $ revision_number                                                 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ time_period_time_interval_start                                 <dttm> 2018-12-30 23:00:00, 2018-12-30 23:00:00, 201…
#> $ time_period_time_interval_end                                   <dttm> 2018-12-31 23:00:00, 2018-12-31 23:00:00, 201…
#> $ ts_resolution                                                   <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "…
#> $ ts_time_interval_start                                          <dttm> 2018-12-30 23:00:00, 2018-12-30 23:00:00, 201…
#> $ ts_time_interval_end                                            <dttm> 2018-12-31 23:00:00, 2018-12-31 23:00:00, 201…
#> $ ts_mrid                                                         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ ts_point_dt_start                                               <dttm> 2018-12-30 23:00:00, 2018-12-30 23:00:00, 201…
#> $ constraint_ts_monitored_ptdf_domain_mrid                        <chr> "10YAT-APG------L", "10YNL----------L", "10YFR…
#> $ constraint_ts_monitored_ptdf_domain_name                        <chr> "Austria", "Netherlands", "France", "Germany_L…
#> $ constraint_ts_monitored_ptdf_domain_quantity                    <dbl> 0.12228, 0.21714, 0.10229, 0.01671, -0.10559, …
#> $ constraint_ts_monitored_flow_based_study_domain_margin_quantity <dbl> 888, 420, 360, 541, 890, 1446, 919, 816, 602, …
```
