# Get Netted & Exchanged Volumes Per Border (IFs 3.10, 3.16 & 3.17)

Netted and exchanged balancing energy volumes on a specific border
between an acquiring and a connecting domain.

## Usage

``` r
exchanged_volumes_per_border(
  acquiring_eic = NULL,
  connecting_eic = NULL,
  process_type = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- acquiring_eic:

  Energy Identification Code of the acquiring area

- connecting_eic:

  Energy Identification Code of the connecting area

- process_type:

  type of frequency restoration reserve "A51" aFRR "A60" mFRR with
  scheduled activation "A61" mFRR with direct activation

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format One day range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format One day range limit applies

- tidy_output:

  Defaults to TRUE. flatten nested tables

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df <- entsoeapi::exchanged_volumes_per_border(
  acquiring_eic = "10YBE----------2",
  connecting_eic = "10YFR-RTE------C",
  process_type = "A60",
  period_start = lubridate::ymd(x = "2025-03-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2025-03-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A30&processType=A60&Acquiring_domain=10YBE----------2&Connecting_Domain=10YFR-RTE------C&periodStart=202502282300&periodEnd=202503012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:08:35 GMT
#> <- content-type: application/zip
#> <- content-length: 1179
#> <- content-disposition: attachment; filename="Netted and Exchanged Volumes per Border_202502282300-202503012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/Rtmpb5sMb4/001-NETTED_AND_EXCHANGED_VOLUMES_PER_BORDER_202502282300-202503012300.xml has been read in
#> ℹ No additional definitions added!

str(df)
#> tibble [96 × 21] (S3: tbl_df/tbl/data.frame)
#>  $ area_domain_mrid             : chr [1:96] "10Y1001C--00085O" "10Y1001C--00085O" "10Y1001C--00085O" "10Y1001C--00085O" ...
#>  $ area_domain_name             : chr [1:96] "mFRR region" "mFRR region" "mFRR region" "mFRR region" ...
#>  $ ts_connecting_domain_mrid    : chr [1:96] "10YFR-RTE------C" "10YFR-RTE------C" "10YFR-RTE------C" "10YFR-RTE------C" ...
#>  $ ts_connecting_domain_name    : chr [1:96] "France" "France" "France" "France" ...
#>  $ ts_acquiring_domain_mrid     : chr [1:96] "10YBE----------2" "10YBE----------2" "10YBE----------2" "10YBE----------2" ...
#>  $ ts_acquiring_domain_name     : chr [1:96] "Belgium" "Belgium" "Belgium" "Belgium" ...
#>  $ type                         : chr [1:96] "A30" "A30" "A30" "A30" ...
#>  $ type_def                     : chr [1:96] "Cross border schedule" "Cross border schedule" "Cross border schedule" "Cross border schedule" ...
#>  $ process_type                 : chr [1:96] "A60" "A60" "A60" "A60" ...
#>  $ process_type_def             : chr [1:96] "Scheduled activation mFRR" "Scheduled activation mFRR" "Scheduled activation mFRR" "Scheduled activation mFRR" ...
#>  $ ts_business_type             : chr [1:96] "A45" "A45" "A45" "A45" ...
#>  $ ts_business_type_def         : chr [1:96] "Schedule activated reserves" "Schedule activated reserves" "Schedule activated reserves" "Schedule activated reserves" ...
#>  $ created_date_time            : POSIXct[1:96], format: "2026-03-04 22:08:35" "2026-03-04 22:08:35" ...
#>  $ revision_number              : num [1:96] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution                : chr [1:96] "PT15M" "PT15M" "PT15M" "PT15M" ...
#>  $ ts_time_interval_start       : POSIXct[1:96], format: "2025-02-28 23:00:00" "2025-02-28 23:00:00" ...
#>  $ ts_time_interval_end         : POSIXct[1:96], format: "2025-03-01 23:00:00" "2025-03-01 23:00:00" ...
#>  $ ts_mrid                      : num [1:96] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start            : POSIXct[1:96], format: "2025-02-28 23:00:00" "2025-02-28 23:15:00" ...
#>  $ ts_point_quantity            : num [1:96] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ ts_quantity_measure_unit_name: chr [1:96] "MWH" "MWH" "MWH" "MWH" ...
```
