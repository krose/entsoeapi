# Get Netted Volumes (IFs IN 3.10)

The net position informs whether the given area imports or exports
energy. Those rows which hold the queried eic value in the
'ts_connecting_domain_mrid' column show the export value. Those rows
which hold the queried eic value in the 'ts_acquiring_domain_mrid'
column show the import value.

## Usage

``` r
netted_volumes(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the area

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
df <- entsoeapi::netted_volumes(
  eic = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2022-08-16", tz = "CET"),
  period_end = lubridate::ymd(x = "2022-08-17", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=B17&processType=A63&Acquiring_domain=10YCZ-CEPS-----N&Connecting_Domain=10YCZ-CEPS-----N&periodStart=202208152200&periodEnd=202208162200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:13:49 GMT
#> <- content-type: application/zip
#> <- content-length: 1782
#> <- content-disposition: attachment; filename="Netted and Exchanged Volumes [IFs IN 3.10, mFRR 3.17, aFRR 3.16]_202208152200-202208162200.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpSo3AKG/001-NETTED_AND_EXCHANGED_VOLUMES_202208152200-202208162200.xml has been read in

str(df)
#> tibble [96 × 23] (S3: tbl_df/tbl/data.frame)
#>  $ area_domain_mrid             : chr [1:96] "10Y1001C--00119X" "10Y1001C--00119X" "10Y1001C--00119X" "10Y1001C--00119X" ...
#>  $ area_domain_name             : chr [1:96] "Imbalance netting region" "Imbalance netting region" "Imbalance netting region" "Imbalance netting region" ...
#>  $ ts_connecting_domain_mrid    : chr [1:96] "10Y1001C--00119X" "10Y1001C--00119X" "10Y1001C--00119X" "10Y1001C--00119X" ...
#>  $ ts_connecting_domain_name    : chr [1:96] "Imbalance netting region" "Imbalance netting region" "Imbalance netting region" "Imbalance netting region" ...
#>  $ ts_acquiring_domain_mrid     : chr [1:96] "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" ...
#>  $ ts_acquiring_domain_name     : chr [1:96] "Czech Republic" "Czech Republic" "Czech Republic" "Czech Republic" ...
#>  $ doc_status_value             : chr [1:96] "A02" "A02" "A02" "A02" ...
#>  $ doc_status                   : chr [1:96] "Allocated capacity schedule" "Allocated capacity schedule" "Allocated capacity schedule" "Allocated capacity schedule" ...
#>  $ type                         : chr [1:96] "B17" "B17" "B17" "B17" ...
#>  $ type_def                     : chr [1:96] "Aggregated netted external TSO schedule document" "Aggregated netted external TSO schedule document" "Aggregated netted external TSO schedule document" "Aggregated netted external TSO schedule document" ...
#>  $ process_type                 : chr [1:96] "A63" "A63" "A63" "A63" ...
#>  $ process_type_def             : chr [1:96] "Imbalance Netting" "Imbalance Netting" "Imbalance Netting" "Imbalance Netting" ...
#>  $ ts_business_type             : chr [1:96] "B09" "B09" "B09" "B09" ...
#>  $ ts_business_type_def         : chr [1:96] "Net position" "Net position" "Net position" "Net position" ...
#>  $ created_date_time            : POSIXct[1:96], format: "2026-03-04 22:13:49" "2026-03-04 22:13:49" ...
#>  $ revision_number              : num [1:96] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution                : chr [1:96] "PT15M" "PT15M" "PT15M" "PT15M" ...
#>  $ ts_time_interval_start       : POSIXct[1:96], format: "2022-08-15 22:00:00" "2022-08-15 22:00:00" ...
#>  $ ts_time_interval_end         : POSIXct[1:96], format: "2022-08-15 23:45:00" "2022-08-15 23:45:00" ...
#>  $ ts_mrid                      : num [1:96] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start            : POSIXct[1:96], format: "2022-08-15 22:00:00" "2022-08-15 22:15:00" ...
#>  $ ts_point_quantity            : num [1:96] 1.14 3.74 9.64 13.71 5.75 ...
#>  $ ts_quantity_measure_unit_name: chr [1:96] "MWH" "MWH" "MWH" "MWH" ...
```
