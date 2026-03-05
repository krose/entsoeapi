# Balancing Border Capacity Limitations (IFs 4.3 & 4.4)

This data item publish limitations on borders requested by participating
or affected TSOs.

## Usage

``` r
balancing_border_cap_limit(
  eic_in = NULL,
  eic_out = NULL,
  process_type = "A51",
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic_in:

  Energy Identification Code of an in LFC Area (LFA) or in Scheduling
  area (SCA)

- eic_out:

  Energy Identification Code of out an out LFC Area (LFA) or out
  Scheduling area (SCA)

- process_type:

  type of frequency restoration reserve "A47" mFRR "A51" aFRR "A63"
  Imbalance Netting

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
df <- entsoeapi::balancing_border_cap_limit(
  eic_in = "10YDE-RWENET---I",
  eic_out = "10YBE----------2",
  process_type = "A51",
  period_start = lubridate::ymd(x = "2022-06-22", tz = "CET"),
  period_end = lubridate::ymd(x = "2022-06-23", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A31&BusinessType=A26&processType=A51&In_Domain=10YDE-RWENET---I&Out_Domain=10YBE----------2&periodStart=202206212200&periodEnd=202206222200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 05 Mar 2026 16:11:51 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Balancing Border Capacity Limitations_202206212200-202206222200.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived

str(df)
#> tibble [96 × 24] (S3: tbl_df/tbl/data.frame)
#>  $ domain_mrid           : chr [1:96] "10YDOM-REGION-1V" "10YDOM-REGION-1V" "10YDOM-REGION-1V" "10YDOM-REGION-1V" ...
#>  $ domain_name           : chr [1:96] "CWE Region" "CWE Region" "CWE Region" "CWE Region" ...
#>  $ ts_in_domain_mrid     : chr [1:96] "10YDE-RWENET---I" "10YDE-RWENET---I" "10YDE-RWENET---I" "10YDE-RWENET---I" ...
#>  $ ts_in_domain_name     : chr [1:96] "Amprion GmbH" "Amprion GmbH" "Amprion GmbH" "Amprion GmbH" ...
#>  $ ts_out_domain_mrid    : chr [1:96] "10YBE----------2" "10YBE----------2" "10YBE----------2" "10YBE----------2" ...
#>  $ ts_out_domain_name    : chr [1:96] "Belgium" "Belgium" "Belgium" "Belgium" ...
#>  $ ts_product            : chr [1:96] "8716867000016" "8716867000016" "8716867000016" "8716867000016" ...
#>  $ ts_product_def        : chr [1:96] "Active power" "Active power" "Active power" "Active power" ...
#>  $ type                  : chr [1:96] "A31" "A31" "A31" "A31" ...
#>  $ type_def              : chr [1:96] "Agreed capacity" "Agreed capacity" "Agreed capacity" "Agreed capacity" ...
#>  $ process_type          : chr [1:96] "A51" "A51" "A51" "A51" ...
#>  $ process_type_def      : chr [1:96] "Automatic frequency restoration reserve" "Automatic frequency restoration reserve" "Automatic frequency restoration reserve" "Automatic frequency restoration reserve" ...
#>  $ ts_business_type      : chr [1:96] "A26" "A26" "A26" "A26" ...
#>  $ ts_business_type_def  : chr [1:96] "Available transfer capacity (ATC)" "Available transfer capacity (ATC)" "Available transfer capacity (ATC)" "Available transfer capacity (ATC)" ...
#>  $ created_date_time     : POSIXct[1:96], format: "2026-03-05 16:11:51" "2026-03-05 16:11:51" "2026-03-05 16:11:51" "2026-03-05 16:11:51" ...
#>  $ ts_reason_code        : chr [1:96] "B47" "B47" "B47" "B47" ...
#>  $ ts_reason_text        : chr [1:96] "Operational security constraints" "Operational security constraints" "Operational security constraints" "Operational security constraints" ...
#>  $ revision_number       : num [1:96] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution         : chr [1:96] "PT15M" "PT15M" "PT15M" "PT15M" ...
#>  $ ts_time_interval_start: POSIXct[1:96], format: "2022-06-21 22:00:00" "2022-06-21 22:00:00" "2022-06-21 22:00:00" "2022-06-21 22:00:00" ...
#>  $ ts_time_interval_end  : POSIXct[1:96], format: "2022-06-22 22:00:00" "2022-06-22 22:00:00" "2022-06-22 22:00:00" "2022-06-22 22:00:00" ...
#>  $ ts_mrid               : num [1:96] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start     : POSIXct[1:96], format: "2022-06-21 22:00:00" "2022-06-21 22:15:00" "2022-06-21 22:30:00" "2022-06-21 22:45:00" ...
#>  $ ts_point_quantity     : num [1:96] 150 150 150 150 150 150 150 150 113 114 ...
```
