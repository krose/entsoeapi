# Get Fall-Back Procedures. (IFs IN 7.2, mFRR 3.11, aFRR 3.10)

It publishes of application of fall back procedures by participants in
European platforms as a result of disconnection of DSO from the European
platform, unavailability of European platform itself (planned or
unplanned outage) or the situation where the algorithm used on the
platform fails or does not find solution.

## Usage

``` r
outages_fallbacks(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  process_type = "A63",
  event_nature = "A53",
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the bidding zone/ control area

- period_start:

  the starting date of the in-scope period in POSIXct or YYYY-MM-DD
  HH:MM:SS format One year range limit applies

- period_end:

  the ending date of the outage in-scope period in POSIXct or YYYY-MM-DD
  HH:MM:SS format One year range limit applies

- process_type:

  "A47" = mFRR "A51" = aFRR "A63" = imbalance netting defaults to "A63"

- event_nature:

  "C47" = Disconnection, "A53" = Planned maintenance, "A54": Unplanned
  outage, "A83" = Auction cancellation (used in case no solution found
  or algorithm failure); Defaults to "A53".

- tidy_output:

  Defaults to TRUE. flatten nested tables

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
#'
df <- entsoeapi::outages_fallbacks(
  eic = "10YBE----------2",
  period_start = lubridate::ymd(x = "2023-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-01-01", tz = "CET"),
  process_type = "A51",
  event_nature = "C47")
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A53&biddingZone_Domain=10YBE----------2&processType=A51&businessType=C47&periodStart=202212312300&periodEnd=202312312300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:09:24 GMT
#> <- content-type: application/zip
#> <- content-length: 4696
#> <- content-disposition: attachment; filename="Fall-backs_202305222200-202309080130.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/Rtmpb5sMb4/001-FALL_BACKS_202305222200-202305232200.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/002-FALL_BACKS_202306262200-202306270900.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/003-FALL_BACKS_202307202200-202307211245.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/004-FALL_BACKS_202308232200-202308232230.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/005-FALL_BACKS_202308272200-202308280445.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/006-FALL_BACKS_202309072200-202309080130.xml has been read in

str(df)
#> tibble [6 × 18] (S3: tbl_df/tbl/data.frame)
#>  $ ts_bidding_zone_domain_mrid       : chr [1:6] "10YBE----------2" "10YBE----------2" "10YBE----------2" "10YBE----------2" ...
#>  $ ts_bidding_zone_domain_name       : chr [1:6] "Belgium" "Belgium" "Belgium" "Belgium" ...
#>  $ doc_status_value                  : chr [1:6] "A02" "A02" "A02" "A02" ...
#>  $ doc_status                        : chr [1:6] "Allocated capacity schedule" "Allocated capacity schedule" "Allocated capacity schedule" "Allocated capacity schedule" ...
#>  $ type                              : chr [1:6] "A53" "A53" "A53" "A53" ...
#>  $ type_def                          : chr [1:6] "Outage publication Document" "Outage publication Document" "Outage publication Document" "Outage publication Document" ...
#>  $ process_type                      : chr [1:6] "A51" "A51" "A51" "A51" ...
#>  $ process_type_def                  : chr [1:6] "Automatic frequency restoration reserve" "Automatic frequency restoration reserve" "Automatic frequency restoration reserve" "Automatic frequency restoration reserve" ...
#>  $ ts_business_type                  : chr [1:6] "C47" "C47" "C47" "C47" ...
#>  $ ts_business_type_def              : chr [1:6] "Disconnection" "Disconnection" "Disconnection" "Disconnection" ...
#>  $ created_date_time                 : POSIXct[1:6], format: "2025-04-15 12:57:36" "2025-04-15 12:34:15" ...
#>  $ ts_reason_code                    : chr [1:6] "B13" "B13" "B13" "B13" ...
#>  $ ts_reason_text                    : chr [1:6] "Real time connection lost - Communication status currently inactive" "Real time connection lost - Communication status currently inactive" "Real time connection lost - Communication status currently inactive" "Real time connection lost - Communication status currently inactive" ...
#>  $ revision_number                   : num [1:6] 1 1 1 1 1 1
#>  $ unavailability_time_interval_start: POSIXct[1:6], format: "2023-05-22 22:00:00" "2023-06-26 22:00:00" ...
#>  $ unavailability_time_interval_end  : POSIXct[1:6], format: "2023-05-23 22:00:00" "2023-06-27 09:00:00" ...
#>  $ ts_mrid                           : num [1:6] 1 1 1 1 1 1
#>  $ ts_quantity_measure_unit_name     : chr [1:6] "MAW" "MAW" "MAW" "MAW" ...
```
