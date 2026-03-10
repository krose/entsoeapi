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
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A53&biddingZone_Domain=10YBE----------2&processType=A51&businessType=C47&periodStart=202212312300&periodEnd=202312312300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 10 Mar 2026 19:49:08 GMT
#> <- content-type: application/zip
#> <- content-length: 4696
#> <- content-disposition: attachment; filename="Fall-backs_202305222200-202309080130.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpDj0v66/001-FALL_BACKS_202305222200-202305232200.xml has been read in
#> ✔ /tmp/RtmpDj0v66/002-FALL_BACKS_202306262200-202306270900.xml has been read in
#> ✔ /tmp/RtmpDj0v66/003-FALL_BACKS_202307202200-202307211245.xml has been read in
#> ✔ /tmp/RtmpDj0v66/004-FALL_BACKS_202308232200-202308232230.xml has been read in
#> ✔ /tmp/RtmpDj0v66/005-FALL_BACKS_202308272200-202308280445.xml has been read in
#> ✔ /tmp/RtmpDj0v66/006-FALL_BACKS_202309072200-202309080130.xml has been read in

dplyr::glimpse(df)
#> Rows: 6
#> Columns: 18
#> $ ts_bidding_zone_domain_mrid        <chr> "10YBE----------2", "10YBE----------2", "10YBE----------2", "10YBE---------…
#> $ ts_bidding_zone_domain_name        <chr> "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Belgium"
#> $ doc_status_value                   <chr> "A02", "A02", "A02", "A02", "A02", "A02"
#> $ doc_status                         <chr> "Allocated capacity schedule", "Allocated capacity schedule", "Allocated ca…
#> $ type                               <chr> "A53", "A53", "A53", "A53", "A53", "A53"
#> $ type_def                           <chr> "Outage publication Document", "Outage publication Document", "Outage publi…
#> $ process_type                       <chr> "A51", "A51", "A51", "A51", "A51", "A51"
#> $ process_type_def                   <chr> "Automatic frequency restoration reserve", "Automatic frequency restoration…
#> $ ts_business_type                   <chr> "C47", "C47", "C47", "C47", "C47", "C47"
#> $ ts_business_type_def               <chr> "Disconnection", "Disconnection", "Disconnection", "Disconnection", "Discon…
#> $ created_date_time                  <dttm> 2025-04-15 12:57:36, 2025-04-15 12:34:15, 2025-04-15 12:20:12, 2025-04-15 1…
#> $ ts_reason_code                     <chr> "B13", "B13", "B13", "B13", "B13", "B13"
#> $ ts_reason_text                     <chr> "Real time connection lost - Communication status currently inactive", "Rea…
#> $ revision_number                    <dbl> 1, 1, 1, 1, 1, 1
#> $ unavailability_time_interval_start <dttm> 2023-05-22 22:00:00, 2023-06-26 22:00:00, 2023-07-20 22:00:00, 2023-08-23 2…
#> $ unavailability_time_interval_end   <dttm> 2023-05-23 22:00:00, 2023-06-27 09:00:00, 2023-07-21 12:45:00, 2023-08-23 2…
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"
```
