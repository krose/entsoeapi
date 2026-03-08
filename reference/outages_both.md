# Get Unavailability of Production & Generation Units (15.1.A&B + 15.1.C&D)

The planned and forced unavailability of production and generation units
expected to last at least one market time unit up to 3 years ahead. The
"available capacity during the event" means the minimum available
generation capacity during the period specified.

## Usage

``` r
outages_both(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() + lubridate::days(x = 1L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date() + lubridate::days(x = 2L), tz = "CET"),
  doc_status = NULL,
  event_nature = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the bidding zone/control area (To
  extract outages of bidding zone DE-AT-LU area, it is recommended to
  send queries per control area i.e. CTA\|DE(50Hertz), CTA\|DE(Amprion),
  CTA\|DE(TeneTGer), CTA\|DE(TransnetBW),CTA\|AT,CTA\|LU but not per
  bidding zone.)

- period_start:

  the starting date of the in-scope period in POSIXct or YYYY-MM-DD
  HH:MM:SS format One year range limit applies

- period_end:

  the ending date of the outage in-scope period in POSIXct or YYYY-MM-DD
  HH:MM:SS format One year range limit applies

- doc_status:

  Notification document status. "A05" for active, "A09" for cancelled
  and "A13" for withdrawn. Defaults to NULL which means "A05" and "A09"
  together.

- event_nature:

  "A53" for planned maintenance. "A54" for unplanned outage. Defaults to
  NULL which means both of them.

- tidy_output:

  Defaults to TRUE. flatten nested tables

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df <- entsoeapi::outages_both(
  eic = "10YFR-RTE------C",
  period_start = lubridate::ymd(
    x = Sys.Date() + lubridate::days(x = 1L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    x = Sys.Date() + lubridate::days(x = 2L),
    tz = "CET"
  )
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A80&biddingZone_Domain=10YFR-RTE------C&periodStart=202603082300&periodEnd=202603092300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Sun, 08 Mar 2026 23:53:10 GMT
#> <- content-type: application/zip
#> <- content-length: 71166
#> <- content-disposition: attachment; filename="Unavailability_of_production_and_generation_units_201803250000-209912310100.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpJ8b70P/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_201803250000-203408312200.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202002220100-209912310100.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202006292130-209912310100.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202103312200-202712312300.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202504251500-202604241500.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202505130600-202603171600.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/007-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202508260500-202605261500.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/008-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202510312125-202606172030.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/009-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202511240600-202606121500.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/010-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512120600-202608141500.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/011-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601022200-202609112100.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/012-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601162300-202604122200.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/013-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601302000-202604202205.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/014-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601302300-202604302200.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/015-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602062200-202603092200.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/016-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602132100-202605142000.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/017-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602132300-202605142200.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/018-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602202230-202604012130.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/019-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602220602-202603131900.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/020-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230500-202610021500.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/021-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230600-202603171600.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/022-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230700-202611071500.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/023-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602251400-202603301400.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/024-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272130-202604082130.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/025-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272130-202604082205.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/026-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272200-202606042100.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/027-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272300-202604082200.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/028-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272300-202604122200.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/029-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272300-202612312300.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/030-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603020600-202611131600.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/031-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603042300-202603132200.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/032-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603062300-202603192300.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/033-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603070500-202603121700.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/034-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603071900-202603100900.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/035-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603072300-202603101600.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/036-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603082300-202603132300.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/037-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603082300-202603132300.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/038-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603090600-202603090700.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/039-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603090600-202603090700.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/040-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603090600-202603091600.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/041-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603090600-202603101600.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/042-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603090600-202603131600.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/043-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603090600-202603131600.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/044-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603090700-202603090800.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/045-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603090700-202603090800.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/046-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603090700-202603090800.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/047-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603090700-202603090800.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/048-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603090700-202603091600.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/049-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603090700-202603091600.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/050-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603090800-202603091600.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/051-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603091230-202603091600.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/052-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603091400-202603131600.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/053-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202405181730-202603251600.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/054-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202511171553-202604070500.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/055-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512120655-202604301500.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/056-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512312300-202612312300.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/057-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601061455-202604201500.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/058-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601181900-202603261530.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/059-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601240700-202604101500.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/060-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601301501-202603102300.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/061-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603031730-202603091600.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/062-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603031749-202603091600.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/063-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603060700-202603271530.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/064-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603071045-202603110345.xml has been read in
#> processing xml list ■■■■■■■■■■                        30% | ETA:  3s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              62% | ETA:  1s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A77&biddingZone_Domain=10YFR-RTE------C&periodStart=202603082300&periodEnd=202603092300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Sun, 08 Mar 2026 23:53:14 GMT
#> <- content-type: application/zip
#> <- content-length: 7337
#> <- content-disposition: attachment; filename="Unavailability_of_production_and_generation_units_202504090830-202607311500.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpJ8b70P/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603090600-202603091600.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603090600-202603091600.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603090600-202603131600.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603091230-202603091530.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603091300-202603091500.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603091300-202603091500.xml has been read in
#> ✔ /tmp/RtmpJ8b70P/007-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202504090830-202607311500.xml has been read in

dplyr::glimpse(df)
#> Rows: 71
#> Columns: 28
#> $ ts_bidding_zone_domain_mrid        <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE-----…
#> $ ts_bidding_zone_domain_name        <chr> "France", "France", "France", "France", "France", "France", "France", "Fran…
#> $ ts_production_mrid                 <chr> "17W100P100P0352E", "17W100P100P0207N", "17W100P100P0208L", "17W100P100P023…
#> $ ts_production_name                 <chr> "CYCOFOS TV2", "FESSENHEIM 1", "FESSENHEIM 2", "HAVRE 4", "VILLARODIN", "SA…
#> $ ts_production_psr_mrid             <chr> "17W100P100P03396", "17W100P100P0124R", "17W100P100P0125P", "17W100P100P002…
#> $ ts_production_psr_name             <chr> "CYCOFOS PL2", "FESSENHEIM 1", "FESSENHEIM 2", "HAVRE 4", "VILLARODIN 1", "…
#> $ doc_status_value                   <chr> "A09", "A09", "A09", "A09", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ doc_status                         <chr> "Finalised schedule", "Finalised schedule", "Finalised schedule", "Finalise…
#> $ ts_production_location_name        <chr> "France", "FRANCE", "FRANCE", "FRANCE", "FRANCE", "France", "France", "FRAN…
#> $ type                               <chr> "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80"…
#> $ type_def                           <chr> "Generation unavailability", "Generation unavailability", "Generation unava…
#> $ process_type                       <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26"…
#> $ process_type_def                   <chr> "Outage information", "Outage information", "Outage information", "Outage i…
#> $ ts_business_type                   <chr> "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53"…
#> $ ts_business_type_def               <chr> "Planned maintenance", "Planned maintenance", "Planned maintenance", "Plann…
#> $ ts_production_psr_type             <chr> "B20", "B14", "B14", "B05", "B12", "B11", "B11", "B14", "B11", "B11", "B14"…
#> $ ts_production_psr_type_def         <chr> "Other unspecified", "Nuclear unspecified", "Nuclear unspecified", "Fossil …
#> $ created_date_time                  <dttm> 2025-10-08 01:24:29, 2025-10-07 03:23:00, 2025-10-07 03:23:01, 2025-10-07 …
#> $ reason_code                        <chr> "A95", "A95", "A95", "B20", "B19", "B19", "B19", "A95", "B19", "B19", "A95"…
#> $ reason_text                        <chr> "Awaiting information - Complementary information", "For more information p…
#> $ revision_number                    <dbl> 256, 26, 10, 2, 5, 9, 9, 26, 4, 5, 8, 5, 7, 9, 10, 12, 10, 4, 2, 2, 8, 5, 3…
#> $ unavailability_time_interval_start <dttm> 2018-03-25 00:00:00, 2020-02-22 01:00:00, 2020-06-29 21:30:00, 2021-03-31 …
#> $ unavailability_time_interval_end   <dttm> 2034-08-31 22:00:00, 2099-12-31 01:00:00, 2099-12-31 01:00:00, 2027-12-31 …
#> $ ts_available_period_resolution     <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT…
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_production_psr_nominal_p        <dbl> 62.0, 880.0, 880.0, 580.0, 182.0, 33.0, 45.0, 1330.0, 30.9, 13.5, 1330.0, 9…
#> $ ts_available_period_point_quantity <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1166, 0, 0, 0, 0, 0, …
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"…
```
