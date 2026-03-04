# Get Unavailability of Generation Units. (15.1.A&B)

The planned and forced unavailability of generation units expected to
last at least one market time unit up to 3 years ahead. The "available
capacity during the event" means the minimum available generation
capacity during the period specified.

## Usage

``` r
outages_gen_units(
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

  Energy Identification Code of the bidding zone/ control area (To
  extract outages of bidding zone DE-AT-LU area, it is recommended to
  send queries per control area i.e. CTA\|DE(50Hertz), CTA\|DE(Amprion),
  CTA\|DE(TeneTGer),CTA\|DE(TransnetBW), CTA\|AT,CTA\|LU but not per
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
df <- entsoeapi::outages_gen_units(
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
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A80&biddingZone_Domain=10YFR-RTE------C&periodStart=202603042300&periodEnd=202603052300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:09:26 GMT
#> <- content-type: application/zip
#> <- content-length: 56155
#> <- content-disposition: attachment; filename="Unavailability_of_production_and_generation_units_201803250000-209912310100.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/Rtmpb5sMb4/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_201803250000-203408312200.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202002220100-209912310100.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202006292130-209912310100.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202103312200-202712312300.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202504251500-202604241500.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202505130600-202603171600.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/007-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202508260500-202604301500.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/008-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202510312125-202606172030.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/009-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202511240600-202606121500.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/010-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512120600-202605151500.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/011-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601022200-202609112100.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/012-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601162300-202604122200.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/013-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601302000-202604202205.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/014-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601302300-202604302200.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/015-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602062200-202603152200.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/016-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602132100-202605142000.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/017-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602132300-202605142200.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/018-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602202230-202604012130.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/019-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602220602-202603131900.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/020-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230500-202610021500.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/021-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230600-202603171600.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/022-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230700-202611071500.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/023-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602251400-202603091500.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/024-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272130-202604082130.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/025-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272130-202604082205.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/026-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272200-202606042100.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/027-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272300-202604082200.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/028-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272300-202604122200.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/029-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272300-202612312300.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/030-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603020600-202611131600.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/031-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603020630-202603061530.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/032-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603040900-202603060900.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/033-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603040900-202603091400.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/034-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603042300-202603132200.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/035-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603050130-202603050200.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/036-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603050600-202603051140.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/037-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603050630-202603051530.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/038-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603050730-202603051230.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/039-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603051430-202603051500.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/040-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603051530-202603051600.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/041-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202405181730-202603251600.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/042-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202511171553-202604070500.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/043-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512120655-202604301500.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/044-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512312300-202612312300.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/045-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601061455-202604201500.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/046-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601181900-202604031430.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/047-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601240700-202604101500.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/048-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601301501-202603062300.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/049-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603031730-202603051600.xml has been read in
#> ✔ /tmp/Rtmpb5sMb4/050-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603031749-202603051600.xml has been read in
#> processing xml list ■■■■■■■■■■■■                      38% | ETA:  2s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA:  1s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s

str(df)
#> tibble [50 × 28] (S3: tbl_df/tbl/data.frame)
#>  $ ts_bidding_zone_domain_mrid       : chr [1:50] "10YFR-RTE------C" "10YFR-RTE------C" "10YFR-RTE------C" "10YFR-RTE------C" ...
#>  $ ts_bidding_zone_domain_name       : chr [1:50] "France" "France" "France" "France" ...
#>  $ ts_production_mrid                : chr [1:50] "17W100P100P0352E" "17W100P100P0207N" "17W100P100P0208L" "17W100P100P0238C" ...
#>  $ ts_production_name                : chr [1:50] "CYCOFOS TV2" "FESSENHEIM 1" "FESSENHEIM 2" "HAVRE 4" ...
#>  $ ts_production_psr_mrid            : chr [1:50] "17W100P100P03396" "17W100P100P0124R" "17W100P100P0125P" "17W100P100P0023X" ...
#>  $ ts_production_psr_name            : chr [1:50] "CYCOFOS PL2" "FESSENHEIM 1" "FESSENHEIM 2" "HAVRE 4" ...
#>  $ doc_status_value                  : chr [1:50] "A09" "A09" "A09" "A09" ...
#>  $ doc_status                        : chr [1:50] "Finalised schedule" "Finalised schedule" "Finalised schedule" "Finalised schedule" ...
#>  $ ts_production_location_name       : chr [1:50] "France" "FRANCE" "FRANCE" "FRANCE" ...
#>  $ type                              : chr [1:50] "A80" "A80" "A80" "A80" ...
#>  $ type_def                          : chr [1:50] "Generation unavailability" "Generation unavailability" "Generation unavailability" "Generation unavailability" ...
#>  $ process_type                      : chr [1:50] "A26" "A26" "A26" "A26" ...
#>  $ process_type_def                  : chr [1:50] "Outage information" "Outage information" "Outage information" "Outage information" ...
#>  $ ts_business_type                  : chr [1:50] "A53" "A53" "A53" "A53" ...
#>  $ ts_business_type_def              : chr [1:50] "Planned maintenance" "Planned maintenance" "Planned maintenance" "Planned maintenance" ...
#>  $ ts_production_psr_type            : chr [1:50] "B20" "B14" "B14" "B05" ...
#>  $ ts_production_psr_type_def        : chr [1:50] "Other unspecified" "Nuclear unspecified" "Nuclear unspecified" "Fossil Hard coal" ...
#>  $ created_date_time                 : POSIXct[1:50], format: "2025-10-08 01:24:29" "2025-10-07 03:23:00" ...
#>  $ reason_code                       : chr [1:50] "A95" "A95" "A95" "B20" ...
#>  $ reason_text                       : chr [1:50] "Awaiting information - Complementary information" "For more information please refer to REMIT message EDF-2019-00058 and EDF press releases of April 6th 2017, Jul"| __truncated__ "For more information please refer to REMIT message EDF-2019-00058 and EDF press releases of April 6th 2017, Jul"| __truncated__ "Shutdown" ...
#>  $ revision_number                   : num [1:50] 256 26 10 2 5 9 8 26 4 4 ...
#>  $ unavailability_time_interval_start: POSIXct[1:50], format: "2018-03-25 00:00:00" "2020-02-22 01:00:00" ...
#>  $ unavailability_time_interval_end  : POSIXct[1:50], format: "2034-08-31 22:00:00" "2099-12-31 01:00:00" ...
#>  $ ts_available_period_resolution    : chr [1:50] "PT1M" "PT1M" "PT1M" "PT1M" ...
#>  $ ts_mrid                           : num [1:50] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_production_psr_nominal_p       : num [1:50] 62 880 880 580 182 33 45 1330 30.9 13.5 ...
#>  $ ts_available_period_point_quantity: num [1:50] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ ts_quantity_measure_unit_name     : chr [1:50] "MAW" "MAW" "MAW" "MAW" ...
```
