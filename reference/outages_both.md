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

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with the queried data, or `NULL` if no data is available for the given
parameters.

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
#> → https://web-api.tp.entsoe.eu/api?documentType=A80&biddingZone_Domain=10YFR-RTE------C&periodStart=202603172300&periodEnd=202603182300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 17 Mar 2026 22:17:25 GMT
#> <- content-type: application/zip
#> <- content-length: 70705
#> <- content-disposition: attachment; filename="Unavailability_of_production_and_generation_units_201803250000-209912310100.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpCEPnwY/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_201803250000-203408312200.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202002220100-209912310100.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202006292130-209912310100.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202103312200-202712312300.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202504251500-202604241500.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202505130600-202603251600.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/007-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202508260500-202605261500.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/008-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202510312125-202606172030.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/009-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202511240600-202606121500.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/010-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512120600-202608141500.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/011-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601022200-202609112100.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/012-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601162300-202604122200.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/013-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601302000-202604202205.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/014-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601302300-202604302200.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/015-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602132100-202605142000.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/016-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602132300-202605142200.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/017-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602202230-202604012130.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/018-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230500-202610021500.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/019-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230600-202603261600.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/020-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230700-202611071500.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/021-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602251400-202603301400.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/022-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272130-202604082130.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/023-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272130-202604082205.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/024-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272200-202606042100.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/025-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272300-202604122200.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/026-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272300-202604302100.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/027-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272300-202612312300.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/028-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603020600-202611131600.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/029-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603062300-202603190800.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/030-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603100600-202610211400.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/031-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603132100-202604252000.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/032-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603140700-202603180500.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/033-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603161600-202603180700.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/034-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603170600-202603181600.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/035-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603170600-202603201700.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/036-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603170600-202603271600.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/037-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603180130-202603180200.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/038-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603180500-202603180915.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/039-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603180600-202603181600.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/040-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603180600-202603181600.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/041-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603180600-202603201700.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/042-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603180700-202603181600.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/043-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603180700-202603181600.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/044-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603180700-202603181600.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/045-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603180830-202603181700.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/046-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603180915-202603181800.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/047-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603181000-202603181700.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/048-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603181400-202603181500.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/049-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603181430-202603181500.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/050-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603181630-202603181700.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/051-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603181630-202603181700.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/052-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603181800-202603191400.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/053-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202405181730-202603301500.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/054-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202511171553-202604070500.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/055-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512120655-202604301500.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/056-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512312300-202612312300.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/057-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601061455-202604201500.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/058-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601181900-202603261530.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/059-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601240700-202604101500.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/060-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603031730-202603181600.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/061-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603031749-202603181600.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/062-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603060700-202603271530.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/063-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603170052-202603201100.xml has been read in
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■                       33% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■                       33% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■                       33% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■                       33% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■                       33% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■                       33% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■                       33% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■                       33% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■                       33% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■                       33% | ETA:  2s
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  2s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ℹ pulling area_eic_name table from cache
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A77&biddingZone_Domain=10YFR-RTE------C&periodStart=202603172300&periodEnd=202603182300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 17 Mar 2026 22:17:29 GMT
#> <- content-type: application/zip
#> <- content-length: 6465
#> <- content-disposition: attachment; filename="Unavailability_of_production_and_generation_units_202504090830-202607311500.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpCEPnwY/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603180600-202603181300.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603180600-202603181600.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603180600-202603181630.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603180700-202603181100.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603181400-202603181500.xml has been read in
#> ✔ /tmp/RtmpCEPnwY/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202504090830-202607311500.xml has been read in
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache

dplyr::glimpse(df)
#> Rows: 69
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
#> $ revision_number                    <dbl> 256, 26, 10, 2, 5, 10, 9, 26, 4, 5, 8, 5, 7, 10, 12, 10, 4, 2, 9, 5, 3, 1, …
#> $ unavailability_time_interval_start <dttm> 2018-03-25 00:00:00, 2020-02-22 01:00:00, 2020-06-29 21:30:00, 2021-03-31 …
#> $ unavailability_time_interval_end   <dttm> 2034-08-31 22:00:00, 2099-12-31 01:00:00, 2099-12-31 01:00:00, 2027-12-31 …
#> $ ts_available_period_resolution     <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT…
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_production_psr_nominal_p        <dbl> 62.0, 880.0, 880.0, 580.0, 182.0, 33.0, 45.0, 1330.0, 30.9, 13.5, 1330.0, 9…
#> $ ts_available_period_point_quantity <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"…
```
