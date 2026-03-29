# Get Unavailability of Generation Units. (15.1.A&B)

The planned and forced unavailability of generation units expected to
last at least one market time unit up to 3 years ahead. The "available
capacity during the event" means the minimum available generation
capacity during the period specified.

## Usage

``` r
outages_gen_units(
  eic = NULL,
  period_start = ymd(Sys.Date() + days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date() + days(x = 2L), tz = "CET"),
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
  "A13" for withdrawn. Defaults to NULL which means "A05" and "A09"
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
with the queried data.

## See also

Other outage endpoints:
[`outages_both()`](https://krose.github.io/entsoeapi/reference/outages_both.md),
[`outages_cons_units()`](https://krose.github.io/entsoeapi/reference/outages_cons_units.md),
[`outages_fallbacks()`](https://krose.github.io/entsoeapi/reference/outages_fallbacks.md),
[`outages_offshore_grid()`](https://krose.github.io/entsoeapi/reference/outages_offshore_grid.md),
[`outages_prod_units()`](https://krose.github.io/entsoeapi/reference/outages_prod_units.md),
[`outages_transmission_grid()`](https://krose.github.io/entsoeapi/reference/outages_transmission_grid.md)

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
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A80&biddingZone_Domain=10YFR-RTE------C&periodStart=202603292200&periodEnd=202603302200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Sun, 29 Mar 2026 15:50:47 GMT
#> <- content-type: application/zip
#> <- content-length: 78765
#> <- content-disposition: attachment; filename="Unavailability_of_production_and_generation_units_201803250000-209912310100.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/Rtmp8QGEwR/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202504251500-202604241500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202505130600-202604070600.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202508260500-202605221500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202510312125-202606172030.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202511240600-202606301500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512120600-202608141500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/007-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601022200-202609112100.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/008-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601162300-202604122200.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/009-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601302000-202604202205.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/010-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601302300-202604302200.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/011-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602132100-202605142000.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/012-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602132300-202605142200.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/013-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230500-202610021500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/014-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230700-202611071500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/015-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602251400-202604301300.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/016-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272200-202606042100.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/017-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272300-202604122200.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/018-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272300-202604302100.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/019-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272300-202612312300.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/020-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603020600-202610091500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/021-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603132100-202604252000.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/022-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603192300-202604102100.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/023-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603212300-202604030600.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/024-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230600-202604031500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/025-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230600-202604031500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/026-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230630-202604011430.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/027-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603271600-202604161500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/028-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603271630-202603302300.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/029-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603272300-202606242200.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/030-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603280846-202603312045.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/031-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603282300-202607072200.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/032-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603291030-202604010330.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/033-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604031500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/034-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604071500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/035-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604241500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/036-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604241500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/037-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604241500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/038-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604241500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/039-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604241500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/040-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202611201600.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/041-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202712311600.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/042-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300530-202603301500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/043-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300530-202603301500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/044-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300600-202603311500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/045-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300600-202603311500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/046-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300700-202603301300.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/047-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603301200-202603301400.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/048-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202405181730-202604070800.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/049-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202511171553-202604070500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/050-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512120655-202604301500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/051-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512312300-202612312300.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/052-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601061455-202604201500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/053-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601181900-202604011430.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/054-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601240700-202604101500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/055-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603031730-202603301500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/056-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603031749-202603301500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/057-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603060700-202604031430.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/058-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603211700-202603311500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/059-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603231030-202604301200.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/060-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603250630-202604031430.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/061-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603271500-202603301430.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/062-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603282230-202603301500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/063-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_201803250000-203408312200.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/064-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202002220100-209912310100.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/065-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202006292130-209912310100.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/066-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202103312200-202712312300.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/067-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272130-202604082205.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/068-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603100600-202610211400.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/069-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230600-202604101500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/070-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202605221500.xml has been read in
#> ✔ /tmp/Rtmp8QGEwR/071-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202605221500.xml has been read in
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■                     39% | ETA:  2s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        85% | ETA:  0s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!

dplyr::glimpse(df)
#> Rows: 71
#> Columns: 28
#> $ ts_bidding_zone_domain_mrid        <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE-----…
#> $ ts_bidding_zone_domain_name        <chr> "France", "France", "France", "France", "France", "France", "France", "Fran…
#> $ ts_production_mrid                 <chr> "17W100P100P0300X", "17W000001017800G", "17W000001017789F", "17W100P100P021…
#> $ ts_production_name                 <chr> "VILLARODIN", "SAUVETERRE", "CHAUTAGNE", "FLAMANVILLE 2", "CADEROUSSE", "SE…
#> $ ts_production_psr_mrid             <chr> "17W100P100P00877", "17W100P100P1211R", "17W100P100P15262", "17W100P100P012…
#> $ ts_production_psr_name             <chr> "VILLARODIN 1", "SAUVETERRE G1", "CHAUTAGNE G1", "FLAMANVILLE 2", "CADEROUS…
#> $ ts_production_location_name        <chr> "FRANCE", "France", "France", "FRANCE", "France", "France", "FRANCE", "FRAN…
#> $ type                               <chr> "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80"…
#> $ type_def                           <chr> "Generation unavailability", "Generation unavailability", "Generation unava…
#> $ process_type                       <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26"…
#> $ process_type_def                   <chr> "Outage information", "Outage information", "Outage information", "Outage i…
#> $ ts_business_type                   <chr> "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53"…
#> $ ts_business_type_def               <chr> "Planned maintenance", "Planned maintenance", "Planned maintenance", "Plann…
#> $ ts_production_psr_type             <chr> "B12", "B11", "B11", "B14", "B11", "B11", "B14", "B14", "B14", "B14", "B14"…
#> $ ts_production_psr_type_def         <chr> "Hydro-electric storage head installation", "Hydro Run-of-river head instal…
#> $ created_date_time                  <dttm> 2025-10-08 01:34:11, 2026-03-27 01:48:06, 2026-03-20 10:35:02, 2025-11-10 …
#> $ reason_code                        <chr> "B19", "B19", "B19", "A95", "B19", "B19", "A95", "A95", "A95", "A95", "A95"…
#> $ reason_text                        <chr> "Foreseen maintenance", "Foreseen maintenance", "Foreseen maintenance", "Af…
#> $ revision_number                    <dbl> 5, 14, 10, 26, 5, 5, 8, 5, 7, 10, 12, 10, 2, 5, 4, 9, 6, 5, 3, 3, 11, 1, 1,…
#> $ unavailability_time_interval_start <dttm> 2025-04-25 15:00:00, 2025-05-13 06:00:00, 2025-08-26 05:00:00, 2025-10-31 …
#> $ unavailability_time_interval_end   <dttm> 2026-04-24 15:00:00, 2026-04-07 06:00:00, 2026-05-22 15:00:00, 2026-06-17 …
#> $ ts_available_period_resolution     <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT…
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_production_psr_nominal_p        <dbl> 182.0, 33.0, 45.0, 1330.0, 30.9, 13.5, 1330.0, 910.0, 915.0, 1300.0, 1500.0…
#> $ ts_available_period_point_quantity <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 810, 0, 0, 0…
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"…
#> $ doc_status_value                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ doc_status                         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
```
