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
#> → https://web-api.tp.entsoe.eu/api?documentType=A80&biddingZone_Domain=10YFR-RTE------C&periodStart=202603252300&periodEnd=202603262300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 25 Mar 2026 19:10:02 GMT
#> <- content-type: application/zip
#> <- content-length: 88956
#> <- content-disposition: attachment; filename="Unavailability_of_production_and_generation_units_201803250000-209912310100.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpADDhvc/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_201803250000-203408312200.xml has been read in
#> ✔ /tmp/RtmpADDhvc/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202002220100-209912310100.xml has been read in
#> ✔ /tmp/RtmpADDhvc/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202006292130-209912310100.xml has been read in
#> ✔ /tmp/RtmpADDhvc/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202103312200-202712312300.xml has been read in
#> ✔ /tmp/RtmpADDhvc/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202504251500-202604241500.xml has been read in
#> ✔ /tmp/RtmpADDhvc/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202508260500-202605221500.xml has been read in
#> ✔ /tmp/RtmpADDhvc/007-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202510312125-202606172030.xml has been read in
#> ✔ /tmp/RtmpADDhvc/008-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202511240600-202606121500.xml has been read in
#> ✔ /tmp/RtmpADDhvc/009-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512120600-202608141500.xml has been read in
#> ✔ /tmp/RtmpADDhvc/010-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601022200-202609112100.xml has been read in
#> ✔ /tmp/RtmpADDhvc/011-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601162300-202604122200.xml has been read in
#> ✔ /tmp/RtmpADDhvc/012-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601302000-202604202205.xml has been read in
#> ✔ /tmp/RtmpADDhvc/013-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601302300-202604302200.xml has been read in
#> ✔ /tmp/RtmpADDhvc/014-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602132100-202605142000.xml has been read in
#> ✔ /tmp/RtmpADDhvc/015-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602132300-202605142200.xml has been read in
#> ✔ /tmp/RtmpADDhvc/016-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602202230-202604012130.xml has been read in
#> ✔ /tmp/RtmpADDhvc/017-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230500-202610021500.xml has been read in
#> ✔ /tmp/RtmpADDhvc/018-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230600-202603261600.xml has been read in
#> ✔ /tmp/RtmpADDhvc/019-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230700-202611071500.xml has been read in
#> ✔ /tmp/RtmpADDhvc/020-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602251400-202603301400.xml has been read in
#> ✔ /tmp/RtmpADDhvc/021-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272130-202604012100.xml has been read in
#> ✔ /tmp/RtmpADDhvc/022-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272130-202604082205.xml has been read in
#> ✔ /tmp/RtmpADDhvc/023-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272200-202606042100.xml has been read in
#> ✔ /tmp/RtmpADDhvc/024-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272300-202604122200.xml has been read in
#> ✔ /tmp/RtmpADDhvc/025-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272300-202604302100.xml has been read in
#> ✔ /tmp/RtmpADDhvc/026-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272300-202612312300.xml has been read in
#> ✔ /tmp/RtmpADDhvc/027-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603020600-202610091500.xml has been read in
#> ✔ /tmp/RtmpADDhvc/028-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603100600-202610211400.xml has been read in
#> ✔ /tmp/RtmpADDhvc/029-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603132100-202604252000.xml has been read in
#> ✔ /tmp/RtmpADDhvc/030-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603170600-202603271600.xml has been read in
#> ✔ /tmp/RtmpADDhvc/031-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603192300-202604102100.xml has been read in
#> ✔ /tmp/RtmpADDhvc/032-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603202215-202603271830.xml has been read in
#> ✔ /tmp/RtmpADDhvc/033-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603212300-202604030600.xml has been read in
#> ✔ /tmp/RtmpADDhvc/034-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230600-202604031500.xml has been read in
#> ✔ /tmp/RtmpADDhvc/035-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230600-202604031500.xml has been read in
#> ✔ /tmp/RtmpADDhvc/036-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230600-202604101500.xml has been read in
#> ✔ /tmp/RtmpADDhvc/037-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230630-202603261530.xml has been read in
#> ✔ /tmp/RtmpADDhvc/038-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230630-202604011430.xml has been read in
#> ✔ /tmp/RtmpADDhvc/039-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603250630-202603271530.xml has been read in
#> ✔ /tmp/RtmpADDhvc/040-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603251730-202603270900.xml has been read in
#> ✔ /tmp/RtmpADDhvc/041-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603252300-202603272200.xml has been read in
#> ✔ /tmp/RtmpADDhvc/042-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603260130-202603260200.xml has been read in
#> ✔ /tmp/RtmpADDhvc/043-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603260600-202603261100.xml has been read in
#> ✔ /tmp/RtmpADDhvc/044-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603260600-202603261600.xml has been read in
#> ✔ /tmp/RtmpADDhvc/045-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603260600-202603261600.xml has been read in
#> ✔ /tmp/RtmpADDhvc/046-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603260600-202603261700.xml has been read in
#> ✔ /tmp/RtmpADDhvc/047-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603260630-202603261500.xml has been read in
#> ✔ /tmp/RtmpADDhvc/048-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603260700-202603261400.xml has been read in
#> ✔ /tmp/RtmpADDhvc/049-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603260700-202603261630.xml has been read in
#> ✔ /tmp/RtmpADDhvc/050-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603260700-202603261630.xml has been read in
#> ✔ /tmp/RtmpADDhvc/051-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603260700-202603261630.xml has been read in
#> ✔ /tmp/RtmpADDhvc/052-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603260800-202603261000.xml has been read in
#> ✔ /tmp/RtmpADDhvc/053-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603260800-202603261100.xml has been read in
#> ✔ /tmp/RtmpADDhvc/054-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603260800-202603261100.xml has been read in
#> ✔ /tmp/RtmpADDhvc/055-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603260800-202603261100.xml has been read in
#> ✔ /tmp/RtmpADDhvc/056-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603260800-202603261100.xml has been read in
#> ✔ /tmp/RtmpADDhvc/057-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603260800-202603261700.xml has been read in
#> ✔ /tmp/RtmpADDhvc/058-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603261230-202603261530.xml has been read in
#> ✔ /tmp/RtmpADDhvc/059-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603261230-202603261530.xml has been read in
#> ✔ /tmp/RtmpADDhvc/060-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603261230-202603261600.xml has been read in
#> ✔ /tmp/RtmpADDhvc/061-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603261230-202603261600.xml has been read in
#> ✔ /tmp/RtmpADDhvc/062-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603261230-202603261600.xml has been read in
#> ✔ /tmp/RtmpADDhvc/063-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603261300-202603261500.xml has been read in
#> ✔ /tmp/RtmpADDhvc/064-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603261300-202603261600.xml has been read in
#> ✔ /tmp/RtmpADDhvc/065-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603261400-202603261600.xml has been read in
#> ✔ /tmp/RtmpADDhvc/066-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603261430-202603261500.xml has been read in
#> ✔ /tmp/RtmpADDhvc/067-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202405181730-202604070800.xml has been read in
#> ✔ /tmp/RtmpADDhvc/068-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202511171553-202604070500.xml has been read in
#> ✔ /tmp/RtmpADDhvc/069-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512120655-202604301500.xml has been read in
#> ✔ /tmp/RtmpADDhvc/070-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512312300-202612312300.xml has been read in
#> ✔ /tmp/RtmpADDhvc/071-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601061455-202604201500.xml has been read in
#> ✔ /tmp/RtmpADDhvc/072-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601181900-202603261530.xml has been read in
#> ✔ /tmp/RtmpADDhvc/073-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601240700-202604101500.xml has been read in
#> ✔ /tmp/RtmpADDhvc/074-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603031730-202603261600.xml has been read in
#> ✔ /tmp/RtmpADDhvc/075-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603031749-202603261600.xml has been read in
#> ✔ /tmp/RtmpADDhvc/076-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603060700-202604031430.xml has been read in
#> ✔ /tmp/RtmpADDhvc/077-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603202258-202603271700.xml has been read in
#> ✔ /tmp/RtmpADDhvc/078-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603211700-202603271600.xml has been read in
#> ✔ /tmp/RtmpADDhvc/079-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603231030-202604301200.xml has been read in
#> ✔ /tmp/RtmpADDhvc/080-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603251830-202603261830.xml has been read in
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
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■                       34% | ETA:  2s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  1s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!

dplyr::glimpse(df)
#> Rows: 80
#> Columns: 28
#> $ ts_bidding_zone_domain_mrid        <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE-----…
#> $ ts_bidding_zone_domain_name        <chr> "France", "France", "France", "France", "France", "France", "France", "Fran…
#> $ ts_production_mrid                 <chr> "17W100P100P0352E", "17W100P100P0207N", "17W100P100P0208L", "17W100P100P023…
#> $ ts_production_name                 <chr> "CYCOFOS TV2", "FESSENHEIM 1", "FESSENHEIM 2", "HAVRE 4", "VILLARODIN", "CH…
#> $ ts_production_psr_mrid             <chr> "17W100P100P03396", "17W100P100P0124R", "17W100P100P0125P", "17W100P100P002…
#> $ ts_production_psr_name             <chr> "CYCOFOS PL2", "FESSENHEIM 1", "FESSENHEIM 2", "HAVRE 4", "VILLARODIN 1", "…
#> $ doc_status_value                   <chr> "A09", "A09", "A09", "A09", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ doc_status                         <chr> "Finalised schedule", "Finalised schedule", "Finalised schedule", "Finalise…
#> $ ts_production_location_name        <chr> "France", "FRANCE", "FRANCE", "FRANCE", "FRANCE", "France", "FRANCE", "Fran…
#> $ type                               <chr> "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80"…
#> $ type_def                           <chr> "Generation unavailability", "Generation unavailability", "Generation unava…
#> $ process_type                       <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26"…
#> $ process_type_def                   <chr> "Outage information", "Outage information", "Outage information", "Outage i…
#> $ ts_business_type                   <chr> "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53"…
#> $ ts_business_type_def               <chr> "Planned maintenance", "Planned maintenance", "Planned maintenance", "Plann…
#> $ ts_production_psr_type             <chr> "B20", "B14", "B14", "B05", "B12", "B11", "B14", "B11", "B11", "B14", "B14"…
#> $ ts_production_psr_type_def         <chr> "Other unspecified", "Nuclear unspecified", "Nuclear unspecified", "Fossil …
#> $ created_date_time                  <dttm> 2025-10-08 01:24:29, 2025-10-07 03:23:00, 2025-10-07 03:23:01, 2025-10-07 …
#> $ reason_code                        <chr> "A95", "A95", "A95", "B20", "B19", "B19", "A95", "B19", "B19", "A95", "A95"…
#> $ reason_text                        <chr> "Awaiting information - Complementary information", "For more information p…
#> $ revision_number                    <dbl> 256, 26, 10, 2, 5, 10, 26, 4, 5, 8, 5, 7, 10, 12, 10, 4, 2, 9, 5, 3, 2, 6, …
#> $ unavailability_time_interval_start <dttm> 2018-03-25 00:00:00, 2020-02-22 01:00:00, 2020-06-29 21:30:00, 2021-03-31 …
#> $ unavailability_time_interval_end   <dttm> 2034-08-31 22:00:00, 2099-12-31 01:00:00, 2099-12-31 01:00:00, 2027-12-31 …
#> $ ts_available_period_resolution     <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT…
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_production_psr_nominal_p        <dbl> 62.0, 880.0, 880.0, 580.0, 182.0, 45.0, 1330.0, 30.9, 13.5, 1330.0, 910.0, …
#> $ ts_available_period_point_quantity <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"…
```
