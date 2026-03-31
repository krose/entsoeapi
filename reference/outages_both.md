# Get Unavailability of Production & Generation Units (15.1.A&B + 15.1.C&D)

The planned and forced unavailability of production and generation units
expected to last at least one market time unit up to 3 years ahead. The
"available capacity during the event" means the minimum available
generation capacity during the period specified.

## Usage

``` r
outages_both(
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
with the queried data.

## See also

Other outage endpoints:
[`outages_cons_units()`](https://krose.github.io/entsoeapi/reference/outages_cons_units.md),
[`outages_fallbacks()`](https://krose.github.io/entsoeapi/reference/outages_fallbacks.md),
[`outages_gen_units()`](https://krose.github.io/entsoeapi/reference/outages_gen_units.md),
[`outages_offshore_grid()`](https://krose.github.io/entsoeapi/reference/outages_offshore_grid.md),
[`outages_prod_units()`](https://krose.github.io/entsoeapi/reference/outages_prod_units.md),
[`outages_transmission_grid()`](https://krose.github.io/entsoeapi/reference/outages_transmission_grid.md)

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
#> → https://web-api.tp.entsoe.eu/api?documentType=A80&biddingZone_Domain=10YFR-RTE------C&periodStart=202603312200&periodEnd=202604012200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 31 Mar 2026 06:58:26 GMT
#> <- content-type: application/zip
#> <- content-length: 94034
#> <- content-disposition: attachment; filename="Unavailability_of_production_and_generation_units_201803250000-209912310100.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpOqoMiS/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_201803250000-203408312200.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202002220100-209912310100.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202006292130-209912310100.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202103312200-202712312300.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202504251500-202604241500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202505130600-202604070600.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/007-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202508260500-202605221500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/008-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202510312125-202606172030.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/009-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202511240600-202606301500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/010-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512120600-202608141500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/011-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601022200-202609112100.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/012-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601162300-202604122200.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/013-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601302000-202604202205.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/014-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601302300-202604302200.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/015-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602132100-202605142000.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/016-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602132300-202605142200.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/017-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230500-202610021500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/018-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230700-202611071500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/019-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602251400-202604301300.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/020-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272130-202604082205.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/021-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272200-202606042100.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/022-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272300-202604122200.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/023-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272300-202604302100.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/024-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272300-202612312300.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/025-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603020600-202610091500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/026-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603100600-202610211400.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/027-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603132100-202604252000.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/028-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603192300-202604102100.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/029-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603212300-202604030600.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/030-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230600-202604031500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/031-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230600-202604031500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/032-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230600-202604101500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/033-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230630-202604011430.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/034-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603271600-202604161500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/035-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603272300-202606242200.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/036-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603280846-202604010746.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/037-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603282300-202607072200.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/038-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603291030-202604011330.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/039-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604031500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/040-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604071500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/041-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604241500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/042-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604241500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/043-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604241500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/044-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604241500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/045-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604241500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/046-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202605221500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/047-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202605221500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/048-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202611201600.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/049-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202712311600.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/050-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603301442-202604032000.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/051-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603310030-202604032200.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/052-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603311030-202604071000.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/053-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603312200-202604052200.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/054-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603312200-202604052200.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/055-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603312200-202607212200.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/056-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603312200-202610312300.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/057-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603312200-202610312300.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/058-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603312330-202604010000.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/059-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010500-202604171600.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/060-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010500-202604171600.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/061-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010500-202604171600.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/062-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010500-202604171600.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/063-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010500-202604171600.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/064-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010530-202604011430.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/065-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010530-202604011430.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/066-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010530-202604011500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/067-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010530-202604011500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/068-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010545-202707021500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/069-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010600-202604012000.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/070-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010700-202604011300.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/071-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010700-202604011500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/072-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010700-202604011500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/073-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010700-202604031000.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/074-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604011200-202604011400.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/075-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604011200-202604021430.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/076-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202405181730-202604070800.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/077-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202511171553-202604070500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/078-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512120655-202604301500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/079-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512312300-202612312300.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/080-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601061455-202604301500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/081-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601181900-202604011430.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/082-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601240700-202604101500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/083-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603060700-202604031430.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/084-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603231030-202604301200.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/085-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603250630-202604031430.xml has been read in
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
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■                        31% | ETA:  2s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  1s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A77&biddingZone_Domain=10YFR-RTE------C&periodStart=202603312200&periodEnd=202604012200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 31 Mar 2026 06:58:31 GMT
#> <- content-type: application/zip
#> <- content-length: 13727
#> <- content-disposition: attachment; filename="Unavailability_of_production_and_generation_units_202504090830-202607311500.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpOqoMiS/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230600-202604101500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230700-202605110600.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300400-202604031500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604241500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202605221500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300600-202604011500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/007-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010530-202604011500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/008-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010600-202604011500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/009-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010600-202604011500.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/010-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010600-202604011600.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/011-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010600-202604011600.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/012-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010600-202604020600.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/013-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202504090830-202607311500.xml has been read in
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

dplyr::glimpse(df)
#> Rows: 98
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
#> $ revision_number                    <dbl> 256, 26, 10, 2, 5, 14, 10, 26, 5, 5, 8, 5, 7, 10, 12, 10, 2, 5, 4, 6, 9, 6,…
#> $ unavailability_time_interval_start <dttm> 2018-03-25 00:00:00, 2020-02-22 01:00:00, 2020-06-29 21:30:00, 2021-03-31 …
#> $ unavailability_time_interval_end   <dttm> 2034-08-31 22:00:00, 2099-12-31 01:00:00, 2099-12-31 01:00:00, 2027-12-31 …
#> $ ts_available_period_resolution     <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT…
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_production_psr_nominal_p        <dbl> 62.0, 880.0, 880.0, 580.0, 182.0, 33.0, 45.0, 1330.0, 30.9, 13.5, 1330.0, 9…
#> $ ts_available_period_point_quantity <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"…
```
