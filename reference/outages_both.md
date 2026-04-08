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
#> → https://web-api.tp.entsoe.eu/api?documentType=A80&biddingZone_Domain=10YFR-RTE------C&periodStart=202604082200&periodEnd=202604092200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 08 Apr 2026 13:14:55 GMT
#> <- content-type: application/zip
#> <- content-length: 126494
#> <- content-disposition: attachment; filename="Unavailability_of_production_and_generation_units_201803250000-209912310100.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpSXB21U/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202504251500-202604241500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202508260500-202605221500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202510312125-202606172030.xml has been read in
#> ✔ /tmp/RtmpSXB21U/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202511240600-202606301500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512120600-202608141500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601022200-202609112100.xml has been read in
#> ✔ /tmp/RtmpSXB21U/007-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601302000-202604202205.xml has been read in
#> ✔ /tmp/RtmpSXB21U/008-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601302300-202606102100.xml has been read in
#> ✔ /tmp/RtmpSXB21U/009-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602132100-202605142000.xml has been read in
#> ✔ /tmp/RtmpSXB21U/010-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602132300-202605142200.xml has been read in
#> ✔ /tmp/RtmpSXB21U/011-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230500-202610021500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/012-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230700-202611071500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/013-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602251400-202604301300.xml has been read in
#> ✔ /tmp/RtmpSXB21U/014-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272200-202606042100.xml has been read in
#> ✔ /tmp/RtmpSXB21U/015-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272300-202604302100.xml has been read in
#> ✔ /tmp/RtmpSXB21U/016-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272300-202612312300.xml has been read in
#> ✔ /tmp/RtmpSXB21U/017-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603020600-202610091500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/018-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603132100-202604252000.xml has been read in
#> ✔ /tmp/RtmpSXB21U/019-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603271600-202604161500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/020-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603272300-202606242200.xml has been read in
#> ✔ /tmp/RtmpSXB21U/021-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603282300-202607072200.xml has been read in
#> ✔ /tmp/RtmpSXB21U/022-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604241500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/023-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604241500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/024-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604241500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/025-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604241500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/026-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604241500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/027-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202611201600.xml has been read in
#> ✔ /tmp/RtmpSXB21U/028-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202712311600.xml has been read in
#> ✔ /tmp/RtmpSXB21U/029-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603312200-202607212200.xml has been read in
#> ✔ /tmp/RtmpSXB21U/030-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603312200-202610312300.xml has been read in
#> ✔ /tmp/RtmpSXB21U/031-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603312200-202610312300.xml has been read in
#> ✔ /tmp/RtmpSXB21U/032-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010500-202604171600.xml has been read in
#> ✔ /tmp/RtmpSXB21U/033-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010500-202604171600.xml has been read in
#> ✔ /tmp/RtmpSXB21U/034-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010500-202604171600.xml has been read in
#> ✔ /tmp/RtmpSXB21U/035-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010500-202604171600.xml has been read in
#> ✔ /tmp/RtmpSXB21U/036-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010500-202604171600.xml has been read in
#> ✔ /tmp/RtmpSXB21U/037-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604011133-202605012000.xml has been read in
#> ✔ /tmp/RtmpSXB21U/038-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604012200-202604102200.xml has been read in
#> ✔ /tmp/RtmpSXB21U/039-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604032200-202605192200.xml has been read in
#> ✔ /tmp/RtmpSXB21U/040-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604032200-202605242200.xml has been read in
#> ✔ /tmp/RtmpSXB21U/041-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604032200-202606142200.xml has been read in
#> ✔ /tmp/RtmpSXB21U/042-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604032230-202606232230.xml has been read in
#> ✔ /tmp/RtmpSXB21U/043-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604040300-202604102100.xml has been read in
#> ✔ /tmp/RtmpSXB21U/044-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604042200-202605290645.xml has been read in
#> ✔ /tmp/RtmpSXB21U/045-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604060545-202710011500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/046-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202604101500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/047-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202604101500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/048-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202604101500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/049-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202604101500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/050-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202604171500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/051-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202604171500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/052-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202604241500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/053-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202604301500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/054-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202610190500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/055-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070545-202605110600.xml has been read in
#> ✔ /tmp/RtmpSXB21U/056-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070545-202605131500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/057-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070600-202604091500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/058-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070600-202604101500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/059-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070600-202604101500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/060-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604071530-202604090800.xml has been read in
#> ✔ /tmp/RtmpSXB21U/061-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604080530-202604091500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/062-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604080530-202604091500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/063-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604080600-202604110815.xml has been read in
#> ✔ /tmp/RtmpSXB21U/064-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604080830-202604092200.xml has been read in
#> ✔ /tmp/RtmpSXB21U/065-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604081315-202604151815.xml has been read in
#> ✔ /tmp/RtmpSXB21U/066-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604081400-202604091500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/067-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604090030-202604090100.xml has been read in
#> ✔ /tmp/RtmpSXB21U/068-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604090530-202604091430.xml has been read in
#> ✔ /tmp/RtmpSXB21U/069-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604090530-202604091500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/070-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604090530-202604091500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/071-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604090800-202604091200.xml has been read in
#> ✔ /tmp/RtmpSXB21U/072-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604090830-202604090900.xml has been read in
#> ✔ /tmp/RtmpSXB21U/073-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604091200-202604091500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/074-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604091200-202604091700.xml has been read in
#> ✔ /tmp/RtmpSXB21U/075-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604091330-202604091400.xml has been read in
#> ✔ /tmp/RtmpSXB21U/076-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604091700-202604102200.xml has been read in
#> ✔ /tmp/RtmpSXB21U/077-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604092100-202604152100.xml has been read in
#> ✔ /tmp/RtmpSXB21U/078-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512120655-202604301500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/079-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512312300-202612312300.xml has been read in
#> ✔ /tmp/RtmpSXB21U/080-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601061455-202604301500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/081-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601181900-202604101430.xml has been read in
#> ✔ /tmp/RtmpSXB21U/082-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601240700-202604171500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/083-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603060700-202606261430.xml has been read in
#> ✔ /tmp/RtmpSXB21U/084-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603211700-202604091500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/085-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603231030-202604301200.xml has been read in
#> ✔ /tmp/RtmpSXB21U/086-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603250630-202604221430.xml has been read in
#> ✔ /tmp/RtmpSXB21U/087-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604011600-202604101500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/088-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604030930-202604241430.xml has been read in
#> ✔ /tmp/RtmpSXB21U/089-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604040820-202604171430.xml has been read in
#> ✔ /tmp/RtmpSXB21U/090-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604060057-202604092200.xml has been read in
#> ✔ /tmp/RtmpSXB21U/091-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_201803250000-203408312200.xml has been read in
#> ✔ /tmp/RtmpSXB21U/092-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202002220100-209912310100.xml has been read in
#> ✔ /tmp/RtmpSXB21U/093-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202006292130-209912310100.xml has been read in
#> ✔ /tmp/RtmpSXB21U/094-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202103312200-202712312300.xml has been read in
#> ✔ /tmp/RtmpSXB21U/095-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602272130-202604082205.xml has been read in
#> ✔ /tmp/RtmpSXB21U/096-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603100600-202610211400.xml has been read in
#> ✔ /tmp/RtmpSXB21U/097-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230600-202604101500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/098-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202605221500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/099-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202605221500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/100-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010545-202707021500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/101-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604060545-202605110600.xml has been read in
#> ✔ /tmp/RtmpSXB21U/102-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604060545-202707021500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/103-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202604241500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/104-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202604241500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/105-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202604241500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/106-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202604241500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/107-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202604241500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/108-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202611201600.xml has been read in
#> ✔ /tmp/RtmpSXB21U/109-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070545-202710311600.xml has been read in
#> ✔ /tmp/RtmpSXB21U/110-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070600-202604101500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/111-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604080600-202604091800.xml has been read in
#> ✔ /tmp/RtmpSXB21U/112-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604090530-202604091500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/113-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604090530-202604091500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/114-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604091300-202604091500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/115-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604091700-202604092200.xml has been read in
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
#> processing xml list ■■■■■■■                           19% | ETA:  6s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■                           19% | ETA:  6s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■                           19% | ETA:  6s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■                           19% | ETA:  6s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■                           19% | ETA:  6s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■                           19% | ETA:  6s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■                           19% | ETA:  6s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■                           19% | ETA:  6s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■                           19% | ETA:  6s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■                           19% | ETA:  6s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■                           19% | ETA:  6s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■                           19% | ETA:  6s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■                           19% | ETA:  6s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■                           19% | ETA:  6s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■                           19% | ETA:  6s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■                           19% | ETA:  6s
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          23% | ETA:  5s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  0s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A77&biddingZone_Domain=10YFR-RTE------C&periodStart=202604082200&periodEnd=202604092200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 08 Apr 2026 13:15:03 GMT
#> <- content-type: application/zip
#> <- content-length: 28900
#> <- content-disposition: attachment; filename="Unavailability_of_production_and_generation_units_202504090830-202607311500.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpSXB21U/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230600-202604101500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604241500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070001-202604092359.xml has been read in
#> ✔ /tmp/RtmpSXB21U/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070430-202604171500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202605071500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604071800-202604171500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/007-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604090530-202604091500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/008-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604090600-202604091500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/009-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604090600-202604091600.xml has been read in
#> ✔ /tmp/RtmpSXB21U/010-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604090700-202604091400.xml has been read in
#> ✔ /tmp/RtmpSXB21U/011-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604090800-202604091000.xml has been read in
#> ✔ /tmp/RtmpSXB21U/012-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604090930-202604091330.xml has been read in
#> ✔ /tmp/RtmpSXB21U/013-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604091000-202604091530.xml has been read in
#> ✔ /tmp/RtmpSXB21U/014-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604091030-202604091530.xml has been read in
#> ✔ /tmp/RtmpSXB21U/015-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604091500-202604091600.xml has been read in
#> ✔ /tmp/RtmpSXB21U/016-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202504090830-202607311500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/017-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230700-202605110600.xml has been read in
#> ✔ /tmp/RtmpSXB21U/018-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202605221500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/019-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202604101500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/020-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604080600-202604091800.xml has been read in
#> ✔ /tmp/RtmpSXB21U/021-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604081000-202604091030.xml has been read in
#> ✔ /tmp/RtmpSXB21U/022-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604081000-202604091030.xml has been read in
#> ✔ /tmp/RtmpSXB21U/023-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604090500-202604091500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/024-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604090600-202604091500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/025-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604090600-202604091500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/026-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604090600-202604091500.xml has been read in
#> ✔ /tmp/RtmpSXB21U/027-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604090920-202604091340.xml has been read in
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

dplyr::glimpse(df)
#> Rows: 142
#> Columns: 28
#> $ ts_bidding_zone_domain_mrid        <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE-----…
#> $ ts_bidding_zone_domain_name        <chr> "France", "France", "France", "France", "France", "France", "France", "Fran…
#> $ ts_production_mrid                 <chr> "17W100P100P0300X", "17W000001017789F", "17W100P100P0210Y", "17W100P100P032…
#> $ ts_production_name                 <chr> "VILLARODIN", "CHAUTAGNE", "FLAMANVILLE 2", "CADEROUSSE", "SEYSSEL", "PALUE…
#> $ ts_production_psr_mrid             <chr> "17W100P100P00877", "17W100P100P15262", "17W100P100P0127L", "17W100P100P136…
#> $ ts_production_psr_name             <chr> "VILLARODIN 1", "CHAUTAGNE G1", "FLAMANVILLE 2", "CADEROUSSE G2", "SEYSSEL …
#> $ ts_production_location_name        <chr> "FRANCE", "France", "FRANCE", "France", "France", "FRANCE", "FRANCE", "FRAN…
#> $ type                               <chr> "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80"…
#> $ type_def                           <chr> "Generation unavailability", "Generation unavailability", "Generation unava…
#> $ process_type                       <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26"…
#> $ process_type_def                   <chr> "Outage information", "Outage information", "Outage information", "Outage i…
#> $ ts_business_type                   <chr> "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53"…
#> $ ts_business_type_def               <chr> "Planned maintenance", "Planned maintenance", "Planned maintenance", "Plann…
#> $ ts_production_psr_type             <chr> "B12", "B11", "B14", "B11", "B11", "B14", "B14", "B14", "B14", "B14", "B11"…
#> $ ts_production_psr_type_def         <chr> "Hydro-electric storage head installation", "Hydro Run-of-river head instal…
#> $ created_date_time                  <dttm> 2025-10-08 01:34:11, 2026-03-20 10:35:02, 2025-11-10 15:09:07, 2026-03-26 …
#> $ reason_code                        <chr> "B19", "B19", "A95", "B19", "B19", "A95", "A95", "A95", "A95", "A95", "B19"…
#> $ reason_text                        <chr> "Foreseen maintenance", "Foreseen maintenance", "After its restart, the pla…
#> $ revision_number                    <dbl> 5, 10, 26, 5, 5, 8, 7, 11, 12, 10, 2, 5, 4, 9, 5, 3, 3, 11, 2, 7, 7, 3, 3, …
#> $ unavailability_time_interval_start <dttm> 2025-04-25 15:00:00, 2025-08-26 05:00:00, 2025-10-31 21:25:00, 2025-11-24 …
#> $ unavailability_time_interval_end   <dttm> 2026-04-24 15:00:00, 2026-05-22 15:00:00, 2026-06-17 20:30:00, 2026-06-30 …
#> $ ts_available_period_resolution     <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT…
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_production_psr_nominal_p        <dbl> 182.0, 45.0, 1330.0, 30.9, 13.5, 1330.0, 915.0, 1300.0, 1500.0, 890.0, 35.0…
#> $ ts_available_period_point_quantity <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"…
#> $ doc_status_value                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ doc_status                         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
```
