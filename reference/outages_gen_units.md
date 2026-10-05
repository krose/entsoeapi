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
#> → https://web-api.tp.entsoe.eu/api?documentType=A80&biddingZone_Domain=10YFR-RTE------C&periodStart=202610052200&periodEnd=202610062200&securityToken=<...>
#> <- HTTP/1.1 200 OK
#> <- Date: Mon, 05 Oct 2026 12:44:57 GMT
#> <- Content-Type: application/zip
#> <- Transfer-Encoding: chunked
#> <- Connection: keep-alive
#> <- Content-Disposition: attachment; filename="Unavailability_of_production_and_generation_units_201803250000-209912310100.zip"
#> <- Strict-Transport-Security: max-age=15724800
#> <- Vary: accept-encoding
#> <- X-Content-Type-Options: nosniff
#> <- X-Xss-Protection: 0
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_201803250000-203408312200.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202002220100-209912310100.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202006292130-209912310100.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202103312200-202712312300.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601302300-202610062100.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230500-202611121600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/007-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230700-202611071500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/008-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603020600-202610091500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/009-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603100600-202610211400.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/010-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202611201600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/011-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202712171600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/012-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603312200-202611022300.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/013-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010545-202707021500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/014-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604060545-202707021500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/015-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604060545-202710011500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/016-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604060545-202710011500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/017-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202610190500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/018-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202611201600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/019-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070545-202710311600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/020-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604200500-202611201600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/021-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604241500-202703261600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/022-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202605180500-202611271600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/023-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202605290500-202705111500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/024-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606122200-202611242300.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/025-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606150600-202610260700.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/026-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606152200-202702122300.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/027-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606192100-202701252200.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/028-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606290500-202611131600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/029-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606290500-202611131600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/030-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606290500-202804251500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/031-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202607102145-202610152200.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/032-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202607152200-202701042300.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/033-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202607242215-202611012300.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/034-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202607312000-202610192000.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/035-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608011930-202610152200.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/036-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608030500-202611201600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/037-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608072200-202611122300.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/038-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608081038-202702122300.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/039-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608082200-202610220645.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/040-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608152200-202610090645.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/041-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608170500-202610091500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/042-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608170530-202610091430.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/043-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608202200-202610061800.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/044-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608212200-202611152300.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/045-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608240500-202610161500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/046-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608282200-202611262300.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/047-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608282200-202612032300.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/048-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608310500-202610161500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/049-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608310500-202610301600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/050-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608310600-202610301600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/051-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608312200-209912302300.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/052-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609022200-202610090700.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/053-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609040500-202610161500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/054-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609040500-202610161500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/055-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609070500-202611041600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/056-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609070530-202611131530.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/057-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609111500-202610161500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/058-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609111500-202610161500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/059-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609112200-202610172200.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/060-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609140530-202712311600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/061-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609171500-202612191600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/062-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609182100-202611102200.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/063-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609182100-202612172200.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/064-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609182100-202612172300.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/065-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609182200-202701062300.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/066-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609202200-202610122200.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/067-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609210500-202610091500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/068-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609210500-202704161500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/069-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609210530-202610161500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/070-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609251600-202610161600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/071-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609251600-202610161600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/072-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609252100-202611072230.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/073-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609252200-202610160600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/074-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609252200-202709102200.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/075-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609280500-202610091500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/076-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609280500-202610161500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/077-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609280500-202611031600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/078-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609280545-202610161000.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/079-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609280545-202610161000.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/080-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609280600-202610161600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/081-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610022200-202610312300.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/082-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610030700-202610082200.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/083-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610040056-202610070600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/084-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610041200-202610142200.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/085-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610041200-202610142200.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/086-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610042200-202610062200.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/087-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610050400-202611031600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/088-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610050500-202610091500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/089-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610050500-202610101600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/090-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610050500-202610161500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/091-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610050500-202610161500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/092-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610050500-202610161500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/093-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610050500-202610161500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/094-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610050500-202610161500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/095-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610050500-202610161500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/096-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610050500-202610191500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/097-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610050500-202610301600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/098-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610050500-202610301600.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/099-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610060400-202610060800.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/100-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610060400-202610061000.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/101-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610060400-202610061100.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/102-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610060500-202610061500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/103-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610060500-202610061500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/104-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610060500-202610061500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/105-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610060530-202610061500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/106-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610060530-202610061500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/107-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610060530-202610061500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/108-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610060530-202610061500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/109-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610060600-202610061000.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/110-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610060600-202610061000.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/111-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610060600-202610061000.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/112-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610060930-202610061000.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/113-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610061100-202610061500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/114-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610061200-202610061500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/115-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512312300-202612312300.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/116-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604230700-202612181530.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/117-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202605040530-202610301530.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/118-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606241300-202711011400.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/119-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608160401-202701282300.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/120-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609241356-202610161500.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/121-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609290948-202610152200.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/122-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609290948-202610152200.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/123-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609300608-202610092100.xml has been read in
#> ✔ /tmp/RtmpJhTgYk/unzipped_1ac9541eba6f/124-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609301830-202610312300.xml has been read in
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
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■                          22% | ETA:  4s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!

dplyr::glimpse(df)
#> Rows: 124
#> Columns: 28
#> $ ts_bidding_zone_domain_mrid        <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE-----…
#> $ ts_bidding_zone_domain_name        <chr> "France", "France", "France", "France", "France", "France", "France", "Fran…
#> $ ts_production_mrid                 <chr> "17W100P100P0352E", "17W100P100P0207N", "17W100P100P0208L", "17W100P100P023…
#> $ ts_production_name                 <chr> "CYCOFOS TV2", "FESSENHEIM 1", "FESSENHEIM 2", "HAVRE 4", "CATTENOM 4", "BE…
#> $ ts_production_psr_mrid             <chr> "17W100P100P03396", "17W100P100P0124R", "17W100P100P0125P", "17W100P100P002…
#> $ ts_production_psr_name             <chr> "CYCOFOS PL2", "FESSENHEIM 1", "FESSENHEIM 2", "HAVRE 4", "CATTENOM 4", "BE…
#> $ doc_status_value                   <chr> "A09", "A09", "A09", "A09", NA, NA, NA, NA, "A09", NA, NA, NA, "A09", "A09"…
#> $ doc_status                         <chr> "Finalised schedule", "Finalised schedule", "Finalised schedule", "Finalise…
#> $ ts_production_location_name        <chr> "France", "FRANCE", "FRANCE", "FRANCE", "FRANCE", "France", "FRANCE", "FRAN…
#> $ type                               <chr> "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80"…
#> $ type_def                           <chr> "Generation unavailability", "Generation unavailability", "Generation unava…
#> $ process_type                       <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26"…
#> $ process_type_def                   <chr> "Outage information", "Outage information", "Outage information", "Outage i…
#> $ ts_business_type                   <chr> "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53"…
#> $ ts_business_type_def               <chr> "Planned maintenance", "Planned maintenance", "Planned maintenance", "Plann…
#> $ ts_production_psr_type             <chr> "B20", "B14", "B14", "B05", "B14", "B11", "B11", "B12", "B11", "B11", "B11"…
#> $ ts_production_psr_type_def         <chr> "Other unspecified", "Nuclear unspecified", "Nuclear unspecified", "Fossil …
#> $ created_date_time                  <dttm> 2025-10-08 01:24:29, 2025-10-07 03:23:00, 2025-10-07 03:23:01, 2025-10-07 …
#> $ reason_code                        <chr> "A95", "A95", "A95", "B20", "A95", "B19", "B19", "B19", "B19", "B19", "B19"…
#> $ reason_text                        <chr> "Awaiting information - Complementary information", "For more information p…
#> $ revision_number                    <dbl> 256, 26, 10, 2, 28, 4, 5, 3, 5, 2, 2, 4, 2, 3, 2, 1, 4, 2, 3, 1, 9, 1, 5, 4…
#> $ unavailability_time_interval_start <dttm> 2018-03-25 00:00:00, 2020-02-22 01:00:00, 2020-06-29 21:30:00, 2021-03-31 …
#> $ unavailability_time_interval_end   <dttm> 2034-08-31 22:00:00, 2099-12-31 01:00:00, 2099-12-31 01:00:00, 2027-12-31 …
#> $ ts_available_period_resolution     <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT…
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_production_psr_nominal_p        <dbl> 62.0, 880.0, 880.0, 580.0, 1300.0, 35.0, 106.0, 240.0, 106.0, 20.0, 69.8, 5…
#> $ ts_available_period_point_quantity <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.0…
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"…
```
