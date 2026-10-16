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
#> → https://web-api.tp.entsoe.eu/api?documentType=A80&biddingZone_Domain=10YFR-RTE------C&periodStart=202610162200&periodEnd=202610172200&securityToken=<...>
#> <- HTTP/1.1 200 OK
#> <- Date: Fri, 16 Oct 2026 07:45:31 GMT
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
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_201803250000-203408312200.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202002220100-209912310100.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202006292130-209912310100.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202103312200-202712312300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601302300-202610172100.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230500-202611161600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/007-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230700-202611071500.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/008-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603100600-202610211400.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/009-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202611201600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/010-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202712171600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/011-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603312200-202611022300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/012-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010545-202707021500.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/013-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604060545-202707021500.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/014-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604060545-202710011500.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/015-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604060545-202710011500.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/016-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202610190500.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/017-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202611201600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/018-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070545-202710311600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/019-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604200500-202611201600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/020-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604241500-202703261600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/021-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202605180500-202611271600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/022-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202605290500-202705111500.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/023-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606122200-202611242300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/024-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606150600-202610260700.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/025-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606152200-202702122300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/026-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606192100-202701252200.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/027-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606290500-202611181600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/028-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606290500-202611181600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/029-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606290500-202804251500.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/030-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202607102145-202610292300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/031-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202607152200-202701042300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/032-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202607242215-202611012300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/033-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202607312000-202610232100.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/034-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608011930-202610292300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/035-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608030500-202612041600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/036-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608072200-202611122300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/037-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608082200-202610220645.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/038-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608212200-202611152300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/039-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608240500-202610301600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/040-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608282200-202611262300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/041-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608282200-202612032300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/042-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608310500-202610190500.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/043-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608310500-202610301600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/044-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608310600-202611131600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/045-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608312200-209912302300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/046-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609070500-202611041600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/047-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609070530-202611181530.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/048-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609112200-202610312300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/049-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609140530-202712311600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/050-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609171500-202612191600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/051-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609182100-202611102200.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/052-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609182100-202612172200.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/053-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609182100-202612172300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/054-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609182200-202701062300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/055-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609210500-202704161500.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/056-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609252100-202611072230.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/057-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609252200-202612030800.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/058-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609252200-202709102200.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/059-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609280500-202611031600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/060-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610022200-202610312300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/061-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610050400-202611031600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/062-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610050500-202610221500.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/063-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610050500-202610301600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/064-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610050500-202610301600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/065-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610091500-202610301600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/066-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610120500-202610231500.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/067-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610120500-202611161600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/068-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610121000-202612312300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/069-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610121000-202612312300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/070-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610140230-202610221330.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/071-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610142200-202612312300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/072-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610142200-202612312300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/073-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610161300-202610202100.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/074-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610161500-202610231500.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/075-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610161500-202611031700.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/076-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610161600-202611061700.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/077-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610162130-202611252230.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/078-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610170000-202612010100.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/079-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610170100-202610170200.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/080-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610170100-202610170230.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/081-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610170730-202610170930.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/082-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610170730-202610171630.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/083-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610170800-202610171545.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/084-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610170800-202610171545.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/085-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512312300-202612312300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/086-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604230700-202612011530.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/087-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202605040530-202611021530.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/088-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606241300-202711011400.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/089-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608160401-202701282300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/090-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609241356-202610201500.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/091-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609301830-202610312300.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/092-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610111319-202610171800.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/093-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610132305-202610170000.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/094-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610150654-202610271600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19397819322e/095-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610160740-202610170740.xml has been read in
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
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   99% | ETA:  0s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   99% | ETA:  0s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   99% | ETA:  0s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   99% | ETA:  0s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A77&biddingZone_Domain=10YFR-RTE------C&periodStart=202610162200&periodEnd=202610172200&securityToken=<...>
#> <- HTTP/1.1 200 OK
#> <- Date: Fri, 16 Oct 2026 07:45:41 GMT
#> <- Content-Type: application/zip
#> <- Transfer-Encoding: chunked
#> <- Connection: keep-alive
#> <- Content-Disposition: attachment; filename="Unavailability_of_production_and_generation_units_202606150600-202611131600.zip"
#> <- Strict-Transport-Security: max-age=15724800
#> <- Vary: accept-encoding
#> <- X-Content-Type-Options: nosniff
#> <- X-Xss-Protection: 0
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpPrwxUI/unzipped_1939334d9024/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606150600-202610260700.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_1939334d9024/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202607142200-202610240959.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_1939334d9024/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202607220500-202611131600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_1939334d9024/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610021500-202610260600.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_1939334d9024/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610120500-202610231500.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_1939334d9024/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610051350-202610201200.xml has been read in
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
#> Rows: 101
#> Columns: 28
#> $ ts_bidding_zone_domain_mrid        <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE-----…
#> $ ts_bidding_zone_domain_name        <chr> "France", "France", "France", "France", "France", "France", "France", "Fran…
#> $ ts_production_mrid                 <chr> "17W100P100P0352E", "17W100P100P0207N", "17W100P100P0208L", "17W100P100P023…
#> $ ts_production_name                 <chr> "CYCOFOS TV2", "FESSENHEIM 1", "FESSENHEIM 2", "HAVRE 4", "CATTENOM 4", "BE…
#> $ ts_production_psr_mrid             <chr> "17W100P100P03396", "17W100P100P0124R", "17W100P100P0125P", "17W100P100P002…
#> $ ts_production_psr_name             <chr> "CYCOFOS PL2", "FESSENHEIM 1", "FESSENHEIM 2", "HAVRE 4", "CATTENOM 4", "BE…
#> $ doc_status_value                   <chr> "A09", "A09", "A09", "A09", NA, NA, NA, "A09", NA, NA, NA, "A09", "A09", NA…
#> $ doc_status                         <chr> "Finalised schedule", "Finalised schedule", "Finalised schedule", "Finalise…
#> $ ts_production_location_name        <chr> "France", "FRANCE", "FRANCE", "FRANCE", "FRANCE", "France", "FRANCE", "FRAN…
#> $ type                               <chr> "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80"…
#> $ type_def                           <chr> "Generation unavailability", "Generation unavailability", "Generation unava…
#> $ process_type                       <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26"…
#> $ process_type_def                   <chr> "Outage information", "Outage information", "Outage information", "Outage i…
#> $ ts_business_type                   <chr> "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53"…
#> $ ts_business_type_def               <chr> "Planned maintenance", "Planned maintenance", "Planned maintenance", "Plann…
#> $ ts_production_psr_type             <chr> "B20", "B14", "B14", "B05", "B14", "B11", "B11", "B11", "B11", "B11", "B05"…
#> $ ts_production_psr_type_def         <chr> "Other unspecified", "Nuclear unspecified", "Nuclear unspecified", "Fossil …
#> $ created_date_time                  <dttm> 2025-04-10 07:45:20, 2023-11-21 15:05:05, 2023-11-21 15:10:12, 2023-11-21 …
#> $ reason_code                        <chr> "A95", "A95", "A95", "B20", "A95", "B19", "B19", "B19", "B19", "B19", "A95"…
#> $ reason_text                        <chr> "Awaiting information - Complementary information", "For more information p…
#> $ revision_number                    <dbl> 256, 26, 10, 2, 34, 5, 5, 5, 2, 2, 4, 2, 3, 2, 1, 4, 2, 3, 1, 9, 1, 5, 4, 5…
#> $ unavailability_time_interval_start <dttm> 2018-03-25 00:00:00, 2020-02-22 01:00:00, 2020-06-29 21:30:00, 2021-03-31 …
#> $ unavailability_time_interval_end   <dttm> 2034-08-31 22:00:00, 2099-12-31 01:00:00, 2099-12-31 01:00:00, 2027-12-31 …
#> $ ts_available_period_resolution     <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT…
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_production_psr_nominal_p        <dbl> 62.0, 880.0, 880.0, 580.0, 1300.0, 35.0, 106.0, 106.0, 20.0, 69.8, 580.0, 3…
#> $ ts_available_period_point_quantity <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.0…
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"…
```
