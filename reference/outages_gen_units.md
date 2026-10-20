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
#> → https://web-api.tp.entsoe.eu/api?documentType=A80&biddingZone_Domain=10YFR-RTE------C&periodStart=202610202200&periodEnd=202610212200&securityToken=<...>
#> <- HTTP/1.1 200 OK
#> <- Date: Tue, 20 Oct 2026 10:26:28 GMT
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
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202601302300-202610212100.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230500-202611271600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202602230700-202611300700.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202611201600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202712171600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603312200-202611022300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/007-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604060545-202710011500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/008-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604200500-202611201600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/009-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604241500-202703261600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/010-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202605180500-202611271600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/011-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202605290500-202705111500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/012-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606122200-202611242300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/013-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606192100-202701252200.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/014-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606290500-202611181600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/015-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606290500-202611181600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/016-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606290500-202804251500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/017-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202607102145-202610292300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/018-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202607152200-202701042300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/019-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202607242215-202611012300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/020-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202607312000-202610232100.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/021-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608011930-202610292300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/022-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608030500-202612041600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/023-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608072200-202611122300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/024-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608082200-202610220645.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/025-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608212200-202611152300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/026-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608240500-202610301600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/027-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608282200-202611262300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/028-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608282200-202612032300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/029-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608310500-202610301600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/030-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608310600-202611131600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/031-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608312200-209912302300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/032-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609070500-202611041600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/033-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609070530-202611181530.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/034-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609111500-202610221500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/035-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609111500-202610221500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/036-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609112200-202610312300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/037-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609140530-202712311600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/038-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609171500-202612191600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/039-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609182100-202611102200.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/040-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609182100-202612172300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/041-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609182200-202701062300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/042-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609210500-202704161500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/043-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609252100-202611072230.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/044-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609252200-202612030800.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/045-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609252200-202709102200.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/046-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609280500-202611031600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/047-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610022200-202610312300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/048-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610050400-202611031600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/049-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610050500-202610221500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/050-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610050500-202610301600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/051-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610050500-202610301600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/052-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610091500-202610301600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/053-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610120500-202610231500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/054-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610120500-202611161600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/055-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610121000-202612312300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/056-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610121000-202612312300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/057-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610140230-202610221330.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/058-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610142200-202612312300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/059-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610142200-202612312300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/060-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610161500-202610231500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/061-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610161500-202611031700.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/062-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610161600-202611061700.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/063-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610162055-202611252230.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/064-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610171530-202610202300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/065-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610172200-202610300745.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/066-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610190500-202610231600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/067-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610190500-202610231600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/068-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610190500-202610301600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/069-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610190500-202611201600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/070-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610190530-202610231500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/071-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610190530-202610231500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/072-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610190530-202610231500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/073-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610190530-202610231500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/074-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610190530-202610231500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/075-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610190530-202610231500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/076-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610191500-202611021400.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/077-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610202200-202610212200.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/078-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610202300-202610222300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/079-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610210130-202610210200.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/080-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610210530-202610211500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/081-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610210600-202610211300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/082-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610210600-202610211530.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/083-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610210600-202610231100.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/084-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610210630-202610211500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/085-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610210800-202610211545.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/086-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610210800-202610211545.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/087-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610211000-202610212030.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/088-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610211300-202610211600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/089-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202512312300-202612312300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/090-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604230700-202612011530.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/091-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202605040530-202611021530.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/092-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606241300-202711011400.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/093-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202608160401-202701282300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/094-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609241356-202610211500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/095-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609301830-202610312300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/096-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610132305-202610211000.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/097-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610150654-202610271600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/098-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_201803250000-203408312200.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/099-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202002220100-209912310100.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/100-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202006292130-209912310100.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/101-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202103312200-202712312300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/102-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603100600-202610211400.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/103-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604010545-202707021500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/104-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604060545-202707021500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/105-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604060545-202710011500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/106-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202611201600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/107-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070545-202710311600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/108-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606150600-202610260700.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/109-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606152200-202702122300.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/110-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202609182100-202612172200.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/111-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610170000-202612010100.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/112-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610210500-202610211500.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/113-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610210600-202610211600.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/114-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610211100-202610211530.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18815b76c278/115-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610211400-202612011600.xml has been read in
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
#> Rows: 115
#> Columns: 28
#> $ ts_bidding_zone_domain_mrid        <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE-----…
#> $ ts_bidding_zone_domain_name        <chr> "France", "France", "France", "France", "France", "France", "France", "Fran…
#> $ ts_production_mrid                 <chr> "17W100P100P0190E", "17W100P100P0327D", "17W100P100P0302T", "17W00000101779…
#> $ ts_production_name                 <chr> "CATTENOM 4", "BEAUCAIRE", "CHASTANG", "PIERRE-BENITE", "GENISSIAT", "CORDE…
#> $ ts_production_psr_mrid             <chr> "17W100P100P0105V", "17W100P100P1332F", "17W100P100P0050U", "17W100P100P182…
#> $ ts_production_psr_name             <chr> "CATTENOM 4", "BEAUCAIRE G3", "CHASTANG 3", "PIERRE-BENITE G2", "GENISSIAT …
#> $ ts_production_location_name        <chr> "FRANCE", "France", "FRANCE", "France", "France", "FRANCE", "France", "Fran…
#> $ type                               <chr> "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80", "A80"…
#> $ type_def                           <chr> "Generation unavailability", "Generation unavailability", "Generation unava…
#> $ process_type                       <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26"…
#> $ process_type_def                   <chr> "Outage information", "Outage information", "Outage information", "Outage i…
#> $ ts_business_type                   <chr> "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53"…
#> $ ts_business_type_def               <chr> "Planned maintenance", "Planned maintenance", "Planned maintenance", "Plann…
#> $ ts_production_psr_type             <chr> "B14", "B11", "B11", "B11", "B11", "B05", "B11", "B11", "B12", "B11", "B11"…
#> $ ts_production_psr_type_def         <chr> "Nuclear unspecified", "Hydro Run-of-river head installation", "Hydro Run-o…
#> $ created_date_time                  <dttm> 2026-10-19 12:25:45, 2026-10-16 10:11:38, 2026-10-20 09:42:39, 2025-12-11 …
#> $ reason_code                        <chr> "A95", "B19", "B19", "B19", "B19", "A95", "B19", "B19", "B19", "B19", "B19"…
#> $ reason_text                        <chr> "Après son recouplage, la tranche remontera progressivement par palier de c…
#> $ revision_number                    <dbl> 37, 6, 6, 2, 2, 4, 2, 1, 9, 1, 5, 4, 11, 2, 2, 2, 43, 7, 2, 10, 30, 6, 7, 8…
#> $ unavailability_time_interval_start <dttm> 2026-01-30 23:00:00, 2026-02-23 05:00:00, 2026-02-23 07:00:00, 2026-03-30 …
#> $ unavailability_time_interval_end   <dttm> 2026-10-21 21:00:00, 2026-11-27 16:00:00, 2026-11-30 07:00:00, 2026-11-20 …
#> $ ts_available_period_resolution     <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT…
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_production_psr_nominal_p        <dbl> 1300.0, 35.0, 106.0, 20.0, 69.8, 580.0, 37.5, 32.0, 182.0, 46.0, 58.4, 915.…
#> $ ts_available_period_point_quantity <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"…
#> $ doc_status_value                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ doc_status                         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
```
