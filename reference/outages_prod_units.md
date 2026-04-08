# Get Unavailability of Production Units. (15.1.C&D)

The planned and forced unavailability of production units expected to
last at least one market time unit up to 3 years ahead. The "available
capacity during the event" means the minimum available generation
capacity during the period specified.

## Usage

``` r
outages_prod_units(
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
[`outages_both()`](https://krose.github.io/entsoeapi/reference/outages_both.md),
[`outages_cons_units()`](https://krose.github.io/entsoeapi/reference/outages_cons_units.md),
[`outages_fallbacks()`](https://krose.github.io/entsoeapi/reference/outages_fallbacks.md),
[`outages_gen_units()`](https://krose.github.io/entsoeapi/reference/outages_gen_units.md),
[`outages_offshore_grid()`](https://krose.github.io/entsoeapi/reference/outages_offshore_grid.md),
[`outages_transmission_grid()`](https://krose.github.io/entsoeapi/reference/outages_transmission_grid.md)

## Examples

``` r
df <- entsoeapi::outages_prod_units(
  eic = "10YFR-RTE------C",
  period_start = lubridate::ymd(
    x = Sys.Date() +
      lubridate::days(x = 1L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    x = Sys.Date() +
      lubridate::days(x = 2L),
    tz = "CET"
  )
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A77&biddingZone_Domain=10YFR-RTE------C&periodStart=202604082200&periodEnd=202604092200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 08 Apr 2026 13:15:25 GMT
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
#> Rows: 27
#> Columns: 26
#> $ ts_bidding_zone_domain_mrid        <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE-----…
#> $ ts_bidding_zone_domain_name        <chr> "France", "France", "France", "France", "France", "France", "France", "Fran…
#> $ ts_production_mrid                 <chr> "17W100P100P02853", "17W100P100P0300X", "17W000000989066M", "17W100P100P028…
#> $ ts_production_name                 <chr> "MALGOVERT", "VILLARODIN", "FR_CPCUCOGEVI_GP", "MALGOVERT", "MALGOVERT", "O…
#> $ ts_production_location_name        <chr> "FRANCE", "FRANCE", "France", "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRAN…
#> $ type                               <chr> "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77"…
#> $ type_def                           <chr> "Production unavailability", "Production unavailability", "Production unava…
#> $ process_type                       <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26"…
#> $ process_type_def                   <chr> "Outage information", "Outage information", "Outage information", "Outage i…
#> $ ts_business_type                   <chr> "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53"…
#> $ ts_business_type_def               <chr> "Planned maintenance", "Planned maintenance", "Planned maintenance", "Plann…
#> $ ts_production_psr_type             <chr> "B12", "B12", "B04", "B12", "B12", "B12", "B12", "B12", "B12", "B12", "B12"…
#> $ ts_production_psr_type_def         <chr> "Hydro-electric storage head installation", "Hydro-electric storage head in…
#> $ created_date_time                  <dttm> 2025-10-07 07:40:17, 2025-10-07 20:55:18, 2026-03-23 16:19:24, 2026-04-01 …
#> $ reason_code                        <chr> "B19", "B19", "B19", "B19", "B19", "B19", "A95", "B19", "B19", "B19", "B19"…
#> $ reason_text                        <chr> "Foreseen maintenance", "Foreseen maintenance", "Foreseen maintenance", "Fo…
#> $ revision_number                    <dbl> 1, 5, 1, 5, 6, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 3, 2, 2, 2, 2, 2, 2, 2, 2, …
#> $ unavailability_time_interval_start <dttm> 2026-03-23 06:00:00, 2026-03-30 05:00:00, 2026-04-07 00:01:00, 2026-04-07 …
#> $ unavailability_time_interval_end   <dttm> 2026-04-10 15:00:00, 2026-04-24 15:00:00, 2026-04-09 23:59:00, 2026-04-17 …
#> $ ts_available_period_resolution     <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT…
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_production_psr_nominal_p        <dbl> 297.0, 364.0, 125.0, 297.0, 297.0, 187.5, 384.0, 131.8, 360.0, 103.3, 146.0…
#> $ ts_available_period_point_quantity <dbl> 166.00, 0.00, 0.00, 0.00, 166.00, 111.17, 194.00, 0.00, 260.00, 0.00, 0.00,…
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"…
#> $ doc_status_value                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "A09", "A09…
#> $ doc_status                         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Finalised …
```
