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
#> → https://web-api.tp.entsoe.eu/api?documentType=A77&biddingZone_Domain=10YFR-RTE------C&periodStart=202604132200&periodEnd=202604142200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:52:23 GMT
#> <- content-type: application/zip
#> <- content-length: 24736
#> <- content-disposition: attachment; filename="Unavailability_of_production_and_generation_units_202504090830-202607311500.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpB1aCB1/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604241500.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070430-202604171500.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202605071500.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604071800-202604171500.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604130600-202604141800.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604131000-202604141500.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/007-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604140500-202604151500.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/008-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604140600-202604141400.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/009-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604140600-202604141600.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/010-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604141130-202604141400.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/011-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202504090830-202607311500.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/012-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230700-202605110600.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/013-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202605221500.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/014-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604130600-202606010600.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/015-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604130600-202606010600.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/016-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604140300-202604141900.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/017-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604140530-202604141430.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/018-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604140530-202604141500.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/019-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604140530-202604141500.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/020-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604140600-202604141500.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/021-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604140600-202604141500.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/022-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604141500-202604150600.xml has been read in
#> ✔ /tmp/RtmpB1aCB1/023-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604141900-202604150530.xml has been read in
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
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
#> Rows: 23
#> Columns: 26
#> $ ts_bidding_zone_domain_mrid        <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE-----…
#> $ ts_bidding_zone_domain_name        <chr> "France", "France", "France", "France", "France", "France", "France", "Fran…
#> $ ts_production_mrid                 <chr> "17W100P100P0300X", "17W100P100P02853", "17W100P100P02853", "17W100P100P028…
#> $ ts_production_name                 <chr> "VILLARODIN", "MALGOVERT", "MALGOVERT", "ORAISON", "BESS_AFD7_BARBAN_SAUCAT…
#> $ ts_production_location_name        <chr> "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRAN…
#> $ type                               <chr> "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77"…
#> $ type_def                           <chr> "Production unavailability", "Production unavailability", "Production unava…
#> $ process_type                       <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26"…
#> $ process_type_def                   <chr> "Outage information", "Outage information", "Outage information", "Outage i…
#> $ ts_business_type                   <chr> "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A54"…
#> $ ts_business_type_def               <chr> "Planned maintenance", "Planned maintenance", "Planned maintenance", "Plann…
#> $ ts_production_psr_type             <chr> "B12", "B12", "B12", "B12", "B20", "B11", "B10", "B12", "B12", "B11", "B12"…
#> $ ts_production_psr_type_def         <chr> "Hydro-electric storage head installation", "Hydro-electric storage head in…
#> $ created_date_time                  <dttm> 2025-10-07 20:55:18, 2026-04-01 12:50:21, 2026-02-17 17:49:21, 2026-03-25 …
#> $ reason_code                        <chr> "B19", "B19", "B19", "B19", "B19", "B19", "B19", "B19", "B19", "B19", "B18"…
#> $ reason_text                        <chr> "Foreseen maintenance", "Foreseen maintenance", "Foreseen maintenance", "Fo…
#> $ revision_number                    <dbl> 5, 5, 6, 4, 1, 1, 2, 1, 1, 2, 4, 3, 2, 3, 2, 3, 2, 2, 2, 2, 4, 2, 2
#> $ unavailability_time_interval_start <dttm> 2026-03-30 05:00:00, 2026-04-07 04:30:00, 2026-04-07 05:00:00, 2026-04-07 1…
#> $ unavailability_time_interval_end   <dttm> 2026-04-24 15:00:00, 2026-04-17 15:00:00, 2026-05-07 15:00:00, 2026-04-17 …
#> $ ts_available_period_resolution     <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "P…
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_production_psr_nominal_p        <dbl> 364.0, 297.0, 297.0, 187.5, 105.0, 104.0, 800.0, 285.0, 360.0, 150.0, 360.0…
#> $ ts_available_period_point_quantity <dbl> 0.00, 0.00, 166.00, 111.17, 0.00, 0.00, 0.00, 142.00, 260.00, 0.00, 262.00,…
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"…
#> $ doc_status_value                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "A09", "A09", "A09", "A09", "A0…
#> $ doc_status                         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Finalised schedule", "Finalise…
```
