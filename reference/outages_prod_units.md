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
#> <- date: Mon, 13 Apr 2026 08:12:57 GMT
#> <- content-type: application/zip
#> <- content-length: 24736
#> <- content-disposition: attachment; filename="Unavailability_of_production_and_generation_units_202504090830-202607311500.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpptwzjC/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230700-202605110600.xml has been read in
#> ✔ /tmp/RtmpptwzjC/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202604241500.xml has been read in
#> ✔ /tmp/RtmpptwzjC/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603300500-202605221500.xml has been read in
#> ✔ /tmp/RtmpptwzjC/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070430-202604171500.xml has been read in
#> ✔ /tmp/RtmpptwzjC/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604070500-202605071500.xml has been read in
#> ✔ /tmp/RtmpptwzjC/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604071800-202604171500.xml has been read in
#> ✔ /tmp/RtmpptwzjC/007-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604130600-202604141800.xml has been read in
#> ✔ /tmp/RtmpptwzjC/008-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604130600-202606010600.xml has been read in
#> ✔ /tmp/RtmpptwzjC/009-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604130600-202606010600.xml has been read in
#> ✔ /tmp/RtmpptwzjC/010-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604131000-202604141500.xml has been read in
#> ✔ /tmp/RtmpptwzjC/011-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604140300-202604141900.xml has been read in
#> ✔ /tmp/RtmpptwzjC/012-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604140500-202604151500.xml has been read in
#> ✔ /tmp/RtmpptwzjC/013-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604140530-202604141430.xml has been read in
#> ✔ /tmp/RtmpptwzjC/014-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604140530-202604141500.xml has been read in
#> ✔ /tmp/RtmpptwzjC/015-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604140530-202604141500.xml has been read in
#> ✔ /tmp/RtmpptwzjC/016-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604140600-202604141400.xml has been read in
#> ✔ /tmp/RtmpptwzjC/017-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604140600-202604141500.xml has been read in
#> ✔ /tmp/RtmpptwzjC/018-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604140600-202604141500.xml has been read in
#> ✔ /tmp/RtmpptwzjC/019-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604140600-202604141600.xml has been read in
#> ✔ /tmp/RtmpptwzjC/020-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604141130-202604141400.xml has been read in
#> ✔ /tmp/RtmpptwzjC/021-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604141500-202604150600.xml has been read in
#> ✔ /tmp/RtmpptwzjC/022-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202604141900-202604150530.xml has been read in
#> ✔ /tmp/RtmpptwzjC/023-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202504090830-202607311500.xml has been read in
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
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
#> $ ts_production_mrid                 <chr> "17W100P100P02934", "17W100P100P0300X", "17W100P100P0280D", "17W100P100P028…
#> $ ts_production_name                 <chr> "SARRANS", "VILLARODIN", "BATHIE", "MALGOVERT", "MALGOVERT", "ORAISON", "BE…
#> $ doc_status_value                   <chr> "A09", NA, "A09", NA, NA, NA, NA, "A09", "A09", NA, "A09", NA, "A09", "A09"…
#> $ doc_status                         <chr> "Finalised schedule", NA, "Finalised schedule", NA, NA, NA, NA, "Finalised …
#> $ ts_production_location_name        <chr> "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRAN…
#> $ type                               <chr> "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77"…
#> $ type_def                           <chr> "Production unavailability", "Production unavailability", "Production unava…
#> $ process_type                       <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26"…
#> $ process_type_def                   <chr> "Outage information", "Outage information", "Outage information", "Outage i…
#> $ ts_business_type                   <chr> "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53"…
#> $ ts_business_type_def               <chr> "Planned maintenance", "Planned maintenance", "Planned maintenance", "Plann…
#> $ ts_production_psr_type             <chr> "B12", "B12", "B12", "B12", "B12", "B12", "B20", "B12", "B12", "B11", "B12"…
#> $ ts_production_psr_type_def         <chr> "Hydro-electric storage head installation", "Hydro-electric storage head in…
#> $ created_date_time                  <dttm> 2026-03-09 07:49:32, 2025-10-07 20:55:18, 2025-10-07 22:00:10, 2026-04-01 …
#> $ reason_code                        <chr> "B19", "B19", "B19", "B19", "B19", "B19", "B19", "B19", "B19", "B19", "B19"…
#> $ reason_text                        <chr> "L'indisponibilité prévue n'aura pas lieu du 23/03/2026 08:00 au 11/05/2026…
#> $ revision_number                    <dbl> 3, 5, 2, 5, 6, 4, 1, 3, 2, 1, 3, 2, 2, 2, 2, 1, 2, 4, 1, 2, 2, 2, 4
#> $ unavailability_time_interval_start <dttm> 2026-03-23 07:00:00, 2026-03-30 05:00:00, 2026-03-30 05:00:00, 2026-04-07 0…
#> $ unavailability_time_interval_end   <dttm> 2026-05-11 06:00:00, 2026-04-24 15:00:00, 2026-05-22 15:00:00, 2026-04-17 …
#> $ ts_available_period_resolution     <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "P…
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_production_psr_nominal_p        <dbl> 183.0, 364.0, 600.0, 297.0, 297.0, 187.5, 105.0, 406.0, 183.0, 104.0, 384.0…
#> $ ts_available_period_point_quantity <dbl> 0.00, 0.00, 0.00, 0.00, 166.00, 111.17, 0.00, 240.00, 0.00, 0.00, 0.00, 0.0…
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"…
```
