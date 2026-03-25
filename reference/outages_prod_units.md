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
#> → https://web-api.tp.entsoe.eu/api?documentType=A77&biddingZone_Domain=10YFR-RTE------C&periodStart=202603252300&periodEnd=202603262300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 25 Mar 2026 19:17:08 GMT
#> <- content-type: application/zip
#> <- content-length: 9841
#> <- content-disposition: attachment; filename="Unavailability_of_production_and_generation_units_202504090830-202607311500.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpF6x61X/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230600-202604101500.xml has been read in
#> ✔ /tmp/RtmpF6x61X/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230700-202605110600.xml has been read in
#> ✔ /tmp/RtmpF6x61X/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603230800-202603261600.xml has been read in
#> ✔ /tmp/RtmpF6x61X/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603260600-202603261600.xml has been read in
#> ✔ /tmp/RtmpF6x61X/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603260700-202603261600.xml has been read in
#> ✔ /tmp/RtmpF6x61X/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603260700-202603261600.xml has been read in
#> ✔ /tmp/RtmpF6x61X/007-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603260700-202603261700.xml has been read in
#> ✔ /tmp/RtmpF6x61X/008-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603260730-202603261600.xml has been read in
#> ✔ /tmp/RtmpF6x61X/009-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202504090830-202607311500.xml has been read in
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
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
#> Rows: 9
#> Columns: 26
#> $ ts_bidding_zone_domain_mrid        <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE-----…
#> $ ts_bidding_zone_domain_name        <chr> "France", "France", "France", "France", "France", "France", "France", "Fran…
#> $ ts_production_mrid                 <chr> "17W100P100P02853", "17W100P100P02934", "17W100P100P0317G", "17W100P100P028…
#> $ ts_production_name                 <chr> "MALGOVERT", "SARRANS", "SAINT PIERRE COGNET", "COMBE D'AVRIEUX", "COCHE", …
#> $ ts_production_location_name        <chr> "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRAN…
#> $ type                               <chr> "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77"
#> $ type_def                           <chr> "Production unavailability", "Production unavailability", "Production unava…
#> $ process_type                       <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26"
#> $ process_type_def                   <chr> "Outage information", "Outage information", "Outage information", "Outage i…
#> $ ts_business_type                   <chr> "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A54"
#> $ ts_business_type_def               <chr> "Planned maintenance", "Planned maintenance", "Planned maintenance", "Plann…
#> $ ts_production_psr_type             <chr> "B12", "B12", "B11", "B12", "B10", "B12", "B12", "B12", "B12"
#> $ ts_production_psr_type_def         <chr> "Hydro-electric storage head installation", "Hydro-electric storage head in…
#> $ created_date_time                  <dttm> 2025-10-07 07:40:17, 2026-03-09 07:49:32, 2026-03-04 10:20:20, 2025-12-17 0…
#> $ reason_code                        <chr> "B19", "B19", "B19", "A95", "A95", "B19", "B19", "A95", "B18"
#> $ reason_text                        <chr> "Foreseen maintenance", "L'indisponibilité prévue n'aura pas lieu du 23/03/…
#> $ revision_number                    <dbl> 1, 3, 1, 2, 1, 4, 2, 1, 4
#> $ unavailability_time_interval_start <dttm> 2026-03-23 06:00:00, 2026-03-23 07:00:00, 2026-03-23 08:00:00, 2026-03-26 0…
#> $ unavailability_time_interval_end   <dttm> 2026-04-10 15:00:00, 2026-05-11 06:00:00, 2026-03-26 16:00:00, 2026-03-26 1…
#> $ ts_available_period_resolution     <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M"
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_production_psr_nominal_p        <dbl> 297, 183, 101, 123, 384, 360, 360, 123, 360
#> $ ts_available_period_point_quantity <dbl> 166, 0, 0, 0, 0, 260, 260, 0, 262
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"
#> $ doc_status_value                   <chr> NA, "A09", NA, "A09", NA, "A09", "A09", NA, NA
#> $ doc_status                         <chr> NA, "Finalised schedule", NA, "Finalised schedule", NA, "Finalised schedul…
```
