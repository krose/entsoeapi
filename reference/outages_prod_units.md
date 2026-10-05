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
#> → https://web-api.tp.entsoe.eu/api?documentType=A77&biddingZone_Domain=10YFR-RTE------C&periodStart=202610052200&periodEnd=202610062200&securityToken=<...>
#> <- HTTP/1.1 200 OK
#> <- Date: Mon, 05 Oct 2026 13:01:25 GMT
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
#> ✔ /tmp/Rtmpso7PyT/unzipped_19fd2544a1d/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202606150600-202610260700.xml has been read in
#> ✔ /tmp/Rtmpso7PyT/unzipped_19fd2544a1d/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202607142200-202610202159.xml has been read in
#> ✔ /tmp/Rtmpso7PyT/unzipped_19fd2544a1d/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202607220500-202611131600.xml has been read in
#> ✔ /tmp/Rtmpso7PyT/unzipped_19fd2544a1d/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610020800-202610071500.xml has been read in
#> ✔ /tmp/Rtmpso7PyT/unzipped_19fd2544a1d/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610021500-202610260600.xml has been read in
#> ✔ /tmp/Rtmpso7PyT/unzipped_19fd2544a1d/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610060400-202610061500.xml has been read in
#> ✔ /tmp/Rtmpso7PyT/unzipped_19fd2544a1d/007-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610060500-202610061500.xml has been read in
#> ✔ /tmp/Rtmpso7PyT/unzipped_19fd2544a1d/008-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610060500-202610061500.xml has been read in
#> ✔ /tmp/Rtmpso7PyT/unzipped_19fd2544a1d/009-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610060500-202610161500.xml has been read in
#> ✔ /tmp/Rtmpso7PyT/unzipped_19fd2544a1d/010-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610060600-202610061000.xml has been read in
#> ✔ /tmp/Rtmpso7PyT/unzipped_19fd2544a1d/011-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610060600-202610061000.xml has been read in
#> ✔ /tmp/Rtmpso7PyT/unzipped_19fd2544a1d/012-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610060600-202610061600.xml has been read in
#> ✔ /tmp/Rtmpso7PyT/unzipped_19fd2544a1d/013-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610061200-202610061500.xml has been read in
#> ✔ /tmp/Rtmpso7PyT/unzipped_19fd2544a1d/014-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202610061200-202610061600.xml has been read in
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
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
#> Rows: 14
#> Columns: 26
#> $ ts_bidding_zone_domain_mrid        <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE-----…
#> $ ts_bidding_zone_domain_name        <chr> "France", "France", "France", "France", "France", "France", "France", "Fran…
#> $ ts_production_mrid                 <chr> "17W100P100P02829", "17W100P100P0344D", "17W100P100P02748", "17W100P100P029…
#> $ ts_production_name                 <chr> "BROMMAT", "SAINT AVOLD 7", "COCHE", "PRAGNERES", "COMBE D'AVRIEUX", "FECAM…
#> $ doc_status_value                   <chr> "A09", NA, NA, NA, NA, NA, NA, "A09", NA, NA, NA, NA, NA, "A09"
#> $ doc_status                         <chr> "Finalised schedule", NA, NA, NA, NA, NA, NA, "Finalised schedule", NA, NA,…
#> $ ts_production_location_name        <chr> "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRAN…
#> $ type                               <chr> "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77"…
#> $ type_def                           <chr> "Production unavailability", "Production unavailability", "Production unava…
#> $ process_type                       <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26"…
#> $ process_type_def                   <chr> "Outage information", "Outage information", "Outage information", "Outage i…
#> $ ts_business_type                   <chr> "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53"…
#> $ ts_business_type_def               <chr> "Planned maintenance", "Planned maintenance", "Planned maintenance", "Plann…
#> $ ts_production_psr_type             <chr> "B12", "B04", "B10", "B12", "B12", "B18", "B12", "B11", "B12", "B11", "B11"…
#> $ ts_production_psr_type_def         <chr> "Hydro-electric storage head installation", "Fossil Gas", "Hydro-electric p…
#> $ created_date_time                  <dttm> 2026-03-20 14:04:47, 2026-09-24 15:34:04, 2026-01-27 15:24:35, 2026-10-02 0…
#> $ reason_code                        <chr> "B19", "B19", "B19", "B19", "B19", "A95", "B19", "B19", "B19", "B19", "B19…
#> $ reason_text                        <chr> "L'indisponibilité prévue n'aura pas lieu du 15/06/2026 08:00 au 26/10/2026…
#> $ revision_number                    <dbl> 6, 1, 4, 1, 1, 2, 2, 2, 4, 1, 1, 1, 1, 2
#> $ unavailability_time_interval_start <dttm> 2026-06-15 06:00:00, 2026-07-14 22:00:00, 2026-07-22 05:00:00, 2026-10-02 0…
#> $ unavailability_time_interval_end   <dttm> 2026-10-26 07:00:00, 2026-10-20 21:59:00, 2026-11-13 16:00:00, 2026-10-07 1…
#> $ ts_available_period_resolution     <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "P…
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_production_psr_nominal_p        <dbl> 406.0, 435.0, 384.0, 189.2, 123.0, 497.0, 364.0, 150.0, 297.0, 150.0, 150.…
#> $ ts_available_period_point_quantity <dbl> 233.28, 0.00, 0.00, 0.00, 0.00, 384.00, 0.00, 0.00, 166.00, 0.00, 0.00, 0.0…
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"…
```
