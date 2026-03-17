# Get Unavailability of Production Units. (15.1.C&D)

The planned and forced unavailability of production units expected to
last at least one market time unit up to 3 years ahead. The "available
capacity during the event" means the minimum available generation
capacity during the period specified.

## Usage

``` r
outages_prod_units(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() + lubridate::days(x = 1L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date() + lubridate::days(x = 2L), tz = "CET"),
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
with the queried data, or `NULL` if no data is available for the given
parameters.

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
#> → https://web-api.tp.entsoe.eu/api?documentType=A77&biddingZone_Domain=10YFR-RTE------C&periodStart=202603172300&periodEnd=202603182300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 17 Mar 2026 22:12:03 GMT
#> <- content-type: application/zip
#> <- content-length: 6465
#> <- content-disposition: attachment; filename="Unavailability_of_production_and_generation_units_202504090830-202607311500.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpOgumiW/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603180600-202603181300.xml has been read in
#> ✔ /tmp/RtmpOgumiW/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603180600-202603181600.xml has been read in
#> ✔ /tmp/RtmpOgumiW/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603180600-202603181630.xml has been read in
#> ✔ /tmp/RtmpOgumiW/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603180700-202603181100.xml has been read in
#> ✔ /tmp/RtmpOgumiW/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603181400-202603181500.xml has been read in
#> ✔ /tmp/RtmpOgumiW/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202504090830-202607311500.xml has been read in
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache

dplyr::glimpse(df)
#> Rows: 6
#> Columns: 26
#> $ ts_bidding_zone_domain_mrid        <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE-----…
#> $ ts_bidding_zone_domain_name        <chr> "France", "France", "France", "France", "France", "France"
#> $ ts_production_mrid                 <chr> "17W100P100P0297X", "17W100P100P02837", "17W100P100P02837", "17W100P100P030…
#> $ ts_production_name                 <chr> "SAINTE CROIX", "COMBE D'AVRIEUX", "COMBE D'AVRIEUX", "SAUSSAZ", "REVIN", "…
#> $ ts_production_location_name        <chr> "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRANCE"
#> $ type                               <chr> "A77", "A77", "A77", "A77", "A77", "A77"
#> $ type_def                           <chr> "Production unavailability", "Production unavailability", "Production unava…
#> $ process_type                       <chr> "A26", "A26", "A26", "A26", "A26", "A26"
#> $ process_type_def                   <chr> "Outage information", "Outage information", "Outage information", "Outage i…
#> $ ts_business_type                   <chr> "A53", "A53", "A53", "A53", "A53", "A54"
#> $ ts_business_type_def               <chr> "Planned maintenance", "Planned maintenance", "Planned maintenance", "Plann…
#> $ ts_production_psr_type             <chr> "B12", "B12", "B12", "B11", "B10", "B12"
#> $ ts_production_psr_type_def         <chr> "Hydro-electric storage head installation", "Hydro-electric storage head in…
#> $ created_date_time                  <dttm> 2026-03-17 14:53:34, 2026-03-10 07:37:42, 2026-03-16 15:30:30, 2026-03-17 1…
#> $ reason_code                        <chr> "A95", "A95", "A95", "B19", "B19", "B18"
#> $ reason_text                        <chr> "Des variations de puissance pour essais sont possibles --- Power variation…
#> $ revision_number                    <dbl> 4, 3, 5, 1, 1, 4
#> $ unavailability_time_interval_start <dttm> 2026-03-18 06:00:00, 2026-03-18 06:00:00, 2026-03-18 06:00:00, 2026-03-18 0…
#> $ unavailability_time_interval_end   <dttm> 2026-03-18 13:00:00, 2026-03-18 16:00:00, 2026-03-18 16:30:00, 2026-03-18 1…
#> $ ts_available_period_resolution     <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M"
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1
#> $ ts_production_psr_nominal_p        <dbl> 132.3, 123.0, 123.0, 150.0, 800.0, 360.0
#> $ ts_available_period_point_quantity <dbl> 0, 0, 0, 0, 0, 262
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"
#> $ doc_status_value                   <chr> NA, "A09", NA, NA, NA, NA
#> $ doc_status                         <chr> NA, "Finalised schedule", NA, NA, NA, NA
```
