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
#> → https://web-api.tp.entsoe.eu/api?documentType=A77&biddingZone_Domain=10YFR-RTE------C&periodStart=202603092300&periodEnd=202603102300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 09 Mar 2026 20:05:45 GMT
#> <- content-type: application/zip
#> <- content-length: 12784
#> <- content-disposition: attachment; filename="Unavailability_of_production_and_generation_units_202504090830-202607311500.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/Rtmp1NLnYK/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603090600-202603131600.xml has been read in
#> ✔ /tmp/Rtmp1NLnYK/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603100600-202603101600.xml has been read in
#> ✔ /tmp/Rtmp1NLnYK/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603100600-202603101600.xml has been read in
#> ✔ /tmp/Rtmp1NLnYK/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603100600-202603111600.xml has been read in
#> ✔ /tmp/Rtmp1NLnYK/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603100700-202603101100.xml has been read in
#> ✔ /tmp/Rtmp1NLnYK/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603100700-202603101500.xml has been read in
#> ✔ /tmp/Rtmp1NLnYK/007-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603100700-202603101600.xml has been read in
#> ✔ /tmp/Rtmp1NLnYK/008-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603100800-202603101600.xml has been read in
#> ✔ /tmp/Rtmp1NLnYK/009-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603100830-202603101030.xml has been read in
#> ✔ /tmp/Rtmp1NLnYK/010-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603100900-202603101100.xml has been read in
#> ✔ /tmp/Rtmp1NLnYK/011-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202504090830-202607311500.xml has been read in
#> ✔ /tmp/Rtmp1NLnYK/012-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603090925-202603101700.xml has been read in

dplyr::glimpse(df)
#> Rows: 12
#> Columns: 26
#> $ ts_bidding_zone_domain_mrid        <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE-----…
#> $ ts_bidding_zone_domain_name        <chr> "France", "France", "France", "France", "France", "France", "France", "Fran…
#> $ ts_production_mrid                 <chr> "17W100P100P0312Q", "17W100P100P02837", "17W100P100P02748", "17W100P100P031…
#> $ ts_production_name                 <chr> "RANDENS", "COMBE D'AVRIEUX", "COCHE", "SAINT PIERRE COGNET", "SARRANS", "R…
#> $ ts_production_location_name        <chr> "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRANCE", "FRAN…
#> $ type                               <chr> "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77", "A77"…
#> $ type_def                           <chr> "Production unavailability", "Production unavailability", "Production unava…
#> $ process_type                       <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26"…
#> $ process_type_def                   <chr> "Outage information", "Outage information", "Outage information", "Outage i…
#> $ ts_business_type                   <chr> "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A54"…
#> $ ts_business_type_def               <chr> "Planned maintenance", "Planned maintenance", "Planned maintenance", "Plann…
#> $ ts_production_psr_type             <chr> "B11", "B12", "B10", "B11", "B12", "B11", "B12", "B20", "B12", "B11", "B12"…
#> $ ts_production_psr_type_def         <chr> "Hydro Run-of-river head installation", "Hydro-electric storage head instal…
#> $ created_date_time                  <dttm> 2025-10-07 23:59:23, 2025-12-17 08:52:27, 2026-01-12 14:53:21, 2026-03-04 …
#> $ reason_code                        <chr> "B19", "A95", "A95", "B19", "A95", "A95", "A95", "B19", "B19", "B19", "B18"…
#> $ reason_text                        <chr> "Foreseen maintenance", "Des variations de puissance pour essais sont possi…
#> $ revision_number                    <dbl> 1, 2, 2, 2, 1, 1, 1, 1, 1, 2, 4, 1
#> $ unavailability_time_interval_start <dttm> 2026-03-09 06:00:00, 2026-03-10 06:00:00, 2026-03-10 06:00:00, 2026-03-10 0…
#> $ unavailability_time_interval_end   <dttm> 2026-03-13 16:00:00, 2026-03-10 16:00:00, 2026-03-10 16:00:00, 2026-03-11 …
#> $ ts_available_period_resolution     <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "P…
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_production_psr_nominal_p        <dbl> 124.0, 123.0, 384.0, 101.0, 183.0, 124.0, 189.2, 105.0, 600.0, 150.0, 360.0…
#> $ ts_available_period_point_quantity <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 178, 0, 262, 0
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"…
#> $ doc_status_value                   <chr> NA, NA, NA, "A09", NA, NA, NA, NA, NA, NA, NA, NA
#> $ doc_status                         <chr> NA, NA, NA, "Finalised schedule", NA, NA, NA, NA, NA, NA, NA, NA
```
