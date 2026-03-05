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
#> → https://web-api.tp.entsoe.eu/api?documentType=A77&biddingZone_Domain=10YFR-RTE------C&periodStart=202603052300&periodEnd=202603062300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 05 Mar 2026 16:13:27 GMT
#> <- content-type: application/zip
#> <- content-length: 9608
#> <- content-disposition: attachment; filename="Unavailability_of_production_and_generation_units_202504090830-202607311500.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpksiKAO/001-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603052300-202603060700.xml has been read in
#> ✔ /tmp/RtmpksiKAO/002-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603052300-202603060700.xml has been read in
#> ✔ /tmp/RtmpksiKAO/003-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603060630-202603061600.xml has been read in
#> ✔ /tmp/RtmpksiKAO/004-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603060630-202603061600.xml has been read in
#> ✔ /tmp/RtmpksiKAO/005-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603060700-202603061600.xml has been read in
#> ✔ /tmp/RtmpksiKAO/006-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603060700-202603061600.xml has been read in
#> ✔ /tmp/RtmpksiKAO/007-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603060700-202603061600.xml has been read in
#> ✔ /tmp/RtmpksiKAO/008-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202603060700-202603061700.xml has been read in
#> ✔ /tmp/RtmpksiKAO/009-UNAVAILABILITY_OF_PRODUCTION_AND_GENERATION_UNITS_202504090830-202607311500.xml has been read in

str(df)
#> tibble [9 × 26] (S3: tbl_df/tbl/data.frame)
#>  $ ts_bidding_zone_domain_mrid       : chr [1:9] "10YFR-RTE------C" "10YFR-RTE------C" "10YFR-RTE------C" "10YFR-RTE------C" ...
#>  $ ts_bidding_zone_domain_name       : chr [1:9] "France" "France" "France" "France" ...
#>  $ ts_production_mrid                : chr [1:9] "17W100P100P02942" "17W100P100P02926" "17W100P100P02926" "17W100P100P02942" ...
#>  $ ts_production_name                : chr [1:9] "SISTERON" "SERRE PONCON" "SERRE PONCON" "SISTERON" ...
#>  $ ts_production_location_name       : chr [1:9] "FRANCE" "FRANCE" "FRANCE" "FRANCE" ...
#>  $ type                              : chr [1:9] "A77" "A77" "A77" "A77" ...
#>  $ type_def                          : chr [1:9] "Production unavailability" "Production unavailability" "Production unavailability" "Production unavailability" ...
#>  $ process_type                      : chr [1:9] "A26" "A26" "A26" "A26" ...
#>  $ process_type_def                  : chr [1:9] "Outage information" "Outage information" "Outage information" "Outage information" ...
#>  $ ts_business_type                  : chr [1:9] "A53" "A53" "A53" "A53" ...
#>  $ ts_business_type_def              : chr [1:9] "Planned maintenance" "Planned maintenance" "Planned maintenance" "Planned maintenance" ...
#>  $ ts_production_psr_type            : chr [1:9] "B12" "B12" "B12" "B12" ...
#>  $ ts_production_psr_type_def        : chr [1:9] "Hydro-electric storage head installation" "Hydro-electric storage head installation" "Hydro-electric storage head installation" "Hydro-electric storage head installation" ...
#>  $ created_date_time                 : POSIXct[1:9], format: "2026-02-18 10:49:18" "2026-02-23 15:13:20" "2026-02-18 10:05:27" "2026-02-18 10:06:28" ...
#>  $ reason_code                       : chr [1:9] "B19" "B19" "B19" "B19" ...
#>  $ reason_text                       : chr [1:9] "Foreseen maintenance" "Foreseen maintenance" "L'indisponibilité prévue n'aura pas lieu du 06/03/2026 07:30 au 06/03/2026 17:00 - Foreseen maintenance" "L'indisponibilité prévue n'aura pas lieu du 06/03/2026 07:30 au 06/03/2026 17:00 - Foreseen maintenance" ...
#>  $ revision_number                   : num [1:9] 1 2 3 3 2 1 2 2 4
#>  $ unavailability_time_interval_start: POSIXct[1:9], format: "2026-03-05 23:00:00" "2026-03-05 23:00:00" "2026-03-06 06:30:00" "2026-03-06 06:30:00" ...
#>  $ unavailability_time_interval_end  : POSIXct[1:9], format: "2026-03-06 07:00:00" "2026-03-06 07:00:00" "2026-03-06 16:00:00" "2026-03-06 16:00:00" ...
#>  $ ts_available_period_resolution    : chr [1:9] "PT1M" "PT1M" "PT1M" "PT1M" ...
#>  $ ts_mrid                           : num [1:9] 1 1 1 1 1 1 1 1 1
#>  $ ts_production_psr_nominal_p       : num [1:9] 240 384 384 240 384 384 240 240 360
#>  $ ts_available_period_point_quantity: num [1:9] 124 111 111 117 0 111 124 117 262
#>  $ ts_quantity_measure_unit_name     : chr [1:9] "MAW" "MAW" "MAW" "MAW" ...
#>  $ doc_status_value                  : chr [1:9] NA NA "A09" "A09" ...
#>  $ doc_status                        : chr [1:9] NA NA "Finalised schedule" "Finalised schedule" ...
```
