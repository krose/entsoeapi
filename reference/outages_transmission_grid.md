# Get Unavailability of Transmission Infrastructure. (10.1.A&B)

The planned and forced unavailability, including changes in
unavailability of interconnections in the transmission grid that reduce
transfer capacities between areas during at least one market time unit
including information about new net transfer capacity.

## Usage

``` r
outages_transmission_grid(
  eic_in = NULL,
  eic_out = NULL,
  period_start = lubridate::ymd(Sys.Date() + lubridate::days(x = 1L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date() + lubridate::days(x = 3L), tz = "CET"),
  period_start_update = NULL,
  period_end_update = NULL,
  doc_status = NULL,
  event_nature = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic_in:

  Energy Identification Code of the IN bidding zone area

- eic_out:

  Energy Identification Code of the OUT bidding zone area

- period_start:

  the starting date of the in-scope period in POSIXct or YYYY-MM-DD
  HH:MM:SS format One year range limit applies

- period_end:

  the ending date of the outage in-scope period in POSIXct or YYYY-MM-DD
  HH:MM:SS format One year range limit applies

- period_start_update:

  notification submission/update starting date in POSIXct or YYYY-MM-DD
  HH:MM:SS format

- period_end_update:

  notification submission/update ending date in POSIXct or YYYY-MM-DD
  HH:MM:SS format

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
df <- entsoeapi::outages_transmission_grid(
  eic_in = "10YFR-RTE------C",
  eic_out = "10Y1001A1001A82H",
  period_start = lubridate::ymd(
    x = Sys.Date() + lubridate::days(x = 1),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    x = Sys.Date() + lubridate::days(x = 2),
    tz = "CET"
  ),
  period_start_update = lubridate::ymd(
    x = Sys.Date() - lubridate::days(x = 7),
    tz = "CET"
  ),
  period_end_update = lubridate::ymd(x = Sys.Date(), tz = "CET")
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A78&in_Domain=10YFR-RTE------C&out_domain=10Y1001A1001A82H&periodStart=202603102300&periodEnd=202603112300&periodStartUpdate=202603022300&periodEndUpdate=202603092300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 10 Mar 2026 19:49:33 GMT
#> <- content-type: application/zip
#> <- content-length: 6188
#> <- content-disposition: attachment; filename="Unavailability_in_the_Transmission_Grid_202602020630-202604151500.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpDj0v66/001-UNAVAILABILITY_IN_TRANSMISSION_GRID_202602020630-202603201600.xml has been read in
#> ✔ /tmp/RtmpDj0v66/002-UNAVAILABILITY_IN_TRANSMISSION_GRID_202603090630-202603131600.xml has been read in
#> ✔ /tmp/RtmpDj0v66/003-UNAVAILABILITY_IN_TRANSMISSION_GRID_202603090630-202603131600.xml has been read in
#> ✔ /tmp/RtmpDj0v66/004-UNAVAILABILITY_IN_TRANSMISSION_GRID_202603090630-202604151500.xml has been read in
#> ✔ /tmp/RtmpDj0v66/005-UNAVAILABILITY_IN_TRANSMISSION_GRID_202603110630-202603111600.xml has been read in
#> ✔ /tmp/RtmpDj0v66/006-UNAVAILABILITY_IN_TRANSMISSION_GRID_202603110630-202603111600.xml has been read in

dplyr::glimpse(df)
#> Rows: 16
#> Columns: 25
#> $ ts_in_domain_mrid                  <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE-----…
#> $ ts_in_domain_name                  <chr> "France", "France", "France", "France", "France", "France", "France", "Fran…
#> $ ts_out_domain_mrid                 <chr> "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A8…
#> $ ts_out_domain_name                 <chr> "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Lux…
#> $ ts_asset_location_name             <chr> "intra-zonal", "intra-zonal", "intra-zonal", "intra-zonal", "intra-zonal", …
#> $ ts_asset_mrid                      <chr> "17T-FR-000000309", "17T-FR-000000309", "17T-FR-000000309", "17T-FR-0000003…
#> $ ts_asset_name                      <chr> "L 400kV N0 2 MOULAINE-VIGY", "L 400kV N0 2 MOULAINE-VIGY", "L 400kV N0 2 M…
#> $ type                               <chr> "A78", "A78", "A78", "A78", "A78", "A78", "A78", "A78", "A78", "A78", "A78"…
#> $ type_def                           <chr> "Transmission unavailability", "Transmission unavailability", "Transmission…
#> $ process_type                       <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26"…
#> $ process_type_def                   <chr> "Outage information", "Outage information", "Outage information", "Outage i…
#> $ ts_business_type                   <chr> "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53", "A53"…
#> $ ts_business_type_def               <chr> "Planned maintenance", "Planned maintenance", "Planned maintenance", "Plann…
#> $ ts_asset_psr_type                  <chr> "B21", "B21", "B21", "B21", "B21", "B21", "B21", "B21", "B21", "B21", "B21"…
#> $ ts_asset_psr_type_def              <chr> "AC Link", "AC Link", "AC Link", "AC Link", "AC Link", "AC Link", "AC Link"…
#> $ created_date_time                  <dttm> 2026-03-05 15:55:39, 2026-03-05 15:55:39, 2026-03-05 15:55:39, 2026-03-05 …
#> $ reason_code                        <chr> "B19", "B19", "B19", "B19", "B19", "B19", "B19", "B19", "B19", "B19", "B19"…
#> $ reason_text                        <chr> "Foreseen maintenance", "Foreseen maintenance", "Foreseen maintenance", "Fo…
#> $ revision_number                    <dbl> 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3
#> $ unavailability_time_interval_start <dttm> 2026-02-02 06:30:00, 2026-02-02 06:30:00, 2026-02-02 06:30:00, 2026-02-02 0…
#> $ unavailability_time_interval_end   <dttm> 2026-03-20 16:00:00, 2026-03-20 16:00:00, 2026-03-20 16:00:00, 2026-03-20 …
#> $ ts_available_period_resolution     <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "P…
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_available_period_point_quantity <dbl> 3000, 3200, 3000, 4000, 6000, 3700, 1200, 3700, 3700, 3700, 1200, 3550, 430…
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"…
```
