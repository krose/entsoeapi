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
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A78&in_Domain=10YFR-RTE------C&out_domain=10Y1001A1001A82H&periodStart=202603042300&periodEnd=202603052300&periodStartUpdate=202602242300&periodEndUpdate=202603032300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:14:20 GMT
#> <- content-type: application/zip
#> <- content-length: 4124
#> <- content-disposition: attachment; filename="Unavailability_in_the_Transmission_Grid_202602020630-202603201600.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpSo3AKG/001-UNAVAILABILITY_IN_TRANSMISSION_GRID_202602020630-202603201600.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/002-UNAVAILABILITY_IN_TRANSMISSION_GRID_202602230630-202603061600.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/003-UNAVAILABILITY_IN_TRANSMISSION_GRID_202603020700-202603061600.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/004-UNAVAILABILITY_IN_TRANSMISSION_GRID_202603050630-202603051600.xml has been read in

str(df)
#> tibble [10 × 25] (S3: tbl_df/tbl/data.frame)
#>  $ ts_in_domain_mrid                 : chr [1:10] "10YFR-RTE------C" "10YFR-RTE------C" "10YFR-RTE------C" "10YFR-RTE------C" ...
#>  $ ts_in_domain_name                 : chr [1:10] "France" "France" "France" "France" ...
#>  $ ts_out_domain_mrid                : chr [1:10] "10Y1001A1001A82H" "10Y1001A1001A82H" "10Y1001A1001A82H" "10Y1001A1001A82H" ...
#>  $ ts_out_domain_name                : chr [1:10] "Germany_Luxemburg" "Germany_Luxemburg" "Germany_Luxemburg" "Germany_Luxemburg" ...
#>  $ ts_asset_location_name            : chr [1:10] "intra-zonal" "intra-zonal" "intra-zonal" "intra-zonal" ...
#>  $ ts_asset_mrid                     : chr [1:10] "17T-FR-000000309" "17T-FR-000000309" "17T-FR-000000309" "17T-FR-000000309" ...
#>  $ ts_asset_name                     : chr [1:10] "L 400kV N0 2 MOULAINE-VIGY" "L 400kV N0 2 MOULAINE-VIGY" "L 400kV N0 2 MOULAINE-VIGY" "L 400kV N0 2 MOULAINE-VIGY" ...
#>  $ type                              : chr [1:10] "A78" "A78" "A78" "A78" ...
#>  $ type_def                          : chr [1:10] "Transmission unavailability" "Transmission unavailability" "Transmission unavailability" "Transmission unavailability" ...
#>  $ process_type                      : chr [1:10] "A26" "A26" "A26" "A26" ...
#>  $ process_type_def                  : chr [1:10] "Outage information" "Outage information" "Outage information" "Outage information" ...
#>  $ ts_business_type                  : chr [1:10] "A53" "A53" "A53" "A53" ...
#>  $ ts_business_type_def              : chr [1:10] "Planned maintenance" "Planned maintenance" "Planned maintenance" "Planned maintenance" ...
#>  $ ts_asset_psr_type                 : chr [1:10] "B21" "B21" "B21" "B21" ...
#>  $ ts_asset_psr_type_def             : chr [1:10] "AC Link" "AC Link" "AC Link" "AC Link" ...
#>  $ created_date_time                 : POSIXct[1:10], format: "2026-02-26 15:54:24" "2026-02-26 15:54:24" ...
#>  $ reason_code                       : chr [1:10] "B19" "B19" "B19" "B19" ...
#>  $ reason_text                       : chr [1:10] "Foreseen maintenance" "Foreseen maintenance" "Foreseen maintenance" "Foreseen maintenance" ...
#>  $ revision_number                   : num [1:10] 1 1 1 1 1 1 4 4 3 3
#>  $ unavailability_time_interval_start: POSIXct[1:10], format: "2026-02-02 06:30:00" "2026-02-02 06:30:00" ...
#>  $ unavailability_time_interval_end  : POSIXct[1:10], format: "2026-03-20 16:00:00" "2026-03-20 16:00:00" ...
#>  $ ts_available_period_resolution    : chr [1:10] "PT1M" "PT1M" "PT1M" "PT1M" ...
#>  $ ts_mrid                           : num [1:10] 1 1 1 1 1 1 1 1 1 1
#>  $ ts_available_period_point_quantity: num [1:10] 3000 3200 3000 4000 6000 1200 4000 6000 6000 6000
#>  $ ts_quantity_measure_unit_name     : chr [1:10] "MAW" "MAW" "MAW" "MAW" ...
```
