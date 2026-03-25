# Get Unavailability of Offshore Grid Infrastructure. (10.1.C)

Unavailability of the off-shore grid that reduce wind power feed-in
during at least one market time unit. Wind power fed in at the time of
the change in the availability is provided.

## Usage

``` r
outages_offshore_grid(
  eic = NULL,
  period_start = ymd(Sys.Date() + days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date() + days(x = 2L), tz = "CET"),
  doc_status = NULL,
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

  Notification document status. NULL or "A05" for active and "A13" for
  withdrawn. Defaults to NULL.

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
[`outages_prod_units()`](https://krose.github.io/entsoeapi/reference/outages_prod_units.md),
[`outages_transmission_grid()`](https://krose.github.io/entsoeapi/reference/outages_transmission_grid.md)

## Examples

``` r
df <- entsoeapi::outages_offshore_grid(
  eic = "10Y1001A1001A82H",
  period_start = lubridate::ymd(
    x = Sys.Date() - lubridate::days(x = 30L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(x = Sys.Date(), tz = "CET")
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A79&biddingZone_Domain=10Y1001A1001A82H&periodStart=202602222300&periodEnd=202603242300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 25 Mar 2026 19:10:07 GMT
#> <- content-type: application/zip
#> <- content-length: 21607
#> <- content-disposition: attachment; filename="Unavailability_of_offshore_grid_202602221637-202606012159.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpADDhvc/001-UNAVAILABILITY_OF_OFFSHORE_GRID_202602221637-202603101912.xml has been read in
#> ✔ /tmp/RtmpADDhvc/002-UNAVAILABILITY_OF_OFFSHORE_GRID_202602221637-202603101912.xml has been read in
#> ✔ /tmp/RtmpADDhvc/003-UNAVAILABILITY_OF_OFFSHORE_GRID_202603031122-202606012159.xml has been read in
#> ✔ /tmp/RtmpADDhvc/004-UNAVAILABILITY_OF_OFFSHORE_GRID_202603111106-202603121730.xml has been read in
#> ✔ /tmp/RtmpADDhvc/005-UNAVAILABILITY_OF_OFFSHORE_GRID_202603121559-202603121828.xml has been read in
#> ✔ /tmp/RtmpADDhvc/006-UNAVAILABILITY_OF_OFFSHORE_GRID_202603121559-202603121828.xml has been read in
#> ✔ /tmp/RtmpADDhvc/007-UNAVAILABILITY_OF_OFFSHORE_GRID_202603121559-202603121828.xml has been read in
#> ✔ /tmp/RtmpADDhvc/008-UNAVAILABILITY_OF_OFFSHORE_GRID_202603121559-202603121840.xml has been read in
#> ✔ /tmp/RtmpADDhvc/009-UNAVAILABILITY_OF_OFFSHORE_GRID_202603121559-202603121840.xml has been read in
#> ✔ /tmp/RtmpADDhvc/010-UNAVAILABILITY_OF_OFFSHORE_GRID_202603121559-202603121840.xml has been read in
#> ✔ /tmp/RtmpADDhvc/011-UNAVAILABILITY_OF_OFFSHORE_GRID_202603121559-202603121840.xml has been read in
#> ✔ /tmp/RtmpADDhvc/012-UNAVAILABILITY_OF_OFFSHORE_GRID_202603151834-202603152208.xml has been read in
#> ✔ /tmp/RtmpADDhvc/013-UNAVAILABILITY_OF_OFFSHORE_GRID_202603151834-202603152208.xml has been read in
#> ✔ /tmp/RtmpADDhvc/014-UNAVAILABILITY_OF_OFFSHORE_GRID_202603151834-202603240917.xml has been read in
#> ✔ /tmp/RtmpADDhvc/015-UNAVAILABILITY_OF_OFFSHORE_GRID_202603151834-202603240917.xml has been read in
#> ✔ /tmp/RtmpADDhvc/016-UNAVAILABILITY_OF_OFFSHORE_GRID_202603241229-202603241242.xml has been read in
#> ✔ /tmp/RtmpADDhvc/017-UNAVAILABILITY_OF_OFFSHORE_GRID_202603241230-202603241242.xml has been read in
#> ✔ /tmp/RtmpADDhvc/018-UNAVAILABILITY_OF_OFFSHORE_GRID_202603241401-202603241439.xml has been read in
#> ✔ /tmp/RtmpADDhvc/019-UNAVAILABILITY_OF_OFFSHORE_GRID_202603241401-202603241439.xml has been read in
#> ✔ /tmp/RtmpADDhvc/020-UNAVAILABILITY_OF_OFFSHORE_GRID_202603241401-202603242201.xml has been read in
#> ✔ /tmp/RtmpADDhvc/021-UNAVAILABILITY_OF_OFFSHORE_GRID_202603241401-202603242230.xml has been read in
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■                           19% | ETA: 12s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■                           19% | ETA: 12s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■                           19% | ETA: 12s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■                           19% | ETA: 12s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■                           19% | ETA: 12s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■                           19% | ETA: 12s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■                           19% | ETA: 12s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■                           19% | ETA: 12s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■                           19% | ETA: 12s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■                           19% | ETA: 12s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■                           19% | ETA: 12s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■                           19% | ETA: 12s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■                           19% | ETA: 12s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■                           19% | ETA: 12s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■                           19% | ETA: 12s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■                           19% | ETA: 12s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■                           19% | ETA: 12s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■                           19% | ETA: 12s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■                           19% | ETA: 12s
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional type names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional eic names have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> ✔ Additional definitions have been added!
#> processing xml list ■■■■■■■■■■■■■■■                   48% | ETA:  4s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!

dplyr::glimpse(df)
#> Rows: 205,865
#> Columns: 27
#> $ ts_bidding_zone_domain_mrid        <chr> "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A8…
#> $ ts_bidding_zone_domain_name        <chr> "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Lux…
#> $ ts_asset_location_name             <chr> "BORWIN ALPHA-DIELE", "BORWIN ALPHA-DIELE", "BORWIN ALPHA-DIELE", "BORWIN A…
#> $ ts_asset_mrid                      <chr> "11TD2L000000263W", "11TD2L000000263W", "11TD2L000000263W", "11TD2L00000026…
#> $ ts_asset_name                      <chr> "TTG/300/BORWIN ALPHA-DIELE/DC31", "TTG/300/BORWIN ALPHA-DIELE/DC31", "TTG/…
#> $ type                               <chr> "A79", "A79", "A79", "A79", "A79", "A79", "A79", "A79", "A79", "A79", "A79"…
#> $ type_def                           <chr> "Offshore grid infrastructure unavailability", "Offshore grid infrastructur…
#> $ process_type                       <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26"…
#> $ process_type_def                   <chr> "Outage information", "Outage information", "Outage information", "Outage i…
#> $ ts_business_type                   <chr> "A54", "A54", "A54", "A54", "A54", "A54", "A54", "A54", "A54", "A54", "A54"…
#> $ ts_business_type_def               <chr> "Unplanned outage", "Unplanned outage", "Unplanned outage", "Unplanned outa…
#> $ ts_asset_psr_type                  <chr> "B22", "B22", "B22", "B22", "B22", "B22", "B22", "B22", "B22", "B22", "B22"…
#> $ ts_asset_psr_type_def              <chr> "DC Link", "DC Link", "DC Link", "DC Link", "DC Link", "DC Link", "DC Link"…
#> $ created_date_time                  <dttm> 2026-03-10 19:50:17, 2026-03-10 19:50:17, 2026-03-10 19:50:17, 2026-03-10 …
#> $ reason_code                        <chr> "B18", "B18", "B18", "B18", "B18", "B18", "B18", "B18", "B18", "B18", "B18"…
#> $ reason_text                        <chr> " - Failure", " - Failure", " - Failure", " - Failure", " - Failure", " - F…
#> $ revision_number                    <dbl> 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,…
#> $ unavailability_time_interval_start <dttm> 2026-02-22 16:37:00, 2026-02-22 16:37:00, 2026-02-22 16:37:00, 2026-02-22 …
#> $ unavailability_time_interval_end   <dttm> 2026-03-10 19:12:00, 2026-03-10 19:12:00, 2026-03-10 19:12:00, 2026-03-10 …
#> $ ts_resolution                      <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT…
#> $ ts_time_interval_start             <dttm> 2026-02-22 16:37:00, 2026-02-22 16:37:00, 2026-02-22 16:37:00, 2026-02-22 …
#> $ ts_time_interval_end               <dttm> 2026-03-10 19:12:00, 2026-03-10 19:12:00, 2026-03-10 19:12:00, 2026-03-10 …
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_point_dt_start                  <dttm> 2026-02-22 16:37:00, 2026-02-22 16:38:00, 2026-02-22 16:39:00, 2026-02-22 …
#> $ ts_production_psr_nominal_p        <dbl> 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, …
#> $ ts_point_quantity                  <dbl> 146.5, 146.5, 146.5, 146.5, 146.5, 146.5, 146.5, 146.5, 146.5, 146.5, 146.5…
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"…
```
