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
#> → https://web-api.tp.entsoe.eu/api?documentType=A79&biddingZone_Domain=10Y1001A1001A82H&periodStart=202609192200&periodEnd=202610192200&securityToken=<...>
#> <- HTTP/1.1 200 OK
#> <- Date: Tue, 20 Oct 2026 10:26:31 GMT
#> <- Content-Type: application/zip
#> <- Transfer-Encoding: chunked
#> <- Connection: keep-alive
#> <- Content-Disposition: attachment; filename="Unavailability_of_offshore_grid_202610141629-202610141820.zip"
#> <- Strict-Transport-Security: max-age=15724800
#> <- Vary: accept-encoding
#> <- X-Content-Type-Options: nosniff
#> <- X-Xss-Protection: 0
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpAUwD2J/unzipped_18816847061f/001-UNAVAILABILITY_OF_OFFSHORE_GRID_202610141629-202610141820.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18816847061f/002-UNAVAILABILITY_OF_OFFSHORE_GRID_202610141629-202610141820.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18816847061f/003-UNAVAILABILITY_OF_OFFSHORE_GRID_202610141629-202610141820.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18816847061f/004-UNAVAILABILITY_OF_OFFSHORE_GRID_202610141630-202610141820.xml has been read in
#> ✔ /tmp/RtmpAUwD2J/unzipped_18816847061f/005-UNAVAILABILITY_OF_OFFSHORE_GRID_202610141630-202610141820.xml has been read in
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
#> Rows: 553
#> Columns: 27
#> $ ts_bidding_zone_domain_mrid        <chr> "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A8…
#> $ ts_bidding_zone_domain_name        <chr> "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Lux…
#> $ ts_asset_location_name             <chr> "DOERPEN W-DOLWIN ALPHA", "DOERPEN W-DOLWIN ALPHA", "DOERPEN W-DOLWIN ALPHA…
#> $ ts_asset_mrid                      <chr> "11TD2L0000003071", "11TD2L0000003071", "11TD2L0000003071", "11TD2L00000030…
#> $ ts_asset_name                      <chr> "TTG/600/DOERPEN W-DOLWIN ALPHA/DC602", "TTG/600/DOERPEN W-DOLWIN ALPHA/DC6…
#> $ type                               <chr> "A79", "A79", "A79", "A79", "A79", "A79", "A79", "A79", "A79", "A79", "A79"…
#> $ type_def                           <chr> "Offshore grid infrastructure unavailability", "Offshore grid infrastructur…
#> $ process_type                       <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26"…
#> $ process_type_def                   <chr> "Outage information", "Outage information", "Outage information", "Outage i…
#> $ ts_business_type                   <chr> "A54", "A54", "A54", "A54", "A54", "A54", "A54", "A54", "A54", "A54", "A54"…
#> $ ts_business_type_def               <chr> "Unplanned outage", "Unplanned outage", "Unplanned outage", "Unplanned outa…
#> $ ts_asset_psr_type                  <chr> "B22", "B22", "B22", "B22", "B22", "B22", "B22", "B22", "B22", "B22", "B22"…
#> $ ts_asset_psr_type_def              <chr> "DC Link", "DC Link", "DC Link", "DC Link", "DC Link", "DC Link", "DC Link"…
#> $ created_date_time                  <dttm> 2026-10-14 23:28:12, 2026-10-14 23:28:12, 2026-10-14 23:28:12, 2026-10-14 …
#> $ reason_code                        <chr> "B18", "B18", "B18", "B18", "B18", "B18", "B18", "B18", "B18", "B18", "B18"…
#> $ reason_text                        <chr> " - Failure", " - Failure", " - Failure", " - Failure", " - Failure", " - F…
#> $ revision_number                    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ unavailability_time_interval_start <dttm> 2026-10-14 16:29:00, 2026-10-14 16:29:00, 2026-10-14 16:29:00, 2026-10-14 …
#> $ unavailability_time_interval_end   <dttm> 2026-10-14 18:20:00, 2026-10-14 18:20:00, 2026-10-14 18:20:00, 2026-10-14 …
#> $ ts_resolution                      <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT…
#> $ ts_time_interval_start             <dttm> 2026-10-14 16:29:00, 2026-10-14 16:29:00, 2026-10-14 16:29:00, 2026-10-14 …
#> $ ts_time_interval_end               <dttm> 2026-10-14 18:20:00, 2026-10-14 18:20:00, 2026-10-14 18:20:00, 2026-10-14 …
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_point_dt_start                  <dttm> 2026-10-14 16:29:00, 2026-10-14 16:30:00, 2026-10-14 16:31:00, 2026-10-14 …
#> $ ts_production_psr_nominal_p        <dbl> 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, …
#> $ ts_point_quantity                  <dbl> 214.27, 214.27, 214.27, 214.27, 214.27, 214.27, 214.27, 214.27, 214.27, 214…
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"…
```
