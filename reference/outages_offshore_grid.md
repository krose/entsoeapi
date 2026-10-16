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
#> → https://web-api.tp.entsoe.eu/api?documentType=A79&biddingZone_Domain=10Y1001A1001A82H&periodStart=202609152200&periodEnd=202610152200&securityToken=<...>
#> <- HTTP/1.1 200 OK
#> <- Date: Fri, 16 Oct 2026 07:46:17 GMT
#> <- Content-Type: application/zip
#> <- Transfer-Encoding: chunked
#> <- Connection: keep-alive
#> <- Content-Disposition: attachment; filename="Unavailability_of_offshore_grid_202609160027-202610141820.zip"
#> <- Strict-Transport-Security: max-age=15724800
#> <- Vary: accept-encoding
#> <- X-Content-Type-Options: nosniff
#> <- X-Xss-Protection: 0
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpPrwxUI/unzipped_19391ccc31c6/001-UNAVAILABILITY_OF_OFFSHORE_GRID_202609160027-202609171817.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19391ccc31c6/002-UNAVAILABILITY_OF_OFFSHORE_GRID_202609160027-202609171817.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19391ccc31c6/003-UNAVAILABILITY_OF_OFFSHORE_GRID_202609160027-202609171819.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19391ccc31c6/004-UNAVAILABILITY_OF_OFFSHORE_GRID_202609160027-202609171823.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19391ccc31c6/005-UNAVAILABILITY_OF_OFFSHORE_GRID_202609160027-202609171824.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19391ccc31c6/006-UNAVAILABILITY_OF_OFFSHORE_GRID_202609160027-202609171830.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19391ccc31c6/007-UNAVAILABILITY_OF_OFFSHORE_GRID_202609161313-202609161716.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19391ccc31c6/008-UNAVAILABILITY_OF_OFFSHORE_GRID_202609161313-202609161716.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19391ccc31c6/009-UNAVAILABILITY_OF_OFFSHORE_GRID_202609161313-202609161717.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19391ccc31c6/010-UNAVAILABILITY_OF_OFFSHORE_GRID_202609161313-202609161731.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19391ccc31c6/011-UNAVAILABILITY_OF_OFFSHORE_GRID_202609161313-202609161731.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19391ccc31c6/012-UNAVAILABILITY_OF_OFFSHORE_GRID_202610141629-202610141820.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19391ccc31c6/013-UNAVAILABILITY_OF_OFFSHORE_GRID_202610141629-202610141820.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19391ccc31c6/014-UNAVAILABILITY_OF_OFFSHORE_GRID_202610141629-202610141820.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19391ccc31c6/015-UNAVAILABILITY_OF_OFFSHORE_GRID_202610141630-202610141820.xml has been read in
#> ✔ /tmp/RtmpPrwxUI/unzipped_19391ccc31c6/016-UNAVAILABILITY_OF_OFFSHORE_GRID_202610141630-202610141820.xml has been read in
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
#> Rows: 16,887
#> Columns: 27
#> $ ts_bidding_zone_domain_mrid        <chr> "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A8…
#> $ ts_bidding_zone_domain_name        <chr> "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Lux…
#> $ ts_asset_location_name             <chr> "BORWIN BETA-DIELE", "BORWIN BETA-DIELE", "BORWIN BETA-DIELE", "BORWIN BETA…
#> $ ts_asset_mrid                      <chr> "11TD2L000000266Q", "11TD2L000000266Q", "11TD2L000000266Q", "11TD2L00000026…
#> $ ts_asset_name                      <chr> "TTG/600/BORWIN BETA-DIELE/DC601", "TTG/600/BORWIN BETA-DIELE/DC601", "TTG/…
#> $ type                               <chr> "A79", "A79", "A79", "A79", "A79", "A79", "A79", "A79", "A79", "A79", "A79"…
#> $ type_def                           <chr> "Offshore grid infrastructure unavailability", "Offshore grid infrastructur…
#> $ process_type                       <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26"…
#> $ process_type_def                   <chr> "Outage information", "Outage information", "Outage information", "Outage i…
#> $ ts_business_type                   <chr> "A54", "A54", "A54", "A54", "A54", "A54", "A54", "A54", "A54", "A54", "A54"…
#> $ ts_business_type_def               <chr> "Unplanned outage", "Unplanned outage", "Unplanned outage", "Unplanned outa…
#> $ ts_asset_psr_type                  <chr> "B22", "B22", "B22", "B22", "B22", "B22", "B22", "B22", "B22", "B22", "B22"…
#> $ ts_asset_psr_type_def              <chr> "DC Link", "DC Link", "DC Link", "DC Link", "DC Link", "DC Link", "DC Link"…
#> $ created_date_time                  <dttm> 2026-09-18 05:13:44, 2026-09-18 05:13:44, 2026-09-18 05:13:44, 2026-09-18 …
#> $ reason_code                        <chr> "B18", "B18", "B18", "B18", "B18", "B18", "B18", "B18", "B18", "B18", "B18"…
#> $ reason_text                        <chr> " - Failure", " - Failure", " - Failure", " - Failure", " - Failure", " - F…
#> $ revision_number                    <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …
#> $ unavailability_time_interval_start <dttm> 2026-09-16 00:27:00, 2026-09-16 00:27:00, 2026-09-16 00:27:00, 2026-09-16 …
#> $ unavailability_time_interval_end   <dttm> 2026-09-17 18:17:00, 2026-09-17 18:17:00, 2026-09-17 18:17:00, 2026-09-17 …
#> $ ts_resolution                      <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT…
#> $ ts_time_interval_start             <dttm> 2026-09-16 00:27:00, 2026-09-16 00:27:00, 2026-09-16 00:27:00, 2026-09-16 …
#> $ ts_time_interval_end               <dttm> 2026-09-17 18:17:00, 2026-09-17 18:17:00, 2026-09-17 18:17:00, 2026-09-17 …
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_point_dt_start                  <dttm> 2026-09-16 00:27:00, 2026-09-16 00:28:00, 2026-09-16 00:29:00, 2026-09-16 …
#> $ ts_production_psr_nominal_p        <dbl> 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, …
#> $ ts_point_quantity                  <dbl> 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, …
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"…
```
