# Get Unavailability of Consumption Units. (7.1.A&B)

Unavailability of consumption units in aggregated form. All planned and
forced outages in selected area are aggregated according to the market
time unit. The list of specific consumption units are not provided.

## Usage

``` r
outages_cons_units(
  eic = NULL,
  period_start = ymd(Sys.Date() + days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date() + days(x = 2L), tz = "CET"),
  period_start_update = NULL,
  period_end_update = NULL,
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

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with the queried data.

## See also

Other outage endpoints:
[`outages_both()`](https://krose.github.io/entsoeapi/reference/outages_both.md),
[`outages_fallbacks()`](https://krose.github.io/entsoeapi/reference/outages_fallbacks.md),
[`outages_gen_units()`](https://krose.github.io/entsoeapi/reference/outages_gen_units.md),
[`outages_offshore_grid()`](https://krose.github.io/entsoeapi/reference/outages_offshore_grid.md),
[`outages_prod_units()`](https://krose.github.io/entsoeapi/reference/outages_prod_units.md),
[`outages_transmission_grid()`](https://krose.github.io/entsoeapi/reference/outages_transmission_grid.md)

## Examples

``` r
df <- entsoeapi::outages_cons_units(
  eic = "10YFI-1--------U",
  period_start = lubridate::ymd(x = "2024-04-10", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-04-11", tz = "CET")
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A76&biddingZone_Domain=10YFI-1--------U&periodStart=202404092200&periodEnd=202404102200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 08 Apr 2026 13:09:04 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Unavailability_of_consumption_units_aggregated_202404100400-202404100900.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!

dplyr::glimpse(df)
#> Rows: 1
#> Columns: 18
#> $ ts_bidding_zone_domain_mrid        <chr> "10YFI-1--------U"
#> $ ts_bidding_zone_domain_name        <chr> "Finland"
#> $ type                               <chr> "A76"
#> $ type_def                           <chr> "Load unavailability"
#> $ process_type                       <chr> "A26"
#> $ process_type_def                   <chr> "Outage information"
#> $ ts_business_type                   <chr> "A53"
#> $ ts_business_type_def               <chr> "Planned maintenance"
#> $ created_date_time                  <dttm> 2026-04-08 13:09:04
#> $ reason_code                        <chr> "A95"
#> $ reason_text                        <chr> "  - Complementary information"
#> $ revision_number                    <dbl> 1
#> $ unavailability_time_interval_start <dttm> 2024-04-10 04:00:00
#> $ unavailability_time_interval_end   <dttm> 2024-04-10 09:00:00
#> $ ts_available_period_resolution     <chr> "PT60M"
#> $ ts_mrid                            <dbl> 1
#> $ ts_available_period_point_quantity <dbl> 171
#> $ ts_quantity_measure_unit_name      <chr> "MAW"
```
