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
  period_start = ymd(Sys.Date() + days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date() + days(x = 3L), tz = "CET"),
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
[`outages_prod_units()`](https://krose.github.io/entsoeapi/reference/outages_prod_units.md)

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
#> → https://web-api.tp.entsoe.eu/api?documentType=A78&in_Domain=10YFR-RTE------C&out_domain=10Y1001A1001A82H&periodStartUpdate=202610122200&periodEndUpdate=202610192200&periodStart=202610202200&periodEnd=202610212200&securityToken=<...>
#> <- HTTP/1.1 200 OK
#> <- Date: Tue, 20 Oct 2026 10:12:18 GMT
#> <- Content-Type: text/xml
#> <- Content-Length: 993
#> <- Connection: keep-alive
#> <- Content-Disposition: inline; filename="acknowledgement.xml"
#> <- Strict-Transport-Security: max-age=15724800
#> <- Vary: accept-encoding
#> <- X-Content-Type-Options: nosniff
#> <- X-Xss-Protection: 0
#> <- 
#> ✔ response has arrived
#> ✔ Additional definitions have been added!

dplyr::glimpse(df)
#> Rows: 1
#> Columns: 3
#> $ created_date_time <dttm> 2026-10-20 10:12:18
#> $ reason_code       <chr> "999"
#> $ reason_text       <chr> "No matching data found for Data item UNAVAILABILITY_IN_TRANSMISSION_GRID [10.1.A, 10.1.B] …
```
