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
#> → https://web-api.tp.entsoe.eu/api?documentType=A78&in_Domain=10YFR-RTE------C&out_domain=10Y1001A1001A82H&periodStart=202603052300&periodEnd=202603062300&periodStartUpdate=202602252300&periodEndUpdate=202603042300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 05 Mar 2026 16:13:28 GMT
#> <- content-type: text/xml
#> <- content-length: 993
#> <- content-disposition: inline; filename="acknowledgement.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ No additional type names added!
#> ℹ No additional eic names added!

str(df)
#> tibble [1 × 3] (S3: tbl_df/tbl/data.frame)
#>  $ created_date_time: POSIXct[1:1], format: "2026-03-05 16:13:28"
#>  $ reason_code      : chr "999"
#>  $ reason_text      : chr "No matching data found for Data item UNAVAILABILITY_IN_TRANSMISSION_GRID [10.1.A, 10.1.B] (10YFR-RTE------C, 10"| __truncated__
```
