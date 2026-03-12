# Get Changes to Bid Availability (IFs mFRR 9.9, aFRR 9.6 & 9.8)

Changes to bid availability for scheduled activation products. 100
documents limit applies; paging is handled transparently.

## Usage

``` r
changes_to_bid_availability(
  eic = NULL,
  business_type = "C46",
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the domain

- business_type:

  type of frequency restoration reserve "C40" Conditional bid "C41"
  Thermal limit "C42" Frequency limit "C43" Voltage limit "C44" Current
  limit "C45" Short-circuit current limits "C46" Dynamic stability limit
  Defaults to "C46".

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- tidy_output:

  Defaults to TRUE. flatten nested tables

- security_token:

  Security token for ENTSO-E transparency platform

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with the queried data, or `NULL` if no data is available for the given
parameters.

## Examples

``` r
df <- entsoeapi::changes_to_bid_availability(
  eic = "10YCZ-CEPS-----N",
  business_type = "C46",
  period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=B45&processType=A47&businessType=C46&ControlArea_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 12 Mar 2026 09:21:49 GMT
#> <- content-type: text/xml
#> <- content-length: 983
#> <- content-disposition: inline; filename="acknowledgement.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ No additional type names added!
#> ℹ No additional eic names added!

dplyr::glimpse(df)
#> Rows: 1
#> Columns: 3
#> $ created_date_time <dttm> 2026-03-12 09:21:49
#> $ reason_code       <chr> "999"
#> $ reason_text       <chr> "No matching data found for Data item CHANGES_TO_BID_AVAILABILITY [IFs: mFRR 9.9 aFRR 9.6&9…
```
