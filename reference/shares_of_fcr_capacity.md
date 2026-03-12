# Get Shares of FCR Capacity (SO GL 187.2)

Shares of Frequency Containment Reserve capacity per area. One year
range limit applies.

## Usage

``` r
shares_of_fcr_capacity(
  eic = NULL,
  business_type = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the area domain

- business_type:

  type of FCR capacity share "C23" share of FCR capacity "B95"
  contracted FCR capacity

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
df <- entsoeapi::shares_of_fcr_capacity(
  eic = "10YDE-VE-------2",
  business_type = "C23",
  period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A26&businessType=C23&area_Domain=10YDE-VE-------2&periodStart=202112312300&periodEnd=202201012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 12 Mar 2026 10:23:25 GMT
#> <- content-type: text/xml
#> <- content-length: 957
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
#> $ created_date_time <dttm> 2026-03-12 10:23:25
#> $ reason_code       <chr> "999"
#> $ reason_text       <chr> "No matching data found for Data item SHARE_OF_CAPACITY_R3 [SO GL 187.2] (10YDE-VE-------2)…
```
