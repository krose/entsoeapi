# Get FCR Total Capacity (SO GL 187.2)

Total Frequency Containment Reserve capacity. One year range limit
applies.

## Usage

``` r
fcr_total_capacity(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the area domain

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- tidy_output:

  Defaults to TRUE. flatten nested tables

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df <- entsoeapi::fcr_total_capacity(
  eic = "10YEU-CONT-SYNC0",
  period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A26&businessType=A25&area_Domain=10YEU-CONT-SYNC0&periodStart=202112312300&periodEnd=202201012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 09 Mar 2026 12:41:52 GMT
#> <- content-type: text/xml
#> <- content-length: 958
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
#> $ created_date_time <dttm> 2026-03-09 12:41:52
#> $ reason_code       <chr> "999"
#> $ reason_text       <chr> "No matching data found for Data item FCR_TOTAL_CAPACITY_R3 [SO GL 187.2] (10YEU-CONT-SYNC0…
```
