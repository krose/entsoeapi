# Get RR Actual Capacity (SO GL 189.3)

Actual capacity of Replacement Reserve (RR). Process type is hardcoded
to A46 (Replacement Reserve).

## Usage

``` r
rr_actual_capacity(
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

  POSIXct or YYYY-MM-DD HH:MM:SS format

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format

- tidy_output:

  Defaults to TRUE. flatten nested tables

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df <- entsoeapi::rr_actual_capacity(
  eic = "10YAT-APG------L",
  period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2022-04-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A26&processType=A46&businessType=C77&area_Domain=10YAT-APG------L&periodStart=202112312300&periodEnd=202203312200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:14:29 GMT
#> <- content-type: text/xml
#> <- content-length: 969
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
#>  $ created_date_time: POSIXct[1:1], format: "2026-03-04 22:14:29"
#>  $ reason_code      : chr "999"
#>  $ reason_text      : chr "No matching data found for Data item FRR_RR_ACTUAL_CAPACITY [SO GL 188.4&189.3] (10YAT-APG------L) and interval"| __truncated__
```
