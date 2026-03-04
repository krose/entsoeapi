# Get Allocation of Cross-Zonal Balancing Capacity (GL EB 12.3.H&I)

Use of allocated cross-zonal balancing capacity between an acquiring and
a connecting domain.

## Usage

``` r
allocation_of_cross_zonal_balancing_cap(
  eic_acquiring = NULL,
  eic_connecting = NULL,
  type_market_agreement = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic_acquiring:

  Energy Identification Code of the acquiring domain

- eic_connecting:

  Energy Identification Code of the connecting domain

- type_market_agreement:

  Optional market agreement type code

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
df <- entsoeapi::allocation_of_cross_zonal_balancing_cap(
  eic_acquiring = "10YAT-APG------L",
  eic_connecting = "10YCH-SWISSGRIDZ",
  period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A38&processType=A46&Acquiring_Domain=10YAT-APG------L&Connecting_Domain=10YCH-SWISSGRIDZ&periodStart=202312312300&periodEnd=202401012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:12:39 GMT
#> <- content-type: text/xml
#> <- content-length: 1001
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
#>  $ created_date_time: POSIXct[1:1], format: "2026-03-04 22:12:39"
#>  $ reason_code      : chr "999"
#>  $ reason_text      : chr "No matching data found for Data item ALLOCATION_AND_USE_CROSS_ZONAL_CAPACITY [GL EB 12.3.H&I] (10YAT-APG------L"| __truncated__
```
