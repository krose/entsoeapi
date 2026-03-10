# Get RR and FRR Actual Capacity (SO GL 188.4 & 189.3)

Actual capacity of Frequency Restoration Reserve (FRR) or Replacement
Reserve (RR).

## Usage

``` r
rr_and_frr_actual_capacity(
  eic = NULL,
  process_type = NULL,
  business_type = "C78",
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the area domain

- process_type:

  type of reserve "A56" FRR "A46" RR

- business_type:

  type of aggregation "C77" minimum "C78" average "C79" max defaults to
  "C78"

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
df <- entsoeapi::rr_and_frr_actual_capacity(
  eic = "10YAT-APG------L",
  process_type = "A56",
  period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2022-04-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A26&processType=A56&businessType=C78&Area_Domain=10YAT-APG------L&periodStart=202112312300&periodEnd=202203312200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 10 Mar 2026 17:33:08 GMT
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

dplyr::glimpse(df)
#> Rows: 1
#> Columns: 3
#> $ created_date_time <dttm> 2026-03-10 17:33:08
#> $ reason_code       <chr> "999"
#> $ reason_text       <chr> "No matching data found for Data item FRR_RR_ACTUAL_CAPACITY [SO GL 188.4&189.3] (10YAT-APG…
```
