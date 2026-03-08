# Get HVDC Link Constraints (IFs 4.5)

Permanent allocation limitations to cross-border capacity on HVDC lines.

## Usage

``` r
hvdc_link_constrains(
  eic_in = NULL,
  eic_out = NULL,
  eic_ic = NULL,
  process_type = "A63",
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic_in:

  Energy Identification Code of the in domain

- eic_out:

  Energy Identification Code of the out domain

- eic_ic:

  Energy Identification Code of the interconnector If used, data for the
  given Transmission Asset is returned

- process_type:

  "A47" Manual frequency restoration reserve "A51" Automatic Frequency
  Restoration Reserve "A63" Imbalance Netting; Defaults to "A63"

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
df <- entsoeapi::hvdc_link_constrains(
  eic_in = "10YAT-APG------L",
  eic_out = "10YDE-RWENET---I",
  process_type = "A63",
  period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A99&processType=A63&BusinessType=B06&In_Domain=10YAT-APG------L&Out_Domain=10YDE-RWENET---I&periodStart=202112312300&periodEnd=202201012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Sun, 08 Mar 2026 23:48:43 GMT
#> <- content-type: text/xml
#> <- content-length: 1014
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
#> $ created_date_time <dttm> 2026-03-08 23:48:43
#> $ reason_code       <chr> "999"
#> $ reason_text       <chr> "No matching data found for Data item PERMANENT_LIMITATIONS_TO_CROSS_BORDER_CAPACITY_ON_HVD…
```
