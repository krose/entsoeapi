# Get Contracted Reserves (17.1.B&C)

Volumes and prices of contracted reserves. 100 documents limit applies;
paging is handled transparently.

## Usage

``` r
contracted_reserves(
  eic = NULL,
  type_market_agreement = NULL,
  process_type = NULL,
  psr_type = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the control area domain

- type_market_agreement:

  Market agreement type code (mandatory) "A01" Daily "A02" Weekly "A03"
  Monthly "A04" Yearly "A06" Long term "A13" Hourly

- process_type:

  Optional process type code "A46" Replacement reserve "A47" Manual
  frequency restoration reserve "A51" Automatic frequency restoration
  reserve "A52" Frequency containment reserve

- psr_type:

  Optional PSR type code "A03" Mixed "A04" Generation "A05" Load

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format

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
df <- entsoeapi::contracted_reserves(
  eic = "10YCZ-CEPS-----N",
  type_market_agreement = "A13",
  period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A81&businessType=B95&type_MarketAgreement.Type=A13&controlArea_Domain=10YCZ-CEPS-----N&periodStart=202112312300&periodEnd=202201012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 17 Mar 2026 13:12:07 GMT
#> <- content-type: text/xml
#> <- content-length: 1000
#> <- content-disposition: inline; filename="acknowledgement.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ No additional type names added!
#> ℹ pulling area_eic_name table from cache
#> ℹ No additional eic names added!

dplyr::glimpse(df)
#> Rows: 1
#> Columns: 3
#> $ created_date_time <dttm> 2026-03-17 13:12:07
#> $ reason_code       <chr> "999"
#> $ reason_text       <chr> "No matching data found for Data item AMOUNT_AND_PRICES_PAID_OF_BALANCING_RESERVES_UNDER_CO…
```
