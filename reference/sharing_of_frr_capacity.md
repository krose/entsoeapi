# Get Sharing of FRR Capacity (SO GL 190.1)

Sharing of Frequency Restoration Reserve capacity between areas. One
year range limit applies.

## Usage

``` r
sharing_of_frr_capacity(
  eic_acquiring = NULL,
  eic_connecting = NULL,
  process_type = NULL,
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

- process_type:

  type of reserve "A56" FRR "A46" RR

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
df <- try(entsoeapi::sharing_of_frr_capacity(
  eic_acquiring = "10YCB-GERMANY--8",
  eic_connecting = "10YAT-APG------L",
  process_type = "A56",
  period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
  tidy_output = TRUE
))
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A26&BusinessType=C22&processType=A56&Acquiring_Domain=10YCB-GERMANY--8&Connecting_Domain=10YAT-APG------L&periodStart=202112312300&periodEnd=202201012300&securityToken=<...>
#> <- HTTP/2 400 
#> <- date: Tue, 17 Mar 2026 22:18:09 GMT
#> <- content-type: text/xml
#> <- content-length: 969
#> <- content-disposition: inline; filename="acknowledgement.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> Error in extract_response(content = en_cont_list, tidy_output = tidy_output) : 
#>   999 The combination of [DOCUMENT_TYPE=A26, PROCESS_TYPE=A56, BUSINESS_TYPE=C22] is not valid, or the requested
#> data is not allowed to be fetched via this service.

if (!inherits(df, "try-error")) dplyr::glimpse(df)
```
