# Get the Auction Revenue (12.1.A)

Explicit Allocations - Auction Revenue.

## Usage

``` r
auction_revenue(
  eic_in = NULL,
  eic_out = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  contract_type = "A01",
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic_in:

  Energy Identification Code of the bidding zone or control area (TSO)

- eic_out:

  Energy Identification Code of the bidding zone or control area (TSO)

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- contract_type:

  Contract market agreement type, valid values can be checked from
  contract_types table; "A01" = Daily "A02" = Weekly "A03" = Monthly
  "A04" = Yearly "A06" = Long Term "A07" = Intraday "A08" = Quarterly
  Defaults to "A01" (Daily)

- tidy_output:

  Defaults to TRUE. If TRUE, then flatten nested tables.

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df <- entsoeapi::auction_revenue(
  eic_in = "10YBA-JPCC-----D",
  eic_out = "10YHR-HEP------M",
  period_start = lubridate::ymd(x = "2023-08-25", tz = "CET"),
  period_end = lubridate::ymd(x = "2023-08-26", tz = "CET"),
  contract_type = "A01",
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A25&businessType=B07&contract_MarketAgreement.Type=A01&in_Domain=10YBA-JPCC-----D&out_Domain=10YHR-HEP------M&periodStart=202308242200&periodEnd=202308252200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:07:53 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Auction_Revenue_202308242200-202308252200.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ No additional definitions added!

str(df)
#> tibble [24 × 22] (S3: tbl_df/tbl/data.frame)
#>  $ ts_in_domain_mrid                    : chr [1:24] "10YBA-JPCC-----D" "10YBA-JPCC-----D" "10YBA-JPCC-----D" "10YBA-JPCC-----D" ...
#>  $ ts_in_domain_name                    : chr [1:24] "Bosnia and Herzegovina" "Bosnia and Herzegovina" "Bosnia and Herzegovina" "Bosnia and Herzegovina" ...
#>  $ ts_out_domain_mrid                   : chr [1:24] "10YHR-HEP------M" "10YHR-HEP------M" "10YHR-HEP------M" "10YHR-HEP------M" ...
#>  $ ts_out_domain_name                   : chr [1:24] "Croatia" "Croatia" "Croatia" "Croatia" ...
#>  $ type                                 : chr [1:24] "A25" "A25" "A25" "A25" ...
#>  $ type_def                             : chr [1:24] "Allocation result document" "Allocation result document" "Allocation result document" "Allocation result document" ...
#>  $ ts_contract_market_agreement_type    : chr [1:24] "A01" "A01" "A01" "A01" ...
#>  $ ts_contract_market_agreement_type_def: chr [1:24] "Daily contract" "Daily contract" "Daily contract" "Daily contract" ...
#>  $ ts_auction_mrid                      : chr [1:24] "HRBA-DH-20230825-45403" "HRBA-DH-20230825-45403" "HRBA-DH-20230825-45403" "HRBA-DH-20230825-45403" ...
#>  $ ts_auction_type                      : chr [1:24] "A02" "A02" "A02" "A02" ...
#>  $ ts_auction_type_def                  : chr [1:24] "Explicit" "Explicit" "Explicit" "Explicit" ...
#>  $ ts_business_type                     : chr [1:24] "B07" "B07" "B07" "B07" ...
#>  $ ts_business_type_def                 : chr [1:24] "Auction revenue" "Auction revenue" "Auction revenue" "Auction revenue" ...
#>  $ created_date_time                    : POSIXct[1:24], format: "2026-03-04 22:07:53" "2026-03-04 22:07:53" ...
#>  $ revision_number                      : num [1:24] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution                        : chr [1:24] "PT60M" "PT60M" "PT60M" "PT60M" ...
#>  $ ts_time_interval_start               : POSIXct[1:24], format: "2023-08-24 22:00:00" "2023-08-24 22:00:00" ...
#>  $ ts_time_interval_end                 : POSIXct[1:24], format: "2023-08-25 22:00:00" "2023-08-25 22:00:00" ...
#>  $ ts_mrid                              : num [1:24] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start                    : POSIXct[1:24], format: "2023-08-24 22:00:00" "2023-08-24 23:00:00" ...
#>  $ ts_point_price_amount                : num [1:24] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ ts_currency_unit_name                : chr [1:24] "EUR" "EUR" "EUR" "EUR" ...
```
