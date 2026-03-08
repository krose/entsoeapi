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
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A25&businessType=B07&contract_MarketAgreement.Type=A01&in_Domain=10YBA-JPCC-----D&out_Domain=10YHR-HEP------M&periodStart=202308242200&periodEnd=202308252200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Sun, 08 Mar 2026 23:47:46 GMT
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

dplyr::glimpse(df)
#> Rows: 24
#> Columns: 22
#> $ ts_in_domain_mrid                     <chr> "10YBA-JPCC-----D", "10YBA-JPCC-----D", "10YBA-JPCC-----D", "10YBA-JPCC-…
#> $ ts_in_domain_name                     <chr> "Bosnia and Herzegovina", "Bosnia and Herzegovina", "Bosnia and Herzegov…
#> $ ts_out_domain_mrid                    <chr> "10YHR-HEP------M", "10YHR-HEP------M", "10YHR-HEP------M", "10YHR-HEP--…
#> $ ts_out_domain_name                    <chr> "Croatia", "Croatia", "Croatia", "Croatia", "Croatia", "Croatia", "Croat…
#> $ type                                  <chr> "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A…
#> $ type_def                              <chr> "Allocation result document", "Allocation result document", "Allocation …
#> $ ts_contract_market_agreement_type     <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A…
#> $ ts_contract_market_agreement_type_def <chr> "Daily contract", "Daily contract", "Daily contract", "Daily contract", …
#> $ ts_auction_mrid                       <chr> "HRBA-DH-20230825-45403", "HRBA-DH-20230825-45403", "HRBA-DH-20230825-45…
#> $ ts_auction_type                       <chr> "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A…
#> $ ts_auction_type_def                   <chr> "Explicit", "Explicit", "Explicit", "Explicit", "Explicit", "Explicit", …
#> $ ts_business_type                      <chr> "B07", "B07", "B07", "B07", "B07", "B07", "B07", "B07", "B07", "B07", "B…
#> $ ts_business_type_def                  <chr> "Auction revenue", "Auction revenue", "Auction revenue", "Auction revenu…
#> $ created_date_time                     <dttm> 2026-03-08 23:47:46, 2026-03-08 23:47:46, 2026-03-08 23:47:46, 2026-03-…
#> $ revision_number                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_resolution                         <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", …
#> $ ts_time_interval_start                <dttm> 2023-08-24 22:00:00, 2023-08-24 22:00:00, 2023-08-24 22:00:00, 2023-08-2…
#> $ ts_time_interval_end                  <dttm> 2023-08-25 22:00:00, 2023-08-25 22:00:00, 2023-08-25 22:00:00, 2023-08-…
#> $ ts_mrid                               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_point_dt_start                     <dttm> 2023-08-24 22:00:00, 2023-08-24 23:00:00, 2023-08-25 00:00:00, 2023-08-…
#> $ ts_point_price_amount                 <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, …
#> $ ts_currency_unit_name                 <chr> "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "…
```
