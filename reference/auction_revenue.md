# Get the Auction Revenue (12.1.A)

Explicit Allocations - Auction Revenue.

## Usage

``` r
auction_revenue(
  eic_in = NULL,
  eic_out = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
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

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with the queried data.

## See also

Other market endpoints:
[`aggregated_bids()`](https://krose.github.io/entsoeapi/reference/aggregated_bids.md),
[`allocated_transfer_capacities_3rd_countries()`](https://krose.github.io/entsoeapi/reference/allocated_transfer_capacities_3rd_countries.md),
[`already_allocated_total_capacity()`](https://krose.github.io/entsoeapi/reference/already_allocated_total_capacity.md),
[`congestion_income()`](https://krose.github.io/entsoeapi/reference/congestion_income.md),
[`continuous_offered_transfer_capacities()`](https://krose.github.io/entsoeapi/reference/continuous_offered_transfer_capacities.md),
[`continuous_offered_transfer_capacity()`](https://krose.github.io/entsoeapi/reference/continuous_offered_transfer_capacity.md),
[`energy_prices()`](https://krose.github.io/entsoeapi/reference/energy_prices.md),
[`explicit_offered_transfer_capacities()`](https://krose.github.io/entsoeapi/reference/explicit_offered_transfer_capacities.md),
[`explicit_offered_transfer_capacity()`](https://krose.github.io/entsoeapi/reference/explicit_offered_transfer_capacity.md),
[`flow_based_allocations()`](https://krose.github.io/entsoeapi/reference/flow_based_allocations.md),
[`implicit_offered_transfer_capacities()`](https://krose.github.io/entsoeapi/reference/implicit_offered_transfer_capacities.md),
[`implicit_offered_transfer_capacity()`](https://krose.github.io/entsoeapi/reference/implicit_offered_transfer_capacity.md),
[`intraday_prices()`](https://krose.github.io/entsoeapi/reference/intraday_prices.md),
[`net_positions()`](https://krose.github.io/entsoeapi/reference/net_positions.md),
[`total_nominated_capacity()`](https://krose.github.io/entsoeapi/reference/total_nominated_capacity.md)

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
#> <- date: Wed, 25 Mar 2026 19:15:43 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Auction_Revenue_202308242200-202308252200.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!

dplyr::glimpse(df)
#> Rows: 24
#> Columns: 22
#> $ ts_in_domain_mrid         <chr> "10YBA-JPCC-----D", "10YBA-JPCC-----D", "10YBA-JPCC-----D", "10YBA-JPCC-----D", "10Y…
#> $ ts_in_domain_name         <chr> "Bosnia and Herzegovina", "Bosnia and Herzegovina", "Bosnia and Herzegovina", "Bosni…
#> $ ts_out_domain_mrid        <chr> "10YHR-HEP------M", "10YHR-HEP------M", "10YHR-HEP------M", "10YHR-HEP------M", "10Y…
#> $ ts_out_domain_name        <chr> "Croatia", "Croatia", "Croatia", "Croatia", "Croatia", "Croatia", "Croatia", "Croati…
#> $ type                      <chr> "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A25", …
#> $ type_def                  <chr> "Allocation result document", "Allocation result document", "Allocation result docum…
#> $ market_agreement_type     <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", …
#> $ market_agreement_type_def <chr> "Daily contract", "Daily contract", "Daily contract", "Daily contract", "Daily contr…
#> $ ts_auction_mrid           <chr> "HRBA-DH-20230825-45403", "HRBA-DH-20230825-45403", "HRBA-DH-20230825-45403", "HRBA-…
#> $ ts_auction_type           <chr> "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", …
#> $ ts_auction_type_def       <chr> "Explicit", "Explicit", "Explicit", "Explicit", "Explicit", "Explicit", "Explicit", …
#> $ ts_business_type          <chr> "B07", "B07", "B07", "B07", "B07", "B07", "B07", "B07", "B07", "B07", "B07", "B07", …
#> $ ts_business_type_def      <chr> "Auction revenue", "Auction revenue", "Auction revenue", "Auction revenue", "Auction…
#> $ created_date_time         <dttm> 2026-03-25 19:15:43, 2026-03-25 19:15:43, 2026-03-25 19:15:43, 2026-03-25 19:15:43,…
#> $ revision_number           <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_resolution             <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT…
#> $ ts_time_interval_start    <dttm> 2023-08-24 22:00:00, 2023-08-24 22:00:00, 2023-08-24 22:00:00, 2023-08-24 22:00:00, …
#> $ ts_time_interval_end      <dttm> 2023-08-25 22:00:00, 2023-08-25 22:00:00, 2023-08-25 22:00:00, 2023-08-25 22:00:00,…
#> $ ts_mrid                   <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_point_dt_start         <dttm> 2023-08-24 22:00:00, 2023-08-24 23:00:00, 2023-08-25 00:00:00, 2023-08-25 01:00:00,…
#> $ ts_point_price_amount     <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 13.50, 33.75…
#> $ ts_currency_unit_name     <chr> "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR",…
```
