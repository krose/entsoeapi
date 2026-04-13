# Get Implicit and Flow-based Allocations — Congestion Income (12.1.E)

Congestion income from implicit and flow-based allocations. For implicit
allocations, In_Domain and Out_Domain must be the same border EIC code.
For flow-based, both must be the same bidding zone EIC.

## Usage

``` r
congestion_income(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  contract_type = "A01",
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the border or bidding zone

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- contract_type:

  Contract market agreement type, valid values can be checked from
  contract_types table; "A01" = Day ahead "A02" = Weekly "A03" = Monthly
  "A04" = Yearly "A06" = Long Term "A07" = Intraday "A08" = Quarterly
  Defaults to "A01" (Day ahead)

- tidy_output:

  Defaults to FALSE. If TRUE, then flatten nested tables.

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
[`auction_revenue()`](https://krose.github.io/entsoeapi/reference/auction_revenue.md),
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
df <- entsoeapi::congestion_income(
  eic = "10YDOM-1001A083J",
  period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2016-01-02", tz = "CET"),
  contract_type = "A01",
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A25&businessType=B10&contract_MarketAgreement.Type=A01&in_Domain=10YDOM-1001A083J&out_Domain=10YDOM-1001A083J&periodStart=201512312300&periodEnd=201601012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:11:46 GMT
#> <- content-type: text/xml
#> <- content-length: 1674
#> <- content-disposition: inline; filename="Implicit_Allocations_Congestion_income_201512312300-201601012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!

dplyr::glimpse(df)
#> Rows: 24
#> Columns: 22
#> $ ts_in_domain_mrid          <chr> "10YDOM-1001A083J", "10YDOM-1001A083J", "10YDOM-1001A083J", "10YDOM-1001A083J", "10…
#> $ ts_in_domain_name          <chr> "Border Area Czech Republic Slovakia", "Border Area Czech Republic Slovakia", "Bord…
#> $ ts_out_domain_mrid         <chr> "10YDOM-1001A083J", "10YDOM-1001A083J", "10YDOM-1001A083J", "10YDOM-1001A083J", "10…
#> $ ts_out_domain_name         <chr> "Border Area Czech Republic Slovakia", "Border Area Czech Republic Slovakia", "Bord…
#> $ type                       <chr> "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A25",…
#> $ type_def                   <chr> "Allocation result document", "Allocation result document", "Allocation result docu…
#> $ market_agreement_type      <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01",…
#> $ market_agreement_type_def  <chr> "Daily contract", "Daily contract", "Daily contract", "Daily contract", "Daily cont…
#> $ ts_auction_type            <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01",…
#> $ ts_auction_type_def        <chr> "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", "Implicit",…
#> $ ts_business_type           <chr> "B10", "B10", "B10", "B10", "B10", "B10", "B10", "B10", "B10", "B10", "B10", "B10",…
#> $ ts_business_type_def       <chr> "Congestion income", "Congestion income", "Congestion income", "Congestion income",…
#> $ created_date_time          <dttm> 2026-04-13 08:11:46, 2026-04-13 08:11:46, 2026-04-13 08:11:46, 2026-04-13 08:11:46…
#> $ revision_number            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_resolution              <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "P…
#> $ ts_time_interval_start     <dttm> 2015-12-31 23:00:00, 2015-12-31 23:00:00, 2015-12-31 23:00:00, 2015-12-31 23:00:00,…
#> $ ts_time_interval_end       <dttm> 2016-01-01 23:00:00, 2016-01-01 23:00:00, 2016-01-01 23:00:00, 2016-01-01 23:00:00…
#> $ ts_mrid                    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_point_dt_start          <dttm> 2015-12-31 23:00:00, 2016-01-01 00:00:00, 2016-01-01 01:00:00, 2016-01-01 02:00:00…
#> $ ts_point_price_amount      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
#> $ ts_currency_unit_name      <chr> "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR",…
#> $ ts_price_measure_unit_name <chr> "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH"…
```
