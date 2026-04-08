# Get Day-Ahead or Intraday Energy Prices (12.1.D)

Prices in currency/MWh created on spot (Day-Ahead or Intraday) market.
The data is delivered for each market time unit.

## Usage

``` r
energy_prices(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  contract_type = "A01",
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the related domain

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- contract_type:

  Contract market agreement type; "A01" = Day ahead "A07" = Intraday
  Defaults to "A01" (Day ahead)

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
[`auction_revenue()`](https://krose.github.io/entsoeapi/reference/auction_revenue.md),
[`congestion_income()`](https://krose.github.io/entsoeapi/reference/congestion_income.md),
[`continuous_offered_transfer_capacities()`](https://krose.github.io/entsoeapi/reference/continuous_offered_transfer_capacities.md),
[`continuous_offered_transfer_capacity()`](https://krose.github.io/entsoeapi/reference/continuous_offered_transfer_capacity.md),
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
df1 <- entsoeapi::energy_prices(
  eic = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2019-12-01", tz = "CET"),
  contract_type = "A01",
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A44&in_Domain=10YCZ-CEPS-----N&out_Domain=10YCZ-CEPS-----N&periodStart=201910312300&periodEnd=201911302300&contract_MarketAgreement.type=A01&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 08 Apr 2026 13:14:17 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Energy_Prices_201910312300-201911302300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!

dplyr::glimpse(df1)
#> Rows: 720
#> Columns: 22
#> $ ts_in_domain_mrid          <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10…
#> $ ts_in_domain_name          <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", "Czech Repu…
#> $ ts_out_domain_mrid         <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10…
#> $ ts_out_domain_name         <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", "Czech Repu…
#> $ type                       <chr> "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44",…
#> $ type_def                   <chr> "Price Document", "Price Document", "Price Document", "Price Document", "Price Docu…
#> $ market_agreement_type      <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01",…
#> $ market_agreement_type_def  <chr> "Daily contract", "Daily contract", "Daily contract", "Daily contract", "Daily cont…
#> $ ts_auction_type            <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01",…
#> $ ts_auction_type_def        <chr> "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", "Implicit",…
#> $ ts_business_type           <chr> "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62",…
#> $ ts_business_type_def       <chr> "Spot price", "Spot price", "Spot price", "Spot price", "Spot price", "Spot price",…
#> $ created_date_time          <dttm> 2026-04-08 13:14:17, 2026-04-08 13:14:17, 2026-04-08 13:14:17, 2026-04-08 13:14:17…
#> $ revision_number            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution              <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "P…
#> $ ts_time_interval_start     <dttm> 2019-10-31 23:00:00, 2019-10-31 23:00:00, 2019-10-31 23:00:00, 2019-10-31 23:00:00…
#> $ ts_time_interval_end       <dttm> 2019-11-01 23:00:00, 2019-11-01 23:00:00, 2019-11-01 23:00:00, 2019-11-01 23:00:00…
#> $ ts_mrid                    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2,…
#> $ ts_point_dt_start          <dttm> 2019-10-31 23:00:00, 2019-11-01 00:00:00, 2019-11-01 01:00:00, 2019-11-01 02:00:00…
#> $ ts_point_price_amount      <dbl> 33.03, 31.81, 30.53, 30.81, 30.95, 31.18, 32.19, 34.00, 33.22, 33.30, 32.50, 31.99,…
#> $ ts_currency_unit_name      <chr> "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR",…
#> $ ts_price_measure_unit_name <chr> "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH",…

df2 <- entsoeapi::energy_prices(
  eic = "10YES-REE------0",
  period_start = lubridate::ymd(x = "2026-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2026-01-02", tz = "CET"),
  contract_type = "A07",
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A44&in_Domain=10YES-REE------0&out_Domain=10YES-REE------0&periodStart=202512312300&periodEnd=202601012300&contract_MarketAgreement.type=A07&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 08 Apr 2026 13:14:17 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Energy_Prices_202512312300-202601012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!

dplyr::glimpse(df2)
#> Rows: 240
#> Columns: 23
#> $ ts_in_domain_mrid                   <chr> "10YES-REE------0", "10YES-REE------0", "10YES-REE------0", "10YES-REE----…
#> $ ts_in_domain_name                   <chr> "Spain", "Spain", "Spain", "Spain", "Spain", "Spain", "Spain", "Spain", "S…
#> $ ts_out_domain_mrid                  <chr> "10YES-REE------0", "10YES-REE------0", "10YES-REE------0", "10YES-REE----…
#> $ ts_out_domain_name                  <chr> "Spain", "Spain", "Spain", "Spain", "Spain", "Spain", "Spain", "Spain", "S…
#> $ type                                <chr> "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44…
#> $ type_def                            <chr> "Price Document", "Price Document", "Price Document", "Price Document", "P…
#> $ market_agreement_type               <chr> "A07", "A07", "A07", "A07", "A07", "A07", "A07", "A07", "A07", "A07", "A07…
#> $ market_agreement_type_def           <chr> "Intraday contract", "Intraday contract", "Intraday contract", "Intraday c…
#> $ ts_auction_type                     <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01…
#> $ ts_auction_type_def                 <chr> "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", "I…
#> $ ts_business_type                    <chr> "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62…
#> $ ts_business_type_def                <chr> "Spot price", "Spot price", "Spot price", "Spot price", "Spot price", "Spo…
#> $ created_date_time                   <dttm> 2026-04-08 13:14:17, 2026-04-08 13:14:17, 2026-04-08 13:14:17, 2026-04-08…
#> $ revision_number                     <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution                       <chr> "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "P…
#> $ ts_time_interval_start              <dttm> 2025-12-31 23:00:00, 2025-12-31 23:00:00, 2025-12-31 23:00:00, 2025-12-31…
#> $ ts_time_interval_end                <dttm> 2026-01-01 23:00:00, 2026-01-01 23:00:00, 2026-01-01 23:00:00, 2026-01-01…
#> $ ts_mrid                             <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start                   <dttm> 2025-12-31 23:00:00, 2025-12-31 23:15:00, 2025-12-31 23:30:00, 2025-12-31…
#> $ ts_point_price_amount               <dbl> 93.22, 92.83, 91.71, 91.25, 89.37, 90.64, 88.99, 87.08, 85.64, 86.00, 84.9…
#> $ ts_currency_unit_name               <chr> "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR…
#> $ ts_price_measure_unit_name          <chr> "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH…
#> $ ts_classification_sequence_position <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
```
