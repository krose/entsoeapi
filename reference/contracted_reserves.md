# Get Contracted Reserves (17.1.B&C)

Volumes and prices of contracted reserves. 100 documents limit applies;
paging is handled transparently.

## Usage

``` r
contracted_reserves(
  eic = NULL,
  market_agreement_type = NULL,
  process_type = NULL,
  psr_type = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the control area domain

- market_agreement_type:

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
with the queried data.

## See also

Other balancing endpoints:
[`activated_balancing_prices()`](https://krose.github.io/entsoeapi/reference/activated_balancing_prices.md),
[`aggregated_balancing_energy_bids()`](https://krose.github.io/entsoeapi/reference/aggregated_balancing_energy_bids.md),
[`allocation_of_cross_zonal_balancing_cap()`](https://krose.github.io/entsoeapi/reference/allocation_of_cross_zonal_balancing_cap.md),
[`balancing_border_cap_limit()`](https://krose.github.io/entsoeapi/reference/balancing_border_cap_limit.md),
[`balancing_energy_bids()`](https://krose.github.io/entsoeapi/reference/balancing_energy_bids.md),
[`changes_to_bid_availability()`](https://krose.github.io/entsoeapi/reference/changes_to_bid_availability.md),
[`current_balancing_state()`](https://krose.github.io/entsoeapi/reference/current_balancing_state.md),
[`elastic_demands()`](https://krose.github.io/entsoeapi/reference/elastic_demands.md),
[`exchanged_volumes()`](https://krose.github.io/entsoeapi/reference/exchanged_volumes.md),
[`exchanged_volumes_per_border()`](https://krose.github.io/entsoeapi/reference/exchanged_volumes_per_border.md),
[`fcr_total_capacity()`](https://krose.github.io/entsoeapi/reference/fcr_total_capacity.md),
[`financial_expenses_and_income()`](https://krose.github.io/entsoeapi/reference/financial_expenses_and_income.md),
[`hvdc_link_constrains()`](https://krose.github.io/entsoeapi/reference/hvdc_link_constrains.md),
[`imbalance_prices()`](https://krose.github.io/entsoeapi/reference/imbalance_prices.md),
[`imbalance_volumes()`](https://krose.github.io/entsoeapi/reference/imbalance_volumes.md),
[`netted_volumes()`](https://krose.github.io/entsoeapi/reference/netted_volumes.md),
[`netted_volumes_per_border()`](https://krose.github.io/entsoeapi/reference/netted_volumes_per_border.md),
[`procured_balancing_capacity()`](https://krose.github.io/entsoeapi/reference/procured_balancing_capacity.md),
[`rr_and_frr_actual_capacity()`](https://krose.github.io/entsoeapi/reference/rr_and_frr_actual_capacity.md),
[`shares_of_fcr_capacity()`](https://krose.github.io/entsoeapi/reference/shares_of_fcr_capacity.md),
[`sharing_of_rr_and_frr_capacity()`](https://krose.github.io/entsoeapi/reference/sharing_of_rr_and_frr_capacity.md)

## Examples

``` r
df <- entsoeapi::contracted_reserves(
  eic = "10YCZ-CEPS-----N",
  market_agreement_type = "A01",
  period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A81&businessType=B95&type_MarketAgreement.Type=A01&controlArea_Domain=10YCZ-CEPS-----N&periodStart=202112312300&periodEnd=202201012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 31 Mar 2026 06:57:11 GMT
#> <- content-type: application/zip
#> <- content-length: 2917
#> <- content-disposition: attachment; filename="Amount and Prices Paid of Balancing Reserves Under Contract_202112312300-202201012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpOqoMiS/001-AMOUNT_AND_PRICES_PAID_OF_BALANCING_RESERVES_UNDER_CONTRACT_R3_202112312300-202201012300.xml has been read in
#> ✔ /tmp/RtmpOqoMiS/002-AMOUNT_AND_PRICES_PAID_OF_BALANCING_RESERVES_UNDER_CONTRACT_R3_202112312300-202201012300.xml has been read in
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!

dplyr::glimpse(df)
#> Rows: 96
#> Columns: 27
#> $ area_domain_mrid              <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", …
#> $ area_domain_name              <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", "Czech R…
#> $ type                          <chr> "A81", "A81", "A81", "A81", "A81", "A81", "A81", "A81", "A81", "A81", "A81", "A8…
#> $ type_def                      <chr> "Contracted reserves", "Contracted reserves", "Contracted reserves", "Contracted…
#> $ process_type                  <chr> "A47", "A47", "A47", "A47", "A47", "A47", "A47", "A47", "A47", "A47", "A47", "A4…
#> $ process_type_def              <chr> "Manual frequency restoration reserve", "Manual frequency restoration reserve", …
#> $ market_agreement_type         <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A0…
#> $ market_agreement_type_def     <chr> "Daily contract", "Daily contract", "Daily contract", "Daily contract", "Daily c…
#> $ ts_flow_direction             <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A0…
#> $ ts_flow_direction_def         <chr> "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "U…
#> $ ts_business_type              <chr> "B95", "B95", "B95", "B95", "B95", "B95", "B95", "B95", "B95", "B95", "B95", "B9…
#> $ ts_business_type_def          <chr> "Procured capacity", "Procured capacity", "Procured capacity", "Procured capacit…
#> $ ts_mkt_psr_type               <chr> "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A0…
#> $ ts_mkt_psr_type_def           <chr> "Generation", "Generation", "Generation", "Generation", "Generation", "Generatio…
#> $ created_date_time             <dttm> 2026-03-31 06:57:11, 2026-03-31 06:57:11, 2026-03-31 06:57:11, 2026-03-31 06:57…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution                 <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M",…
#> $ ts_time_interval_start        <dttm> 2021-12-31 23:00:00, 2021-12-31 23:00:00, 2021-12-31 23:00:00, 2021-12-31 23:00…
#> $ ts_time_interval_end          <dttm> 2022-01-01 23:00:00, 2022-01-01 23:00:00, 2022-01-01 23:00:00, 2022-01-01 23:00…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2,…
#> $ ts_point_dt_start             <dttm> 2021-12-31 23:00:00, 2022-01-01 00:00:00, 2022-01-01 01:00:00, 2022-01-01 02:00…
#> $ ts_point_quantity             <dbl> 377, 377, 377, 377, 377, 377, 377, 365, 377, 405, 407, 407, 377, 377, 377, 383, …
#> $ procurement_price_amount      <dbl> 1131.79, 1128.61, 1128.61, 1128.61, 1128.61, 1136.04, 1155.76, 1157.95, 1128.61,…
#> $ ts_currency_unit_name         <chr> "CZK", "CZK", "CZK", "CZK", "CZK", "CZK", "CZK", "CZK", "CZK", "CZK", "CZK", "CZ…
#> $ ts_quantity_measure_unit_name <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MA…
#> $ imbalance_price_category      <chr> "A06", "A06", "A06", "A06", "A06", "A06", "A06", "A06", "A06", "A06", "A06", "A0…
#> $ imbalance_price_category_def  <chr> "Average bid price", "Average bid price", "Average bid price", "Average bid pric…
```
