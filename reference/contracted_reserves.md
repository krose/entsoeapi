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
if (FALSE) { # there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
df <- entsoeapi::contracted_reserves(
  eic = "10YCZ-CEPS-----N",
  market_agreement_type = "A01",
  period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
  tidy_output = TRUE
)

dplyr::glimpse(df)
}
```
