# Get Explicit Offered Transfer Capacity (11.1.A)

Explicit offered transfer capacity values.

## Usage

``` r
explicit_offered_transfer_capacity(
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
  contract_types table; A01 = Day ahead A02 = Weekly A03 = Monthly A04 =
  Yearly A06 = Long Term A07 = Intraday A08 = Quarterly Defaults to
  "A01" (Day ahead)

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
[`energy_prices()`](https://krose.github.io/entsoeapi/reference/energy_prices.md),
[`explicit_offered_transfer_capacities()`](https://krose.github.io/entsoeapi/reference/explicit_offered_transfer_capacities.md),
[`flow_based_allocations()`](https://krose.github.io/entsoeapi/reference/flow_based_allocations.md),
[`implicit_offered_transfer_capacities()`](https://krose.github.io/entsoeapi/reference/implicit_offered_transfer_capacities.md),
[`implicit_offered_transfer_capacity()`](https://krose.github.io/entsoeapi/reference/implicit_offered_transfer_capacity.md),
[`intraday_prices()`](https://krose.github.io/entsoeapi/reference/intraday_prices.md),
[`net_positions()`](https://krose.github.io/entsoeapi/reference/net_positions.md),
[`total_nominated_capacity()`](https://krose.github.io/entsoeapi/reference/total_nominated_capacity.md)

## Examples

``` r
if (FALSE) { # there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
df <- entsoeapi::explicit_offered_transfer_capacity(
  eic_in = "10YBE----------2",
  eic_out = "10YGB----------A",
  period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
  period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
  contract_type = "A01",
  tidy_output = TRUE
)

dplyr::glimpse(df)
}
```
