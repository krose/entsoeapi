# Get Transfer Capacities Allocated with Third Countries — Explicit (12.1.H)

Capacity allocated outside the EU via explicit auction
(auction.Type=A02). Minimum time interval in query response ranges from
part of day to year, depending on the selected contract type. A
100-document limit applies.

## Usage

``` r
allocated_transfer_capacities_3rd_countries(
  eic_in = NULL,
  eic_out = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  contract_type = "A01",
  auction_category = "A04",
  position = 1L,
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic_in:

  Energy Identification Code of the in domain

- eic_out:

  Energy Identification Code of the out domain

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format

- contract_type:

  Contract market agreement type, valid values can be checked from
  contract_types table; "A01" = Day ahead "A02" = Weekly "A03" = Monthly
  "A04" = Yearly "A06" = Long Term "A07" = Intraday "A08" = Quarterly
  Defaults to "A01" (Day ahead)

- auction_category:

  Optional auction category; "A01" = Base "A02" = Peak "A03" = Off Peak
  "A04" = Hourly Defaults to "A04"

- position:

  Integer position for ts_classification_sequence_position. Defaults to
  1.

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
[`already_allocated_total_capacity()`](https://krose.github.io/entsoeapi/reference/already_allocated_total_capacity.md),
[`auction_revenue()`](https://krose.github.io/entsoeapi/reference/auction_revenue.md),
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
if (FALSE) { # there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
df <- entsoeapi::allocated_transfer_capacities_3rd_countries(
  eic_in = "10YSK-SEPS-----K",
  eic_out = "10YUA-WEPS-----0",
  period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2016-01-02", tz = "CET"),
  contract_type = "A01",
  auction_category = "A04",
  position = 1L,
  tidy_output = TRUE
)

dplyr::glimpse(df)
}
```
