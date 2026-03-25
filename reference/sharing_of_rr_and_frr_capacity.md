# Get Sharing of RR and FRR Capacity (SO GL 190.1)

Sharing of Replacement Reserve and Frequency Restoration Reserve
capacity in the same Synchronous Area.

## Usage

``` r
sharing_of_rr_and_frr_capacity(
  eic_acquiring = NULL,
  eic_connecting = NULL,
  process_type = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
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
with the queried data.

## See also

Other balancing endpoints:
[`activated_balancing_prices()`](https://krose.github.io/entsoeapi/reference/activated_balancing_prices.md),
[`aggregated_balancing_energy_bids()`](https://krose.github.io/entsoeapi/reference/aggregated_balancing_energy_bids.md),
[`allocation_of_cross_zonal_balancing_cap()`](https://krose.github.io/entsoeapi/reference/allocation_of_cross_zonal_balancing_cap.md),
[`balancing_border_cap_limit()`](https://krose.github.io/entsoeapi/reference/balancing_border_cap_limit.md),
[`balancing_energy_bids()`](https://krose.github.io/entsoeapi/reference/balancing_energy_bids.md),
[`changes_to_bid_availability()`](https://krose.github.io/entsoeapi/reference/changes_to_bid_availability.md),
[`contracted_reserves()`](https://krose.github.io/entsoeapi/reference/contracted_reserves.md),
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
[`shares_of_fcr_capacity()`](https://krose.github.io/entsoeapi/reference/shares_of_fcr_capacity.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df1 <- entsoeapi::sharing_of_rr_and_frr_capacity(
  eic_acquiring = "10YCB-GERMANY--8",
  eic_connecting = "10YAT-APG------L",
  process_type = "A46",
  period_start = lubridate::ymd(x = "2025-10-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2025-12-31", tz = "CET"),
  tidy_output = TRUE
)

dplyr::glimpse(df1)

df2 <- entsoeapi::sharing_of_rr_and_frr_capacity(
  eic_acquiring = "10YCB-GERMANY--8",
  eic_connecting = "10YAT-APG------L",
  process_type = "A56",
  period_start = lubridate::ymd(x = "2025-10-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2025-12-31", tz = "CET"),
  tidy_output = TRUE
)

dplyr::glimpse(df2)
} # }
```
