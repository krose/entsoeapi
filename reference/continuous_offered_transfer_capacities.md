# Get Continuous Offered Transfer Capacities (11.1)

Continuous offered transfer capacities for the intraday continuous
market.

## Usage

``` r
continuous_offered_transfer_capacities(
  eic_in = NULL,
  eic_out = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
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
df <- entsoeapi::continuous_offered_transfer_capacities(
  eic_in = "10YNL----------L",
  eic_out = "10YBE----------2",
  period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A31&auction.Type=A08&contract_MarketAgreement.Type=A07&in_Domain=10YNL----------L&out_Domain=10YBE----------2&periodStart=202405152200&periodEnd=202405162200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:11:46 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Offered Transfer Capacities Continuous_202405152200-202405162200.xml"
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
#> Rows: 96
#> Columns: 21
#> $ ts_in_domain_mrid             <chr> "10YNL----------L", "10YNL----------L", "10YNL----------L", "10YNL----------L", …
#> $ ts_in_domain_name             <chr> "Netherlands", "Netherlands", "Netherlands", "Netherlands", "Netherlands", "Neth…
#> $ ts_out_domain_mrid            <chr> "10YBE----------2", "10YBE----------2", "10YBE----------2", "10YBE----------2", …
#> $ ts_out_domain_name            <chr> "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Be…
#> $ type                          <chr> "B33", "B33", "B33", "B33", "B33", "B33", "B33", "B33", "B33", "B33", "B33", "B3…
#> $ type_def                      <chr> "Published offered capacity", "Published offered capacity", "Published offered c…
#> $ market_agreement_type         <chr> "A07", "A07", "A07", "A07", "A07", "A07", "A07", "A07", "A07", "A07", "A07", "A0…
#> $ market_agreement_type_def     <chr> "Intraday contract", "Intraday contract", "Intraday contract", "Intraday contrac…
#> $ ts_auction_type               <chr> "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A0…
#> $ ts_auction_type_def           <chr> "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuou…
#> $ ts_business_type              <chr> "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A3…
#> $ ts_business_type_def          <chr> "Offered Capacity", "Offered Capacity", "Offered Capacity", "Offered Capacity", …
#> $ created_date_time             <dttm> 2026-04-13 08:11:46, 2026-04-13 08:11:46, 2026-04-13 08:11:46, 2026-04-13 08:11…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution                 <chr> "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M",…
#> $ ts_time_interval_start        <dttm> 2024-05-15 22:00:00, 2024-05-15 22:00:00, 2024-05-15 22:00:00, 2024-05-15 22:00…
#> $ ts_time_interval_end          <dttm> 2024-05-16 22:00:00, 2024-05-16 22:00:00, 2024-05-16 22:00:00, 2024-05-16 22:00…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start             <dttm> 2024-05-15 22:00:00, 2024-05-15 22:15:00, 2024-05-15 22:30:00, 2024-05-15 22:45…
#> $ ts_point_quantity             <dbl> 5.1, 0.0, 1.6, 0.0, 126.9, 166.4, 129.4, 107.7, 0.0, 0.1, 14.3, 0.5, 0.0, 29.3, …
#> $ ts_quantity_measure_unit_name <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MA…
```
