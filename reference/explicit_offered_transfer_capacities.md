# Get Explicit Offered Transfer Capacities — All Contract Types (11.1.A)

Convenience wrapper that queries explicit offered transfer capacities
across **all** contract types (day-ahead, weekly, monthly, yearly,
long-term, intraday, quarterly) and returns the combined result.

## Usage

``` r
explicit_offered_transfer_capacities(
  eic_in = NULL,
  eic_out = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  contract_type = "A01",
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
[`auction_revenue()`](https://krose.github.io/entsoeapi/reference/auction_revenue.md),
[`congestion_income()`](https://krose.github.io/entsoeapi/reference/congestion_income.md),
[`continuous_offered_transfer_capacities()`](https://krose.github.io/entsoeapi/reference/continuous_offered_transfer_capacities.md),
[`continuous_offered_transfer_capacity()`](https://krose.github.io/entsoeapi/reference/continuous_offered_transfer_capacity.md),
[`energy_prices()`](https://krose.github.io/entsoeapi/reference/energy_prices.md),
[`explicit_offered_transfer_capacity()`](https://krose.github.io/entsoeapi/reference/explicit_offered_transfer_capacity.md),
[`flow_based_allocations()`](https://krose.github.io/entsoeapi/reference/flow_based_allocations.md),
[`implicit_offered_transfer_capacities()`](https://krose.github.io/entsoeapi/reference/implicit_offered_transfer_capacities.md),
[`implicit_offered_transfer_capacity()`](https://krose.github.io/entsoeapi/reference/implicit_offered_transfer_capacity.md),
[`intraday_prices()`](https://krose.github.io/entsoeapi/reference/intraday_prices.md),
[`net_positions()`](https://krose.github.io/entsoeapi/reference/net_positions.md),
[`total_nominated_capacity()`](https://krose.github.io/entsoeapi/reference/total_nominated_capacity.md)

## Examples

``` r
df <- entsoeapi::explicit_offered_transfer_capacities(
  eic_in = "10YBE----------2",
  eic_out = "10YGB----------A",
  period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
  period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
  contract_type = "A01",
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A31&auction.Type=A02&contract_MarketAgreement.Type=A01&in_Domain=10YBE----------2&out_Domain=10YGB----------A&periodStart=202308152200&periodEnd=202308162200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 08 Apr 2026 13:08:20 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="OFFERED_TRANSFER_CAPACITIES_EXPLICIT_202308152200-202308162200.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!

dplyr::glimpse(df)
#> Rows: 24
#> Columns: 24
#> $ ts_in_domain_mrid                   <chr> "10YBE----------2", "10YBE----------2", "10YBE----------2", "10YBE--------…
#> $ ts_in_domain_name                   <chr> "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Belgium…
#> $ ts_out_domain_mrid                  <chr> "10YGB----------A", "10YGB----------A", "10YGB----------A", "10YGB--------…
#> $ ts_out_domain_name                  <chr> "Great Britain", "Great Britain", "Great Britain", "Great Britain", "Great…
#> $ type                                <chr> "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31…
#> $ type_def                            <chr> "Agreed capacity", "Agreed capacity", "Agreed capacity", "Agreed capacity"…
#> $ market_agreement_type               <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01…
#> $ market_agreement_type_def           <chr> "Daily contract", "Daily contract", "Daily contract", "Daily contract", "D…
#> $ ts_auction_type                     <chr> "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02…
#> $ ts_auction_type_def                 <chr> "Explicit", "Explicit", "Explicit", "Explicit", "Explicit", "Explicit", "E…
#> $ ts_auction_category                 <chr> "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04…
#> $ ts_auction_category_def             <chr> "Hourly", "Hourly", "Hourly", "Hourly", "Hourly", "Hourly", "Hourly", "Hou…
#> $ ts_business_type                    <chr> "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31…
#> $ ts_business_type_def                <chr> "Offered Capacity", "Offered Capacity", "Offered Capacity", "Offered Capac…
#> $ created_date_time                   <dttm> 2026-04-08 13:08:20, 2026-04-08 13:08:20, 2026-04-08 13:08:20, 2026-04-08…
#> $ revision_number                     <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_resolution                       <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "P…
#> $ ts_time_interval_start              <dttm> 2023-08-15 22:00:00, 2023-08-15 22:00:00, 2023-08-15 22:00:00, 2023-08-15 …
#> $ ts_time_interval_end                <dttm> 2023-08-16 22:00:00, 2023-08-16 22:00:00, 2023-08-16 22:00:00, 2023-08-16…
#> $ ts_mrid                             <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_point_dt_start                   <dttm> 2023-08-15 22:00:00, 2023-08-15 23:00:00, 2023-08-16 00:00:00, 2023-08-16…
#> $ ts_point_quantity                   <dbl> 1012, 1012, 1012, 1012, 1012, 1012, 1037, 1071, 1096, 1012, 1012, 1062, 10…
#> $ ts_quantity_measure_unit_name       <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MA…
#> $ ts_classification_sequence_position <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
```
