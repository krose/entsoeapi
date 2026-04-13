# Get Implicit Offered Transfer Capacities — Day-Ahead & Intraday (11.1)

Convenience wrapper that queries the day-ahead (A01) or intraday (A07)
implicit offered transfer capacities.

## Usage

``` r
implicit_offered_transfer_capacities(
  eic_in = NULL,
  eic_out = NULL,
  period_start = ymd(x = Sys.Date() - days(x = 1L), tz = "CET"),
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
[`energy_prices()`](https://krose.github.io/entsoeapi/reference/energy_prices.md),
[`explicit_offered_transfer_capacities()`](https://krose.github.io/entsoeapi/reference/explicit_offered_transfer_capacities.md),
[`explicit_offered_transfer_capacity()`](https://krose.github.io/entsoeapi/reference/explicit_offered_transfer_capacity.md),
[`flow_based_allocations()`](https://krose.github.io/entsoeapi/reference/flow_based_allocations.md),
[`implicit_offered_transfer_capacity()`](https://krose.github.io/entsoeapi/reference/implicit_offered_transfer_capacity.md),
[`intraday_prices()`](https://krose.github.io/entsoeapi/reference/intraday_prices.md),
[`net_positions()`](https://krose.github.io/entsoeapi/reference/net_positions.md),
[`total_nominated_capacity()`](https://krose.github.io/entsoeapi/reference/total_nominated_capacity.md)

## Examples

``` r
df1 <- entsoeapi::implicit_offered_transfer_capacities(
  eic_in = "10Y1001A1001A82H",
  eic_out = "10YDK-1--------W",
  period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
  period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
  contract_type = "A01",
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A31&auction.Type=A01&contract_MarketAgreement.Type=A01&in_Domain=10Y1001A1001A82H&out_Domain=10YDK-1--------W&periodStart=202308152200&periodEnd=202308162200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:51:50 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="OFFERED_TRANSFER_CAPACITIES_IMPLICIT_202308152200-202308162200.xml"
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
#> Rows: 24
#> Columns: 21
#> $ ts_in_domain_mrid             <chr> "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A82H", …
#> $ ts_in_domain_name             <chr> "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Luxembur…
#> $ ts_out_domain_mrid            <chr> "10YDK-1--------W", "10YDK-1--------W", "10YDK-1--------W", "10YDK-1--------W", …
#> $ ts_out_domain_name            <chr> "Denmark DK1", "Denmark DK1", "Denmark DK1", "Denmark DK1", "Denmark DK1", "Denm…
#> $ type                          <chr> "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A3…
#> $ type_def                      <chr> "Agreed capacity", "Agreed capacity", "Agreed capacity", "Agreed capacity", "Agr…
#> $ market_agreement_type         <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A0…
#> $ market_agreement_type_def     <chr> "Daily contract", "Daily contract", "Daily contract", "Daily contract", "Daily c…
#> $ ts_auction_type               <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A0…
#> $ ts_auction_type_def           <chr> "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", "Implici…
#> $ ts_business_type              <chr> "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A3…
#> $ ts_business_type_def          <chr> "Offered Capacity", "Offered Capacity", "Offered Capacity", "Offered Capacity", …
#> $ created_date_time             <dttm> 2026-04-13 08:51:50, 2026-04-13 08:51:50, 2026-04-13 08:51:50, 2026-04-13 08:51…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_resolution                 <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M",…
#> $ ts_time_interval_start        <dttm> 2023-08-15 22:00:00, 2023-08-15 22:00:00, 2023-08-15 22:00:00, 2023-08-15 22:00:…
#> $ ts_time_interval_end          <dttm> 2023-08-16 22:00:00, 2023-08-16 22:00:00, 2023-08-16 22:00:00, 2023-08-16 22:00…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_point_dt_start             <dttm> 2023-08-15 22:00:00, 2023-08-15 23:00:00, 2023-08-16 00:00:00, 2023-08-16 01:00…
#> $ ts_point_quantity             <dbl> 2500, 2500, 2500, 2500, 2500, 2500, 2500, 2500, 2500, 2500, 2380, 2190, 2060, 19…
#> $ ts_quantity_measure_unit_name <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "M…

df2 <- entsoeapi::implicit_offered_transfer_capacities(
  eic_in = "10YFR-RTE------C",
  eic_out = "10YCH-SWISSGRIDZ",
  period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
  period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
  contract_type = "A07",
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A31&auction.Type=A01&contract_MarketAgreement.Type=A07&in_Domain=10YFR-RTE------C&out_Domain=10YCH-SWISSGRIDZ&periodStart=202308152200&periodEnd=202308162200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:51:50 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="OFFERED_TRANSFER_CAPACITIES_IMPLICIT_202308152200-202308162200.xml"
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
#> Rows: 48
#> Columns: 21
#> $ ts_in_domain_mrid             <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", …
#> $ ts_in_domain_name             <chr> "France", "France", "France", "France", "France", "France", "France", "France", …
#> $ ts_out_domain_mrid            <chr> "10YCH-SWISSGRIDZ", "10YCH-SWISSGRIDZ", "10YCH-SWISSGRIDZ", "10YCH-SWISSGRIDZ", …
#> $ ts_out_domain_name            <chr> "Switzerland", "Switzerland", "Switzerland", "Switzerland", "Switzerland", "Swit…
#> $ type                          <chr> "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A3…
#> $ type_def                      <chr> "Agreed capacity", "Agreed capacity", "Agreed capacity", "Agreed capacity", "Agr…
#> $ market_agreement_type         <chr> "A07", "A07", "A07", "A07", "A07", "A07", "A07", "A07", "A07", "A07", "A07", "A0…
#> $ market_agreement_type_def     <chr> "Intraday contract", "Intraday contract", "Intraday contract", "Intraday contrac…
#> $ ts_auction_type               <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A0…
#> $ ts_auction_type_def           <chr> "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", "Implici…
#> $ ts_business_type              <chr> "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A3…
#> $ ts_business_type_def          <chr> "Offered Capacity", "Offered Capacity", "Offered Capacity", "Offered Capacity", …
#> $ created_date_time             <dttm> 2026-04-13 08:51:50, 2026-04-13 08:51:50, 2026-04-13 08:51:50, 2026-04-13 08:51…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution                 <chr> "PT30M", "PT30M", "PT30M", "PT30M", "PT30M", "PT30M", "PT30M", "PT30M", "PT30M",…
#> $ ts_time_interval_start        <dttm> 2023-08-15 22:00:00, 2023-08-15 22:00:00, 2023-08-15 22:00:00, 2023-08-15 22:00…
#> $ ts_time_interval_end          <dttm> 2023-08-16 22:00:00, 2023-08-16 22:00:00, 2023-08-16 22:00:00, 2023-08-16 22:00…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start             <dttm> 2023-08-15 22:00:00, 2023-08-15 22:30:00, 2023-08-15 23:00:00, 2023-08-15 23:30…
#> $ ts_point_quantity             <dbl> 2686.7, 2897.9, 3026.6, 2897.9, 2725.7, 2897.9, 3027.0, 2897.9, 2897.9, 2897.9, …
#> $ ts_quantity_measure_unit_name <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MA…
```
