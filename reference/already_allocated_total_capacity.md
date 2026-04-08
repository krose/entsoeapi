# Get Already Allocated Total Capacity (12.1.C)

Total capacity allocated, for all time horizons (including Intra-Day)
after each allocation process per market time unit.

## Usage

``` r
already_allocated_total_capacity(
  eic_in = NULL,
  eic_out = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  auction_category = "A04",
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

- auction_category:

  A01 = Base A02 = Peak A03 = Off Peak A04 = Hourly Defaults to "A04"
  (Hourly)

- contract_type:

  Contract market agreement type, valid values can be checked from
  contract_types table; A01 = Daily A02 = Weekly A03 = Monthly A04 =
  Yearly A06 = Long Term A07 = Intraday A08 = Quarterly Defaults to
  "A01" (Daily)

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
df <- entsoeapi::already_allocated_total_capacity(
  eic_in = "10YDE-VE-------2",
  eic_out = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2025-02-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2025-02-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A26&businessType=A29&contract_MarketAgreement.Type=A01&auction.Category=A04&in_Domain=10YDE-VE-------2&out_Domain=10YCZ-CEPS-----N&periodStart=202501312300&periodEnd=202502012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 08 Apr 2026 13:07:54 GMT
#> <- content-type: text/xml
#> <- content-length: 1581
#> <- content-disposition: inline; filename="TOTAL_CAPACITY_ALLOCATED_202501312300-202502012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!

dplyr::glimpse(df)
#> Rows: 24
#> Columns: 23
#> $ ts_in_domain_mrid             <chr> "10YDE-VE-------2", "10YDE-VE-------2", "10YDE-VE-------2", "10YDE-VE-------2", …
#> $ ts_in_domain_name             <chr> "Germany 50Hertz", "Germany 50Hertz", "Germany 50Hertz", "Germany 50Hertz", "Ger…
#> $ ts_out_domain_mrid            <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", …
#> $ ts_out_domain_name            <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", "Czech R…
#> $ type                          <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A2…
#> $ type_def                      <chr> "Capacity document", "Capacity document", "Capacity document", "Capacity documen…
#> $ market_agreement_type         <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A0…
#> $ market_agreement_type_def     <chr> "Daily contract", "Daily contract", "Daily contract", "Daily contract", "Daily c…
#> $ ts_auction_type               <chr> "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A0…
#> $ ts_auction_type_def           <chr> "Explicit", "Explicit", "Explicit", "Explicit", "Explicit", "Explicit", "Explici…
#> $ ts_auction_category           <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A0…
#> $ ts_auction_category_def       <chr> "Base", "Base", "Base", "Base", "Base", "Base", "Base", "Base", "Base", "Base", …
#> $ ts_business_type              <chr> "A29", "A29", "A29", "A29", "A29", "A29", "A29", "A29", "A29", "A29", "A29", "A2…
#> $ ts_business_type_def          <chr> "Already allocated capacity (AAC)", "Already allocated capacity (AAC)", "Already…
#> $ created_date_time             <dttm> 2026-04-08 13:07:54, 2026-04-08 13:07:54, 2026-04-08 13:07:54, 2026-04-08 13:07…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_resolution                 <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M",…
#> $ ts_time_interval_start        <dttm> 2025-01-31 23:00:00, 2025-01-31 23:00:00, 2025-01-31 23:00:00, 2025-01-31 23:00:…
#> $ ts_time_interval_end          <dttm> 2025-02-01 23:00:00, 2025-02-01 23:00:00, 2025-02-01 23:00:00, 2025-02-01 23:00…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_point_dt_start             <dttm> 2025-01-31 23:00:00, 2025-02-01 00:00:00, 2025-02-01 01:00:00, 2025-02-01 02:00…
#> $ ts_point_quantity             <dbl> 449, 449, 449, 449, 449, 449, 449, 449, 449, 449, 449, 449, 449, 449, 449, 449, …
#> $ ts_quantity_measure_unit_name <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "M…
```
