# Get Total Nominated Capacity (12.1.B)

Aggregated capacity nominated by market participants from time horizons
(including Intra-Day) corresponding to explicit allocations, agreed
between the TSOs and confirmed to the market.

## Usage

``` r
total_nominated_capacity(
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

  Energy Identification Code of in domain

- eic_out:

  Energy Identification Code of out domain

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
[`continuous_offered_transfer_capacities()`](https://krose.github.io/entsoeapi/reference/continuous_offered_transfer_capacities.md),
[`continuous_offered_transfer_capacity()`](https://krose.github.io/entsoeapi/reference/continuous_offered_transfer_capacity.md),
[`energy_prices()`](https://krose.github.io/entsoeapi/reference/energy_prices.md),
[`explicit_offered_transfer_capacities()`](https://krose.github.io/entsoeapi/reference/explicit_offered_transfer_capacities.md),
[`explicit_offered_transfer_capacity()`](https://krose.github.io/entsoeapi/reference/explicit_offered_transfer_capacity.md),
[`flow_based_allocations()`](https://krose.github.io/entsoeapi/reference/flow_based_allocations.md),
[`implicit_offered_transfer_capacities()`](https://krose.github.io/entsoeapi/reference/implicit_offered_transfer_capacities.md),
[`implicit_offered_transfer_capacity()`](https://krose.github.io/entsoeapi/reference/implicit_offered_transfer_capacity.md),
[`intraday_prices()`](https://krose.github.io/entsoeapi/reference/intraday_prices.md),
[`net_positions()`](https://krose.github.io/entsoeapi/reference/net_positions.md)

## Examples

``` r
df <- entsoeapi::total_nominated_capacity(
  eic_in = "10YDE-VE-------2",
  eic_out = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2019-02-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2019-03-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A26&businessType=B08&in_Domain=10YDE-VE-------2&out_Domain=10YCZ-CEPS-----N&periodStart=201901312300&periodEnd=201902282300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 25 Mar 2026 19:10:32 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Total Capacity Nominated_201901312300-201902282300.xml"
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
#> Rows: 2,016
#> Columns: 19
#> $ ts_in_domain_mrid             <chr> "10YDE-VE-------2", "10YDE-VE-------2", "10YDE-VE-------2", "10YDE-VE-------2", …
#> $ ts_in_domain_name             <chr> "Germany 50Hertz", "Germany 50Hertz", "Germany 50Hertz", "Germany 50Hertz", "Ger…
#> $ ts_out_domain_mrid            <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", …
#> $ ts_out_domain_name            <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", "Czech R…
#> $ type                          <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A2…
#> $ type_def                      <chr> "Capacity document", "Capacity document", "Capacity document", "Capacity documen…
#> $ market_agreement_type         <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A0…
#> $ market_agreement_type_def     <chr> "Daily contract", "Daily contract", "Daily contract", "Daily contract", "Daily c…
#> $ ts_business_type              <chr> "B08", "B08", "B08", "B08", "B08", "B08", "B08", "B08", "B08", "B08", "B08", "B0…
#> $ ts_business_type_def          <chr> "Total nominated capacity", "Total nominated capacity", "Total nominated capacit…
#> $ created_date_time             <dttm> 2026-03-25 19:10:32, 2026-03-25 19:10:32, 2026-03-25 19:10:32, 2026-03-25 19:10…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution                 <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M",…
#> $ ts_time_interval_start        <dttm> 2019-01-31 23:00:00, 2019-01-31 23:00:00, 2019-01-31 23:00:00, 2019-01-31 23:00…
#> $ ts_time_interval_end          <dttm> 2019-02-28 23:00:00, 2019-02-28 23:00:00, 2019-02-28 23:00:00, 2019-02-28 23:00…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start             <dttm> 2019-01-31 23:00:00, 2019-02-01 00:00:00, 2019-02-01 01:00:00, 2019-02-01 02:00…
#> $ ts_point_quantity             <dbl> 123, 0, 405, 405, 405, 405, 405, 405, 405, 405, 405, 405, 405, 405, 405, 405, 40…
#> $ ts_quantity_measure_unit_name <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MA…
```
