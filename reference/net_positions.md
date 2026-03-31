# Get Implicit Auction — Net Positions (12.1.E)

Net positions resulting from implicit auctions per bidding zone.

## Usage

``` r
net_positions(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  contract_type = "A01",
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the bidding zone

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- contract_type:

  Contract market agreement type; "A01" = Day ahead "A07" = Intraday
  Defaults to "A01" (Day ahead)

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
[`total_nominated_capacity()`](https://krose.github.io/entsoeapi/reference/total_nominated_capacity.md)

## Examples

``` r
df <- entsoeapi::net_positions(
  eic = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2015-12-31", tz = "CET"),
  period_end = lubridate::ymd(x = "2016-01-01", tz = "CET"),
  contract_type = "A01",
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A25&businessType=B09&contract_MarketAgreement.Type=A01&in_Domain=10YCZ-CEPS-----N&out_Domain=10YCZ-CEPS-----N&periodStart=201512302300&periodEnd=201512312300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 31 Mar 2026 06:58:23 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Implicit Allocations Net Positions Daily_201512302300-201512312300.xml"
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
#> Rows: 24
#> Columns: 21
#> $ ts_in_domain_mrid             <chr> "10YCZ-CEPS-----N", "REGION_CODE-----", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", …
#> $ ts_in_domain_name             <chr> "Czech Republic", NA, "Czech Republic", "Czech Republic", "Czech Republic", NA, …
#> $ ts_out_domain_mrid            <chr> "REGION_CODE-----", "10YCZ-CEPS-----N", "REGION_CODE-----", "REGION_CODE-----", …
#> $ ts_out_domain_name            <chr> NA, "Czech Republic", NA, NA, NA, "Czech Republic", NA, NA, NA, NA, NA, NA, NA, …
#> $ type                          <chr> "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A2…
#> $ type_def                      <chr> "Allocation result document", "Allocation result document", "Allocation result d…
#> $ market_agreement_type         <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A0…
#> $ market_agreement_type_def     <chr> "Daily contract", "Daily contract", "Daily contract", "Daily contract", "Daily c…
#> $ ts_auction_type               <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A0…
#> $ ts_auction_type_def           <chr> "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", "Implici…
#> $ ts_business_type              <chr> "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B0…
#> $ ts_business_type_def          <chr> "Net position", "Net position", "Net position", "Net position", "Net position", …
#> $ created_date_time             <dttm> 2026-03-31 06:58:23, 2026-03-31 06:58:23, 2026-03-31 06:58:23, 2026-03-31 06:58…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_resolution                 <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M",…
#> $ ts_time_interval_start        <dttm> 2015-12-30 23:00:00, 2015-12-31 00:00:00, 2015-12-31 01:00:00, 2015-12-31 01:00:…
#> $ ts_time_interval_end          <dttm> 2015-12-31 00:00:00, 2015-12-31 01:00:00, 2015-12-31 04:00:00, 2015-12-31 04:00…
#> $ ts_mrid                       <dbl> 1, 2, 3, 3, 3, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 7, 7, 7, 7, 7, 7, 7
#> $ ts_point_dt_start             <dttm> 2015-12-30 23:00:00, 2015-12-31 00:00:00, 2015-12-31 01:00:00, 2015-12-31 02:00…
#> $ ts_point_quantity             <dbl> 44.8, 158.4, 65.8, 178.6, 26.2, 63.4, 198.4, 301.9, 157.2, 102.3, 111.6, 129.0, …
#> $ ts_quantity_measure_unit_name <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "M…
```
