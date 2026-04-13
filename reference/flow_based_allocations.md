# Get Flow Based Allocations (11.1.B)

Flow based capacity allocated, for all time horizons.

## Usage

``` r
flow_based_allocations(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  process_type = "A43",
  archive = FALSE,
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the area

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- process_type:

  Contract market agreement type, valid values can be checked from
  process_types table; "A32" = Month-ahead "A33" = Year-ahead "A43" =
  Day ahead "A44" = Intraday Defaults to "A43" (Day ahead)

- archive:

  Defaults to FALSE, set to TRUE if archives to be queried.

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
[`implicit_offered_transfer_capacities()`](https://krose.github.io/entsoeapi/reference/implicit_offered_transfer_capacities.md),
[`implicit_offered_transfer_capacity()`](https://krose.github.io/entsoeapi/reference/implicit_offered_transfer_capacity.md),
[`intraday_prices()`](https://krose.github.io/entsoeapi/reference/intraday_prices.md),
[`net_positions()`](https://krose.github.io/entsoeapi/reference/net_positions.md),
[`total_nominated_capacity()`](https://krose.github.io/entsoeapi/reference/total_nominated_capacity.md)

## Examples

``` r
df <- entsoeapi::flow_based_allocations(
  eic = "10YDOM-REGION-1V",
  period_start = lubridate::ymd(x = "2018-12-31", tz = "CET"),
  period_end = lubridate::ymd(x = "2019-01-01", tz = "CET"),
  process_type = "A32",
  archive = TRUE,
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=B09&processType=A32&StorageType=archive&in_Domain=10YDOM-REGION-1V&out_Domain=10YDOM-REGION-1V&periodStart=201812302300&periodEnd=201812312300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:12:06 GMT
#> <- content-type: application/zip
#> <- content-disposition: attachment; filename="DayAhead_CWE_20181230T2300Z_20181231T2300Z.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpptwzjC/DayAhead_CWE_20181230T2300Z_20181231T2300Z.xml has been read in
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!

dplyr::glimpse(df)
#> Rows: 9,440
#> Columns: 21
#> $ domain_mrid                                                     <chr> "10YDOM-REGION-1V", "10YDOM-REGION-1V", "10YDO…
#> $ domain_name                                                     <chr> "CWE Region", "CWE Region", "CWE Region", "CWE…
#> $ type                                                            <chr> "B11", "B11", "B11", "B11", "B11", "B11", "B11…
#> $ type_def                                                        <chr> "Anonymized flow based parameters publication"…
#> $ process_type                                                    <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01…
#> $ process_type_def                                                <chr> "Day ahead", "Day ahead", "Day ahead", "Day ah…
#> $ ts_business_type                                                <chr> "B39", "B39", "B39", "B39", "B39", "B39", "B39…
#> $ ts_business_type_def                                            <chr> "Flow based domain adjusted to long term sched…
#> $ created_date_time                                               <dttm> 2024-11-25 14:44:31, 2024-11-25 14:44:31, 202…
#> $ revision_number                                                 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ time_period_time_interval_start                                 <dttm> 2018-12-30 23:00:00, 2018-12-30 23:00:00, 201…
#> $ time_period_time_interval_end                                   <dttm> 2018-12-31 23:00:00, 2018-12-31 23:00:00, 201…
#> $ ts_resolution                                                   <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "…
#> $ ts_time_interval_start                                          <dttm> 2018-12-30 23:00:00, 2018-12-30 23:00:00, 201…
#> $ ts_time_interval_end                                            <dttm> 2018-12-31 23:00:00, 2018-12-31 23:00:00, 201…
#> $ ts_mrid                                                         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ ts_point_dt_start                                               <dttm> 2018-12-30 23:00:00, 2018-12-30 23:00:00, 201…
#> $ constraint_ts_monitored_ptdf_domain_mrid                        <chr> "10YAT-APG------L", "10YBE----------2", "10Y10…
#> $ constraint_ts_monitored_ptdf_domain_name                        <chr> "Austria", "Belgium", "Germany_Luxemburg", "Fr…
#> $ constraint_ts_monitored_ptdf_domain_quantity                    <dbl> 0.12228, -0.04419, -0.02456, -0.05096, -0.0400…
#> $ constraint_ts_monitored_flow_based_study_domain_margin_quantity <dbl> 888, 888, 888, 888, 888, 395, 395, 395, 395, 3…
```
