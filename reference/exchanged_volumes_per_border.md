# Get Netted & Exchanged Volumes Per Border (IFs 3.10, 3.16 & 3.17)

Netted and exchanged balancing energy volumes on a specific border
between an acquiring and a connecting domain.

## Usage

``` r
exchanged_volumes_per_border(
  acquiring_eic = NULL,
  connecting_eic = NULL,
  process_type = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- acquiring_eic:

  Energy Identification Code of the acquiring area

- connecting_eic:

  Energy Identification Code of the connecting area

- process_type:

  type of frequency restoration reserve "A51" aFRR "A60" mFRR with
  scheduled activation "A61" mFRR with direct activation

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format One day range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format One day range limit applies

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
[`fcr_total_capacity()`](https://krose.github.io/entsoeapi/reference/fcr_total_capacity.md),
[`financial_expenses_and_income()`](https://krose.github.io/entsoeapi/reference/financial_expenses_and_income.md),
[`hvdc_link_constrains()`](https://krose.github.io/entsoeapi/reference/hvdc_link_constrains.md),
[`imbalance_prices()`](https://krose.github.io/entsoeapi/reference/imbalance_prices.md),
[`imbalance_volumes()`](https://krose.github.io/entsoeapi/reference/imbalance_volumes.md),
[`netted_volumes()`](https://krose.github.io/entsoeapi/reference/netted_volumes.md),
[`netted_volumes_per_border()`](https://krose.github.io/entsoeapi/reference/netted_volumes_per_border.md),
[`procured_balancing_capacity()`](https://krose.github.io/entsoeapi/reference/procured_balancing_capacity.md),
[`rr_and_frr_actual_capacity()`](https://krose.github.io/entsoeapi/reference/rr_and_frr_actual_capacity.md),
[`shares_of_fcr_capacity()`](https://krose.github.io/entsoeapi/reference/shares_of_fcr_capacity.md),
[`sharing_of_rr_and_frr_capacity()`](https://krose.github.io/entsoeapi/reference/sharing_of_rr_and_frr_capacity.md)

## Examples

``` r
df <- entsoeapi::exchanged_volumes_per_border(
  acquiring_eic = "10YBE----------2",
  connecting_eic = "10YFR-RTE------C",
  process_type = "A60",
  period_start = lubridate::ymd(x = "2025-03-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2025-03-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A30&processType=A60&Acquiring_domain=10YBE----------2&Connecting_Domain=10YFR-RTE------C&periodStart=202502282300&periodEnd=202503012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:12:02 GMT
#> <- content-type: application/zip
#> <- content-length: 1179
#> <- content-disposition: attachment; filename="Netted and Exchanged Volumes per Border_202502282300-202503012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpptwzjC/001-NETTED_AND_EXCHANGED_VOLUMES_PER_BORDER_202502282300-202503012300.xml has been read in
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!

dplyr::glimpse(df)
#> Rows: 96
#> Columns: 21
#> $ area_domain_mrid              <chr> "10Y1001C--00085O", "10Y1001C--00085O", "10Y1001C--00085O", "10Y1001C--00085O", …
#> $ area_domain_name              <chr> "mFRR region", "mFRR region", "mFRR region", "mFRR region", "mFRR region", "mFRR…
#> $ ts_connecting_domain_mrid     <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", …
#> $ ts_connecting_domain_name     <chr> "France", "France", "France", "France", "France", "France", "France", "France", …
#> $ ts_acquiring_domain_mrid      <chr> "10YBE----------2", "10YBE----------2", "10YBE----------2", "10YBE----------2", …
#> $ ts_acquiring_domain_name      <chr> "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Be…
#> $ type                          <chr> "A30", "A30", "A30", "A30", "A30", "A30", "A30", "A30", "A30", "A30", "A30", "A3…
#> $ type_def                      <chr> "Cross border schedule", "Cross border schedule", "Cross border schedule", "Cros…
#> $ process_type                  <chr> "A60", "A60", "A60", "A60", "A60", "A60", "A60", "A60", "A60", "A60", "A60", "A6…
#> $ process_type_def              <chr> "Scheduled activation mFRR", "Scheduled activation mFRR", "Scheduled activation …
#> $ ts_business_type              <chr> "A45", "A45", "A45", "A45", "A45", "A45", "A45", "A45", "A45", "A45", "A45", "A4…
#> $ ts_business_type_def          <chr> "Schedule activated reserves", "Schedule activated reserves", "Schedule activate…
#> $ created_date_time             <dttm> 2026-04-13 08:12:02, 2026-04-13 08:12:02, 2026-04-13 08:12:02, 2026-04-13 08:12…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution                 <chr> "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M",…
#> $ ts_time_interval_start        <dttm> 2025-02-28 23:00:00, 2025-02-28 23:00:00, 2025-02-28 23:00:00, 2025-02-28 23:00…
#> $ ts_time_interval_end          <dttm> 2025-03-01 23:00:00, 2025-03-01 23:00:00, 2025-03-01 23:00:00, 2025-03-01 23:00…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start             <dttm> 2025-02-28 23:00:00, 2025-02-28 23:15:00, 2025-02-28 23:30:00, 2025-02-28 23:45…
#> $ ts_point_quantity             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ ts_quantity_measure_unit_name <chr> "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MW…
```
