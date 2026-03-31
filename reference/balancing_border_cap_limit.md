# Balancing Border Capacity Limitations (IFs 4.3 & 4.4)

This data item publish limitations on borders requested by participating
or affected TSOs.

## Usage

``` r
balancing_border_cap_limit(
  eic_in = NULL,
  eic_out = NULL,
  process_type = "A51",
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic_in:

  Energy Identification Code of an in LFC Area (LFA) or in Scheduling
  area (SCA)

- eic_out:

  Energy Identification Code of out an out LFC Area (LFA) or out
  Scheduling area (SCA)

- process_type:

  type of frequency restoration reserve "A47" mFRR "A51" aFRR "A63"
  Imbalance Netting

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
[`shares_of_fcr_capacity()`](https://krose.github.io/entsoeapi/reference/shares_of_fcr_capacity.md),
[`sharing_of_rr_and_frr_capacity()`](https://krose.github.io/entsoeapi/reference/sharing_of_rr_and_frr_capacity.md)

## Examples

``` r
df <- entsoeapi::balancing_border_cap_limit(
  eic_in = "10YDE-RWENET---I",
  eic_out = "10YBE----------2",
  process_type = "A51",
  period_start = lubridate::ymd(x = "2022-06-22", tz = "CET"),
  period_end = lubridate::ymd(x = "2022-06-23", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A31&BusinessType=A26&processType=A51&In_Domain=10YDE-RWENET---I&Out_Domain=10YBE----------2&periodStart=202206212200&periodEnd=202206222200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 31 Mar 2026 06:57:05 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Balancing Border Capacity Limitations_202206212200-202206222200.xml"
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
#> Rows: 96
#> Columns: 24
#> $ domain_mrid            <chr> "10YDOM-REGION-1V", "10YDOM-REGION-1V", "10YDOM-REGION-1V", "10YDOM-REGION-1V", "10YDOM…
#> $ domain_name            <chr> "CWE Region", "CWE Region", "CWE Region", "CWE Region", "CWE Region", "CWE Region", "CW…
#> $ ts_in_domain_mrid      <chr> "10YDE-RWENET---I", "10YDE-RWENET---I", "10YDE-RWENET---I", "10YDE-RWENET---I", "10YDE-…
#> $ ts_in_domain_name      <chr> "Amprion GmbH", "Amprion GmbH", "Amprion GmbH", "Amprion GmbH", "Amprion GmbH", "Amprio…
#> $ ts_out_domain_mrid     <chr> "10YBE----------2", "10YBE----------2", "10YBE----------2", "10YBE----------2", "10YBE-…
#> $ ts_out_domain_name     <chr> "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Belgium",…
#> $ ts_product             <chr> "8716867000016", "8716867000016", "8716867000016", "8716867000016", "8716867000016", "8…
#> $ ts_product_def         <chr> "Active power", "Active power", "Active power", "Active power", "Active power", "Active…
#> $ type                   <chr> "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A3…
#> $ type_def               <chr> "Agreed capacity", "Agreed capacity", "Agreed capacity", "Agreed capacity", "Agreed cap…
#> $ process_type           <chr> "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A5…
#> $ process_type_def       <chr> "Automatic frequency restoration reserve", "Automatic frequency restoration reserve", "…
#> $ ts_business_type       <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A2…
#> $ ts_business_type_def   <chr> "Available transfer capacity (ATC)", "Available transfer capacity (ATC)", "Available tr…
#> $ created_date_time      <dttm> 2026-03-31 06:57:05, 2026-03-31 06:57:05, 2026-03-31 06:57:05, 2026-03-31 06:57:05, 20…
#> $ ts_reason_code         <chr> "B47", "B47", "B47", "B47", "B47", "B47", "B47", "B47", "B47", "B47", "B47", "B47", "B4…
#> $ ts_reason_text         <chr> "Operational security constraints", "Operational security constraints", "Operational se…
#> $ revision_number        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_resolution          <chr> "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M…
#> $ ts_time_interval_start <dttm> 2022-06-21 22:00:00, 2022-06-21 22:00:00, 2022-06-21 22:00:00, 2022-06-21 22:00:00, 20…
#> $ ts_time_interval_end   <dttm> 2022-06-22 22:00:00, 2022-06-22 22:00:00, 2022-06-22 22:00:00, 2022-06-22 22:00:00, 20…
#> $ ts_mrid                <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_point_dt_start      <dttm> 2022-06-21 22:00:00, 2022-06-21 22:15:00, 2022-06-21 22:30:00, 2022-06-21 22:45:00, 20…
#> $ ts_point_quantity      <dbl> 150, 0, 0, 0, 0, 0, 0, 0, 113, 114, 107, 108, 92, 96, 86, 79, 0, 2, 8, 19, 149, 150, 0,…
```
