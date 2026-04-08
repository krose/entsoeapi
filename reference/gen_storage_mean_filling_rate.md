# Get Weekly Average Filling Rate of Water Reservoirs and Hydro Storage Plants (16.1.D)

Aggregated weekly average filling rate of all water reservoir and hydro
storage plants (MWh) per area, including the same week value of the
previous year.

## Usage

``` r
gen_storage_mean_filling_rate(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the control area, bidding zone or
  country

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format Maximum 380 days range limit
  applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format Maximum 380 days range limit
  applies

- tidy_output:

  Defaults to TRUE. If TRUE, then flatten nested tables.

- security_token:

  Security token for ENTSO-E transparency platform

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with the queried data.

## See also

Other generation endpoints:
[`gen_day_ahead_forecast()`](https://krose.github.io/entsoeapi/reference/gen_day_ahead_forecast.md),
[`gen_installed_capacity_per_pt()`](https://krose.github.io/entsoeapi/reference/gen_installed_capacity_per_pt.md),
[`gen_installed_capacity_per_pu()`](https://krose.github.io/entsoeapi/reference/gen_installed_capacity_per_pu.md),
[`gen_per_gen_unit()`](https://krose.github.io/entsoeapi/reference/gen_per_gen_unit.md),
[`gen_per_prod_type()`](https://krose.github.io/entsoeapi/reference/gen_per_prod_type.md),
[`gen_wind_solar_forecasts()`](https://krose.github.io/entsoeapi/reference/gen_wind_solar_forecasts.md)

## Examples

``` r
df <- entsoeapi::gen_storage_mean_filling_rate(
  eic          = "10YFR-RTE------C",
  period_start = lubridate::ymd(x = "2020-01-01", tz = "CET"),
  period_end   = lubridate::ymd(x = "2020-01-31", tz = "CET"),
  tidy_output  = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A72&processType=A16&in_Domain=10YFR-RTE------C&periodStart=201912312300&periodEnd=202001302300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 08 Apr 2026 13:14:39 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Aggregate Filling Rate of Water Reservoirs_201912312300-202001302300.xml"
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
#> Rows: 5
#> Columns: 21
#> $ ts_in_bidding_zone_domain_mrid  <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C"…
#> $ ts_in_bidding_zone_domain_name  <chr> "France", "France", "France", "France", "France"
#> $ type                            <chr> "A72", "A72", "A72", "A72", "A72"
#> $ type_def                        <chr> "Reservoir filling information", "Reservoir filling information", "Reservoir f…
#> $ process_type                    <chr> "A16", "A16", "A16", "A16", "A16"
#> $ process_type_def                <chr> "Realised", "Realised", "Realised", "Realised", "Realised"
#> $ ts_object_aggregation           <chr> "A01", "A01", "A01", "A01", "A01"
#> $ ts_object_aggregation_def       <chr> "Area", "Area", "Area", "Area", "Area"
#> $ ts_business_type                <chr> "A01", "A01", "A01", "A01", "A01"
#> $ ts_business_type_def            <chr> "Production", "Production", "Production", "Production", "Production"
#> $ created_date_time               <dttm> 2026-04-08 13:14:39, 2026-04-08 13:14:39, 2026-04-08 13:14:39, 2026-04-08 13:1…
#> $ revision_number                 <dbl> 1, 1, 1, 1, 1
#> $ time_period_time_interval_start <dttm> 2019-12-29 23:00:00, 2019-12-29 23:00:00, 2019-12-29 23:00:00, 2019-12-29 23:0…
#> $ time_period_time_interval_end   <dttm> 2020-02-02 23:00:00, 2020-02-02 23:00:00, 2020-02-02 23:00:00, 2020-02-02 23:0…
#> $ ts_resolution                   <chr> "P7D", "P7D", "P7D", "P7D", "P7D"
#> $ ts_time_interval_start          <dttm> 2019-12-29 23:00:00, 2019-12-29 23:00:00, 2019-12-29 23:00:00, 2019-12-29 23:0…
#> $ ts_time_interval_end            <dttm> 2020-02-02 23:00:00, 2020-02-02 23:00:00, 2020-02-02 23:00:00, 2020-02-02 23:0…
#> $ ts_mrid                         <dbl> 1, 1, 1, 1, 1
#> $ ts_point_dt_start               <dttm> 2019-12-29 23:00:00, 2020-01-05 23:00:00, 2020-01-12 23:00:00, 2020-01-19 23:0…
#> $ ts_point_quantity               <dbl> 2780215, 2742325, 2652974, 2426891, 2194236
#> $ ts_quantity_measure_unit_name   <chr> "MWH", "MWH", "MWH", "MWH", "MWH"
```
