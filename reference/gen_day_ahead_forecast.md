# Get Day-Ahead Generation Forecast. (14.1.C)

It is an estimate of the total scheduled net generation (MW) per area
and market time unit of the following day.

## Usage

``` r
gen_day_ahead_forecast(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
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

Other generation endpoints:
[`gen_installed_capacity_per_pt()`](https://krose.github.io/entsoeapi/reference/gen_installed_capacity_per_pt.md),
[`gen_installed_capacity_per_pu()`](https://krose.github.io/entsoeapi/reference/gen_installed_capacity_per_pu.md),
[`gen_per_gen_unit()`](https://krose.github.io/entsoeapi/reference/gen_per_gen_unit.md),
[`gen_per_prod_type()`](https://krose.github.io/entsoeapi/reference/gen_per_prod_type.md),
[`gen_storage_mean_filling_rate()`](https://krose.github.io/entsoeapi/reference/gen_storage_mean_filling_rate.md),
[`gen_wind_solar_forecasts()`](https://krose.github.io/entsoeapi/reference/gen_wind_solar_forecasts.md)

## Examples

``` r
df <- entsoeapi::gen_day_ahead_forecast(
  eic          = "10YFR-RTE------C",
  period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
  period_end   = lubridate::ymd(x = "2020-03-01", tz = "CET"),
  tidy_output  = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A71&processType=A01&in_Domain=10YFR-RTE------C&periodStart=202001312300&periodEnd=202002292300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 31 Mar 2026 06:57:50 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Day Ahead Aggregated Generation_202001312300-202002292300.xml"
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
#> Rows: 696
#> Columns: 21
#> $ ts_in_bidding_zone_domain_mrid  <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C"…
#> $ ts_in_bidding_zone_domain_name  <chr> "France", "France", "France", "France", "France", "France", "France", "France"…
#> $ type                            <chr> "A71", "A71", "A71", "A71", "A71", "A71", "A71", "A71", "A71", "A71", "A71", "…
#> $ type_def                        <chr> "Generation forecast", "Generation forecast", "Generation forecast", "Generati…
#> $ process_type                    <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "…
#> $ process_type_def                <chr> "Day ahead", "Day ahead", "Day ahead", "Day ahead", "Day ahead", "Day ahead", …
#> $ ts_object_aggregation           <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "…
#> $ ts_object_aggregation_def       <chr> "Area", "Area", "Area", "Area", "Area", "Area", "Area", "Area", "Area", "Area"…
#> $ ts_business_type                <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "…
#> $ ts_business_type_def            <chr> "Production", "Production", "Production", "Production", "Production", "Product…
#> $ created_date_time               <dttm> 2026-03-31 06:57:50, 2026-03-31 06:57:50, 2026-03-31 06:57:50, 2026-03-31 06:…
#> $ revision_number                 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ time_period_time_interval_start <dttm> 2020-01-31 23:00:00, 2020-01-31 23:00:00, 2020-01-31 23:00:00, 2020-01-31 23:…
#> $ time_period_time_interval_end   <dttm> 2020-02-29 23:00:00, 2020-02-29 23:00:00, 2020-02-29 23:00:00, 2020-02-29 23:…
#> $ ts_resolution                   <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M…
#> $ ts_time_interval_start          <dttm> 2020-01-31 23:00:00, 2020-01-31 23:00:00, 2020-01-31 23:00:00, 2020-01-31 23:…
#> $ ts_time_interval_end            <dttm> 2020-02-29 23:00:00, 2020-02-29 23:00:00, 2020-02-29 23:00:00, 2020-02-29 23:…
#> $ ts_mrid                         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_point_dt_start               <dttm> 2020-01-31 23:00:00, 2020-02-01 00:00:00, 2020-02-01 01:00:00, 2020-02-01 02:…
#> $ ts_point_quantity               <dbl> 64774.0, 61374.5, 58352.5, 56234.0, 55570.0, 56280.5, 58830.5, 62888.5, 66975.…
#> $ ts_quantity_measure_unit_name   <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "…
```
