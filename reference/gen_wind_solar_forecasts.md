# Get Generation Forecasts for Wind & Solar (14.1.D)

A respective forecast of wind and solar power net generation (MW) per
area and each market time unit of the following/current day.

## Usage

``` r
gen_wind_solar_forecasts(
  eic = NULL,
  period_start = ymd(Sys.Date(), tz = "CET") - days(x = 1L),
  period_end = ymd(Sys.Date(), tz = "CET"),
  process_type = "A18",
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

- process_type:

  type of process "A01" Day-ahead "A18" Current "A40" Intraday Defaults
  to "A18"

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
[`gen_storage_mean_filling_rate()`](https://krose.github.io/entsoeapi/reference/gen_storage_mean_filling_rate.md)

## Examples

``` r
df <- entsoeapi::gen_wind_solar_forecasts(
  eic          = "10YFR-RTE------C",
  period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
  period_end   = lubridate::ymd(x = "2020-03-01", tz = "CET"),
  process_type = "A01",
  tidy_output  = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A69&processType=A01&in_Domain=10YFR-RTE------C&periodStart=202001312300&periodEnd=202002292300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 25 Mar 2026 19:09:38 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Generation Forecasts Wind Solar_202001312300-202002292300.xml"
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
#> Rows: 1,392
#> Columns: 23
#> $ ts_in_bidding_zone_domain_mrid  <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C"…
#> $ ts_in_bidding_zone_domain_name  <chr> "France", "France", "France", "France", "France", "France", "France", "France"…
#> $ type                            <chr> "A69", "A69", "A69", "A69", "A69", "A69", "A69", "A69", "A69", "A69", "A69", "…
#> $ type_def                        <chr> "Wind and solar forecast", "Wind and solar forecast", "Wind and solar forecast…
#> $ process_type                    <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "…
#> $ process_type_def                <chr> "Day ahead", "Day ahead", "Day ahead", "Day ahead", "Day ahead", "Day ahead", …
#> $ ts_object_aggregation           <chr> "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "…
#> $ ts_object_aggregation_def       <chr> "Resource type", "Resource type", "Resource type", "Resource type", "Resource …
#> $ ts_business_type                <chr> "A94", "A94", "A94", "A94", "A94", "A94", "A94", "A94", "A94", "A94", "A94", "…
#> $ ts_business_type_def            <chr> "Solar generation", "Solar generation", "Solar generation", "Solar generation"…
#> $ ts_mkt_psr_type                 <chr> "B16", "B16", "B16", "B16", "B16", "B16", "B16", "B16", "B16", "B16", "B16", "…
#> $ ts_mkt_psr_type_def             <chr> "Solar unspecified", "Solar unspecified", "Solar unspecified", "Solar unspecif…
#> $ created_date_time               <dttm> 2026-03-25 19:09:38, 2026-03-25 19:09:38, 2026-03-25 19:09:38, 2026-03-25 19:…
#> $ revision_number                 <dbl> 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, …
#> $ time_period_time_interval_start <dttm> 2020-01-31 23:00:00, 2020-01-31 23:00:00, 2020-01-31 23:00:00, 2020-01-31 23:…
#> $ time_period_time_interval_end   <dttm> 2020-02-29 23:00:00, 2020-02-29 23:00:00, 2020-02-29 23:00:00, 2020-02-29 23:…
#> $ ts_resolution                   <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M…
#> $ ts_time_interval_start          <dttm> 2020-01-31 23:00:00, 2020-01-31 23:00:00, 2020-01-31 23:00:00, 2020-01-31 23:…
#> $ ts_time_interval_end            <dttm> 2020-02-29 23:00:00, 2020-02-29 23:00:00, 2020-02-29 23:00:00, 2020-02-29 23:…
#> $ ts_mrid                         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_point_dt_start               <dttm> 2020-01-31 23:00:00, 2020-02-01 00:00:00, 2020-02-01 01:00:00, 2020-02-01 02:…
#> $ ts_point_quantity               <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 7.20, 431.69, 1244.42, 1843.30…
#> $ ts_quantity_measure_unit_name   <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "…
```
