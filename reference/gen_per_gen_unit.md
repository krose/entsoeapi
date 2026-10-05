# Get Aggregated Generation per Generation Unit (16.1.A)

Actual net generation output (MW) and optionally consumption data from
all generation units. Data are aggregated as an average of generation
outputs or consumption.

## Usage

``` r
gen_per_gen_unit(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  gen_type = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the control area or bidding zone

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format

- gen_type:

  Defaults to NULL, otherwise list of generation type codes from
  asset_types table

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
[`gen_per_prod_type()`](https://krose.github.io/entsoeapi/reference/gen_per_prod_type.md),
[`gen_storage_mean_filling_rate()`](https://krose.github.io/entsoeapi/reference/gen_storage_mean_filling_rate.md),
[`gen_wind_solar_forecasts()`](https://krose.github.io/entsoeapi/reference/gen_wind_solar_forecasts.md)

## Examples

``` r
if (FALSE) { # there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
df <- entsoeapi::gen_per_gen_unit(
  eic          = "10YDE-VE-------2",
  period_start = lubridate::ymd(x = "2020-01-31", tz = "CET"),
  period_end   = lubridate::ymd(x = "2020-02-06", tz = "CET"),
  gen_type     = c("B04", "B05"),
  tidy_output  = TRUE
)

dplyr::glimpse(df)
}
```
