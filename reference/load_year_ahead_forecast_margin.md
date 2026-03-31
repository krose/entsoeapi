# Get Year-Ahead Forecast Margin (8.1)

It is defined as a difference between yearly forecast of available
generation capacity and yearly forecast of total load, taking into
account the forecast of total generation capacity forecast of
availability of generation and forecast of reserves contracted for
system services.

## Usage

``` r
load_year_ahead_forecast_margin(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the bidding zone/ country/control area

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

Other load endpoints:
[`load_actual_total()`](https://krose.github.io/entsoeapi/reference/load_actual_total.md),
[`load_day_ahead_total_forecast()`](https://krose.github.io/entsoeapi/reference/load_day_ahead_total_forecast.md),
[`load_month_ahead_total_forecast()`](https://krose.github.io/entsoeapi/reference/load_month_ahead_total_forecast.md),
[`load_week_ahead_total_forecast()`](https://krose.github.io/entsoeapi/reference/load_week_ahead_total_forecast.md),
[`load_year_ahead_total_forecast()`](https://krose.github.io/entsoeapi/reference/load_year_ahead_total_forecast.md)

## Examples

``` r
df <- entsoeapi::load_year_ahead_forecast_margin(
  eic = "10Y1001A1001A82H",
  period_start = lubridate::ymd(x = "2019-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2019-12-31", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A70&processType=A33&outBiddingZone_Domain=10Y1001A1001A82H&periodStart=201812312300&periodEnd=201912302300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 31 Mar 2026 06:58:20 GMT
#> <- content-type: text/xml
#> <- content-length: 1550
#> <- content-disposition: inline; filename="Year Ahead Forecast Margin_201812312300-201912302300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!

dplyr::glimpse(df)
#> Rows: 1
#> Columns: 21
#> $ ts_out_bidding_zone_domain_mrid <chr> "10Y1001A1001A82H"
#> $ ts_out_bidding_zone_domain_name <chr> "Germany_Luxemburg"
#> $ type                            <chr> "A70"
#> $ type_def                        <chr> "Load forecast margin"
#> $ process_type                    <chr> "A33"
#> $ process_type_def                <chr> "Year ahead"
#> $ ts_object_aggregation           <chr> "A01"
#> $ ts_object_aggregation_def       <chr> "Area"
#> $ ts_business_type                <chr> "A91"
#> $ ts_business_type_def            <chr> "positive forecast margin"
#> $ created_date_time               <dttm> 2026-03-31 06:58:20
#> $ revision_number                 <dbl> 1
#> $ time_period_time_interval_start <dttm> 2018-12-31 23:00:00
#> $ time_period_time_interval_end   <dttm> 2019-12-31 23:00:00
#> $ ts_resolution                   <chr> "P1Y"
#> $ ts_time_interval_start          <dttm> 2018-12-31 23:00:00
#> $ ts_time_interval_end            <dttm> 2019-12-31 23:00:00
#> $ ts_mrid                         <dbl> 1
#> $ ts_point_dt_start               <dttm> 2018-12-31 23:00:00
#> $ ts_point_quantity               <dbl> 2964
#> $ ts_quantity_measure_unit_name   <chr> "MAW"
```
