# Get Day-Ahead Generation Forecast. (14.1.C)

It is an estimate of the total scheduled net generation (MW) per area
and market time unit of the following day.

## Usage

``` r
gen_day_ahead_forecast(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
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

## Examples

``` r
df <- entsoeapi::gen_day_ahead_forecast(
  eic          = "10YFR-RTE------C",
  period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
  period_end   = lubridate::ymd(x = "2020-03-01", tz = "CET"),
  tidy_output  = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A71&processType=A01&in_Domain=10YFR-RTE------C&periodStart=202001312300&periodEnd=202002292300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:08:46 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Day Ahead Aggregated Generation_202001312300-202002292300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
str(df)
#> tibble [696 × 21] (S3: tbl_df/tbl/data.frame)
#>  $ ts_in_bidding_zone_domain_mrid : chr [1:696] "10YFR-RTE------C" "10YFR-RTE------C" "10YFR-RTE------C" "10YFR-RTE------C" ...
#>  $ ts_in_bidding_zone_domain_name : chr [1:696] "France" "France" "France" "France" ...
#>  $ type                           : chr [1:696] "A71" "A71" "A71" "A71" ...
#>  $ type_def                       : chr [1:696] "Generation forecast" "Generation forecast" "Generation forecast" "Generation forecast" ...
#>  $ process_type                   : chr [1:696] "A01" "A01" "A01" "A01" ...
#>  $ process_type_def               : chr [1:696] "Day ahead" "Day ahead" "Day ahead" "Day ahead" ...
#>  $ ts_object_aggregation          : chr [1:696] "A01" "A01" "A01" "A01" ...
#>  $ ts_object_aggregation_def      : chr [1:696] "Area" "Area" "Area" "Area" ...
#>  $ ts_business_type               : chr [1:696] "A01" "A01" "A01" "A01" ...
#>  $ ts_business_type_def           : chr [1:696] "Production" "Production" "Production" "Production" ...
#>  $ created_date_time              : POSIXct[1:696], format: "2026-03-04 22:08:46" "2026-03-04 22:08:46" ...
#>  $ revision_number                : num [1:696] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ time_period_time_interval_start: POSIXct[1:696], format: "2020-01-31 23:00:00" "2020-01-31 23:00:00" ...
#>  $ time_period_time_interval_end  : POSIXct[1:696], format: "2020-02-29 23:00:00" "2020-02-29 23:00:00" ...
#>  $ ts_resolution                  : chr [1:696] "PT60M" "PT60M" "PT60M" "PT60M" ...
#>  $ ts_time_interval_start         : POSIXct[1:696], format: "2020-01-31 23:00:00" "2020-01-31 23:00:00" ...
#>  $ ts_time_interval_end           : POSIXct[1:696], format: "2020-02-29 23:00:00" "2020-02-29 23:00:00" ...
#>  $ ts_mrid                        : num [1:696] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start              : POSIXct[1:696], format: "2020-01-31 23:00:00" "2020-02-01 00:00:00" ...
#>  $ ts_point_quantity              : num [1:696] 64774 61374 58352 56234 55570 ...
#>  $ ts_quantity_measure_unit_name  : chr [1:696] "MAW" "MAW" "MAW" "MAW" ...
```
