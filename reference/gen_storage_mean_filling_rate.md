# Get Weekly Average Filling Rate of Water Reservoirs and Hydro Storage Plants (16.1.D)

Aggregated weekly average filling rate of all water reservoir and hydro
storage plants (MWh) per area, including the same week value of the
previous year.

## Usage

``` r
gen_storage_mean_filling_rate(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
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

  POSIXct or YYYY-MM-DD HH:MM:SS format Maximum 380 days range limit
  applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format Maximum 380 days range limit
  applies

- tidy_output:

  Defaults to TRUE. If TRUE, then flatten nested tables.

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df <- entsoeapi::gen_storage_mean_filling_rate(
  eic          = "10YFR-RTE------C",
  period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
  period_end   = lubridate::ymd(x = "2021-02-15", tz = "CET"),
  tidy_output  = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A72&processType=A16&in_Domain=10YFR-RTE------C&periodStart=202001312300&periodEnd=202102142300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:08:59 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Aggregate Filling Rate of Water Reservoirs_202001312300-202102142300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
str(df)
#> tibble [55 × 21] (S3: tbl_df/tbl/data.frame)
#>  $ ts_in_bidding_zone_domain_mrid : chr [1:55] "10YFR-RTE------C" "10YFR-RTE------C" "10YFR-RTE------C" "10YFR-RTE------C" ...
#>  $ ts_in_bidding_zone_domain_name : chr [1:55] "France" "France" "France" "France" ...
#>  $ type                           : chr [1:55] "A72" "A72" "A72" "A72" ...
#>  $ type_def                       : chr [1:55] "Reservoir filling information" "Reservoir filling information" "Reservoir filling information" "Reservoir filling information" ...
#>  $ process_type                   : chr [1:55] "A16" "A16" "A16" "A16" ...
#>  $ process_type_def               : chr [1:55] "Realised" "Realised" "Realised" "Realised" ...
#>  $ ts_object_aggregation          : chr [1:55] "A01" "A01" "A01" "A01" ...
#>  $ ts_object_aggregation_def      : chr [1:55] "Area" "Area" "Area" "Area" ...
#>  $ ts_business_type               : chr [1:55] "A01" "A01" "A01" "A01" ...
#>  $ ts_business_type_def           : chr [1:55] "Production" "Production" "Production" "Production" ...
#>  $ created_date_time              : POSIXct[1:55], format: "2026-03-04 22:08:59" "2026-03-04 22:08:59" ...
#>  $ revision_number                : num [1:55] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ time_period_time_interval_start: POSIXct[1:55], format: "2020-01-26 23:00:00" "2020-01-26 23:00:00" ...
#>  $ time_period_time_interval_end  : POSIXct[1:55], format: "2021-02-14 23:00:00" "2021-02-14 23:00:00" ...
#>  $ ts_resolution                  : chr [1:55] "P7D" "P7D" "P7D" "P7D" ...
#>  $ ts_time_interval_start         : POSIXct[1:55], format: "2020-01-26 23:00:00" "2020-01-26 23:00:00" ...
#>  $ ts_time_interval_end           : POSIXct[1:55], format: "2021-02-14 23:00:00" "2021-02-14 23:00:00" ...
#>  $ ts_mrid                        : num [1:55] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start              : POSIXct[1:55], format: "2020-01-26 23:00:00" "2020-02-02 23:00:00" ...
#>  $ ts_point_quantity              : num [1:55] 2194236 2214084 2174132 2116585 2008603 ...
#>  $ ts_quantity_measure_unit_name  : chr [1:55] "MWH" "MWH" "MWH" "MWH" ...
```
