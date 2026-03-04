# Get Aggregated Generation per Production Type (16.1.B&C)

Actual aggregated net generation output (MW) or consumption per market
time unit and per production type.

## Usage

``` r
gen_per_prod_type(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  gen_type = NULL,
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

- gen_type:

  Defaults to NULL, otherwise list of generation type codes from
  asset_types table

- tidy_output:

  Defaults to TRUE. If TRUE, then flatten nested tables.

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df <- entsoeapi::gen_per_prod_type(
  eic          = "10YFR-RTE------C",
  period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
  period_end   = lubridate::ymd(x = "2020-03-01", tz = "CET"),
  gen_type     = NULL,
  tidy_output  = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A75&processType=A16&in_Domain=10YFR-RTE------C&periodStart=202001312300&periodEnd=202002292300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:13:33 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Aggregated Generation per Type_202001312300-202002292300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
str(df)
#> tibble [7,656 × 25] (S3: tbl_df/tbl/data.frame)
#>  $ ts_in_bidding_zone_domain_mrid : chr [1:7656] NA NA NA NA ...
#>  $ ts_in_bidding_zone_domain_name : chr [1:7656] NA NA NA NA ...
#>  $ ts_out_bidding_zone_domain_mrid: chr [1:7656] "10YFR-RTE------C" "10YFR-RTE------C" "10YFR-RTE------C" "10YFR-RTE------C" ...
#>  $ ts_out_bidding_zone_domain_name: chr [1:7656] "France" "France" "France" "France" ...
#>  $ type                           : chr [1:7656] "A75" "A75" "A75" "A75" ...
#>  $ type_def                       : chr [1:7656] "Actual generation per type" "Actual generation per type" "Actual generation per type" "Actual generation per type" ...
#>  $ process_type                   : chr [1:7656] "A16" "A16" "A16" "A16" ...
#>  $ process_type_def               : chr [1:7656] "Realised" "Realised" "Realised" "Realised" ...
#>  $ ts_object_aggregation          : chr [1:7656] "A08" "A08" "A08" "A08" ...
#>  $ ts_object_aggregation_def      : chr [1:7656] "Resource type" "Resource type" "Resource type" "Resource type" ...
#>  $ ts_business_type               : chr [1:7656] "A01" "A01" "A01" "A01" ...
#>  $ ts_business_type_def           : chr [1:7656] "Production" "Production" "Production" "Production" ...
#>  $ ts_mkt_psr_type                : chr [1:7656] "B05" "B05" "B05" "B05" ...
#>  $ ts_mkt_psr_type_def            : chr [1:7656] "Fossil Hard coal" "Fossil Hard coal" "Fossil Hard coal" "Fossil Hard coal" ...
#>  $ created_date_time              : POSIXct[1:7656], format: "2026-03-04 22:13:33" "2026-03-04 22:13:33" ...
#>  $ revision_number                : num [1:7656] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ time_period_time_interval_start: POSIXct[1:7656], format: "2020-01-31 23:00:00" "2020-01-31 23:00:00" ...
#>  $ time_period_time_interval_end  : POSIXct[1:7656], format: "2020-02-29 23:00:00" "2020-02-29 23:00:00" ...
#>  $ ts_resolution                  : chr [1:7656] "PT60M" "PT60M" "PT60M" "PT60M" ...
#>  $ ts_time_interval_start         : POSIXct[1:7656], format: "2020-02-04 03:00:00" "2020-02-04 03:00:00" ...
#>  $ ts_time_interval_end           : POSIXct[1:7656], format: "2020-02-05 17:00:00" "2020-02-05 17:00:00" ...
#>  $ ts_mrid                        : num [1:7656] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start              : POSIXct[1:7656], format: "2020-02-04 03:00:00" "2020-02-04 04:00:00" ...
#>  $ ts_point_quantity              : num [1:7656] 7 7 7 7 8 8 8 8 8 8 ...
#>  $ ts_quantity_measure_unit_name  : chr [1:7656] "MAW" "MAW" "MAW" "MAW" ...
```
