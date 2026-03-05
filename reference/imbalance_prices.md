# Get Imbalance Prices (17.1.G)

Imbalance prices of the control area. One year range limit applies.

## Usage

``` r
imbalance_prices(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the control area domain

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- tidy_output:

  Defaults to TRUE. flatten nested tables

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df <- entsoeapi::imbalance_prices(
  eic = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A85&controlArea_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 05 Mar 2026 16:12:52 GMT
#> <- content-type: application/zip
#> <- content-length: 1563
#> <- content-disposition: attachment; filename="Imbalance Prices_202312312300-202401012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpksiKAO/001-IMBALANCE_PRICES_R3_202312312300-202401012300.xml has been read in

str(df)
#> tibble [72 × 19] (S3: tbl_df/tbl/data.frame)
#>  $ area_domain_mrid          : chr [1:72] "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" ...
#>  $ area_domain_name          : chr [1:72] "Czech Republic" "Czech Republic" "Czech Republic" "Czech Republic" ...
#>  $ doc_status_value          : chr [1:72] "A02" "A02" "A02" "A02" ...
#>  $ doc_status                : chr [1:72] "Allocated capacity schedule" "Allocated capacity schedule" "Allocated capacity schedule" "Allocated capacity schedule" ...
#>  $ type                      : chr [1:72] "A85" "A85" "A85" "A85" ...
#>  $ type_def                  : chr [1:72] "Imbalance prices" "Imbalance prices" "Imbalance prices" "Imbalance prices" ...
#>  $ process_type              : chr [1:72] "A16" "A16" "A16" "A16" ...
#>  $ process_type_def          : chr [1:72] "Realised" "Realised" "Realised" "Realised" ...
#>  $ ts_business_type          : chr [1:72] "A19" "A19" "A19" "A19" ...
#>  $ ts_business_type_def      : chr [1:72] "Balance energy deviation" "Balance energy deviation" "Balance energy deviation" "Balance energy deviation" ...
#>  $ created_date_time         : POSIXct[1:72], format: "2026-03-05 16:12:52" "2026-03-05 16:12:52" "2026-03-05 16:12:52" "2026-03-05 16:12:52" ...
#>  $ revision_number           : num [1:72] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution             : chr [1:72] "PT60M" "PT60M" "PT60M" "PT60M" ...
#>  $ ts_time_interval_start    : POSIXct[1:72], format: "2023-12-31 23:00:00" "2023-12-31 23:00:00" "2023-12-31 23:00:00" "2023-12-31 23:00:00" ...
#>  $ ts_time_interval_end      : POSIXct[1:72], format: "2024-01-01 23:00:00" "2024-01-01 23:00:00" "2024-01-01 23:00:00" "2024-01-01 23:00:00" ...
#>  $ ts_mrid                   : num [1:72] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start         : POSIXct[1:72], format: "2023-12-31 23:00:00" "2023-12-31 23:00:00" "2024-01-01 00:00:00" "2024-01-01 00:00:00" ...
#>  $ ts_currency_unit_name     : chr [1:72] "CZK" "CZK" "CZK" "CZK" ...
#>  $ ts_price_measure_unit_name: chr [1:72] "MWH" "MWH" "MWH" "MWH" ...
```
