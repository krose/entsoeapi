# Get Activated Balancing Prices (TR 17.1.F, IF aFRR 3.16)

Prices of activated balancing energy and aFRR cross-border marginal
prices. One year range limit applies.

## Usage

``` r
activated_balancing_prices(
  eic = NULL,
  process_type = "A16",
  business_type = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the control area domain

- process_type:

  Process type code, defaults to "A16"

- business_type:

  Optional business type code

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
df <- entsoeapi::activated_balancing_prices(
  eic = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A84&processType=A16&controlArea_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:12:23 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="PRICES_OF_ACTIVATED_BALANCING_ENERGY_R3_202312312300-202401012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ downloading Y_eicCodes.csv file ...

str(df)
#> tibble [480 × 21] (S3: tbl_df/tbl/data.frame)
#>  $ area_domain_mrid          : chr [1:480] "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" ...
#>  $ area_domain_name          : chr [1:480] "Czech Republic" "Czech Republic" "Czech Republic" "Czech Republic" ...
#>  $ type                      : chr [1:480] "A84" "A84" "A84" "A84" ...
#>  $ type_def                  : chr [1:480] "Activated balancing prices" "Activated balancing prices" "Activated balancing prices" "Activated balancing prices" ...
#>  $ process_type              : chr [1:480] "A16" "A16" "A16" "A16" ...
#>  $ process_type_def          : chr [1:480] "Realised" "Realised" "Realised" "Realised" ...
#>  $ ts_flow_direction         : chr [1:480] "A01" "A01" "A01" "A01" ...
#>  $ ts_flow_direction_def     : chr [1:480] "UP" "UP" "UP" "UP" ...
#>  $ ts_business_type          : chr [1:480] "A96" "A96" "A96" "A96" ...
#>  $ ts_business_type_def      : chr [1:480] "Automatic frequency restoration reserve" "Automatic frequency restoration reserve" "Automatic frequency restoration reserve" "Automatic frequency restoration reserve" ...
#>  $ ts_mkt_psr_type           : chr [1:480] "A03" "A03" "A03" "A03" ...
#>  $ ts_mkt_psr_type_def       : chr [1:480] "Resource Object" "Resource Object" "Resource Object" "Resource Object" ...
#>  $ created_date_time         : POSIXct[1:480], format: "2026-03-04 22:12:23" "2026-03-04 22:12:23" ...
#>  $ revision_number           : num [1:480] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution             : chr [1:480] "PT15M" "PT15M" "PT15M" "PT15M" ...
#>  $ ts_time_interval_start    : POSIXct[1:480], format: "2023-12-31 23:00:00" "2023-12-31 23:00:00" ...
#>  $ ts_time_interval_end      : POSIXct[1:480], format: "2024-01-01 23:00:00" "2024-01-01 23:00:00" ...
#>  $ ts_mrid                   : num [1:480] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start         : POSIXct[1:480], format: "2023-12-31 23:00:00" "2023-12-31 23:15:00" ...
#>  $ ts_currency_unit_name     : chr [1:480] "EUR" "EUR" "EUR" "EUR" ...
#>  $ ts_price_measure_unit_name: chr [1:480] "MWH" "MWH" "MWH" "MWH" ...
```
