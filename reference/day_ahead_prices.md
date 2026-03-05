# Get Day-Ahead Prices (12.1.D)

Prices in currency/MWh created on spot (Day-Ahead) market. The data is
delivered for each market time unit.

## Usage

``` r
day_ahead_prices(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the related domain

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
df1 <- entsoeapi::day_ahead_prices(
  eic = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2019-12-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A44&in_Domain=10YCZ-CEPS-----N&out_Domain=10YCZ-CEPS-----N&periodStart=201910312300&periodEnd=201911302300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 05 Mar 2026 16:12:02 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Energy_Prices_201910312300-201911302300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ No additional definitions added!

str(df1)
#> tibble [720 × 22] (S3: tbl_df/tbl/data.frame)
#>  $ ts_in_domain_mrid                    : chr [1:720] "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" ...
#>  $ ts_in_domain_name                    : chr [1:720] "Czech Republic" "Czech Republic" "Czech Republic" "Czech Republic" ...
#>  $ ts_out_domain_mrid                   : chr [1:720] "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" ...
#>  $ ts_out_domain_name                   : chr [1:720] "Czech Republic" "Czech Republic" "Czech Republic" "Czech Republic" ...
#>  $ type                                 : chr [1:720] "A44" "A44" "A44" "A44" ...
#>  $ type_def                             : chr [1:720] "Price Document" "Price Document" "Price Document" "Price Document" ...
#>  $ ts_contract_market_agreement_type    : chr [1:720] "A01" "A01" "A01" "A01" ...
#>  $ ts_contract_market_agreement_type_def: chr [1:720] "Daily contract" "Daily contract" "Daily contract" "Daily contract" ...
#>  $ ts_auction_type                      : chr [1:720] "A01" "A01" "A01" "A01" ...
#>  $ ts_auction_type_def                  : chr [1:720] "Implicit" "Implicit" "Implicit" "Implicit" ...
#>  $ ts_business_type                     : chr [1:720] "A62" "A62" "A62" "A62" ...
#>  $ ts_business_type_def                 : chr [1:720] "Spot price" "Spot price" "Spot price" "Spot price" ...
#>  $ created_date_time                    : POSIXct[1:720], format: "2026-03-05 16:12:02" "2026-03-05 16:12:02" "2026-03-05 16:12:02" "2026-03-05 16:12:02" ...
#>  $ revision_number                      : num [1:720] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution                        : chr [1:720] "PT60M" "PT60M" "PT60M" "PT60M" ...
#>  $ ts_time_interval_start               : POSIXct[1:720], format: "2019-10-31 23:00:00" "2019-10-31 23:00:00" "2019-10-31 23:00:00" "2019-10-31 23:00:00" ...
#>  $ ts_time_interval_end                 : POSIXct[1:720], format: "2019-11-01 23:00:00" "2019-11-01 23:00:00" "2019-11-01 23:00:00" "2019-11-01 23:00:00" ...
#>  $ ts_mrid                              : num [1:720] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start                    : POSIXct[1:720], format: "2019-10-31 23:00:00" "2019-11-01 00:00:00" "2019-11-01 01:00:00" "2019-11-01 02:00:00" ...
#>  $ ts_point_price_amount                : num [1:720] 33 31.8 30.5 30.8 30.9 ...
#>  $ ts_currency_unit_name                : chr [1:720] "EUR" "EUR" "EUR" "EUR" ...
#>  $ ts_price_measure_unit_name           : chr [1:720] "MWH" "MWH" "MWH" "MWH" ...

df2 <- entsoeapi::day_ahead_prices(
  eic = "10Y1001A1001A82H",
  period_start = lubridate::ymd(x = "2026-02-13", tz = "CET"),
  period_end = lubridate::ymd(x = "2026-02-13", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A44&in_Domain=10Y1001A1001A82H&out_Domain=10Y1001A1001A82H&periodStart=202602122300&periodEnd=202602122300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 05 Mar 2026 16:12:03 GMT
#> <- content-type: text/xml
#> <- content-length: 963
#> <- content-disposition: inline; filename="acknowledgement.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ No additional type names added!
#> ℹ No additional eic names added!
str(df2)
#> tibble [1 × 3] (S3: tbl_df/tbl/data.frame)
#>  $ created_date_time: POSIXct[1:1], format: "2026-03-05 16:12:03"
#>  $ reason_code      : chr "999"
#>  $ reason_text      : chr "No matching data found for Data item ENERGY_PRICES [12.1.D] (10Y1001A1001A82H, 10Y1001A1001A82H) and interval 2"| __truncated__
```
