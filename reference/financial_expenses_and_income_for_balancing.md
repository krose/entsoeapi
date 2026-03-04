# Get Financial Expenses and Income for Balancing (17.1.I)

Financial expenses and income for balancing of the control area. One
year range limit applies.

## Usage

``` r
financial_expenses_and_income_for_balancing(
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
df <- entsoeapi::financial_expenses_and_income_for_balancing(
  eic = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A87&controlArea_Domain=10YCZ-CEPS-----N&periodStart=202112312300&periodEnd=202201012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:08:38 GMT
#> <- content-type: application/zip
#> <- content-length: 887
#> <- content-disposition: attachment; filename="Financial_expenses_and_income_for_balancing_202112312300-202201012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/Rtmpb5sMb4/001-FINANCIAL_EXPENSES_AND_INCOME_FOR_BALANCING_R3202112312300-202201312300.xml has been read in

str(df)
#> tibble [2 × 18] (S3: tbl_df/tbl/data.frame)
#>  $ control_area_domain_mrid: chr [1:2] "10YCZ-CEPS-----N" "10YCZ-CEPS-----N"
#>  $ control_area_domain_name: chr [1:2] "Czech Republic" "Czech Republic"
#>  $ doc_status_value        : chr [1:2] "A01" "A01"
#>  $ doc_status              : chr [1:2] "Balance responsible schedule" "Balance responsible schedule"
#>  $ type                    : chr [1:2] "A87" "A87"
#>  $ type_def                : chr [1:2] "Financial situation" "Financial situation"
#>  $ process_type            : chr [1:2] "A16" "A16"
#>  $ process_type_def        : chr [1:2] "Realised" "Realised"
#>  $ ts_business_type        : chr [1:2] "A99" "A99"
#>  $ ts_business_type_def    : chr [1:2] "Financial information" "Financial information"
#>  $ created_date_time       : POSIXct[1:2], format: "2026-03-04 22:08:38" "2026-03-04 22:08:38"
#>  $ revision_number         : num [1:2] 1 1
#>  $ ts_resolution           : chr [1:2] "P1M" "P1M"
#>  $ ts_time_interval_start  : POSIXct[1:2], format: "2021-12-31 23:00:00" "2021-12-31 23:00:00"
#>  $ ts_time_interval_end    : POSIXct[1:2], format: "2022-01-31 23:00:00" "2022-01-31 23:00:00"
#>  $ ts_mrid                 : num [1:2] 1 1
#>  $ ts_point_dt_start       : POSIXct[1:2], format: "2021-12-31 23:00:00" "2021-12-31 23:00:00"
#>  $ ts_currency_unit_name   : chr [1:2] "CZK" "CZK"
```
