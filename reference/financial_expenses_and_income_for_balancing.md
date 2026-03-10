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
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A87&controlArea_Domain=10YCZ-CEPS-----N&periodStart=202112312300&periodEnd=202201012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 10 Mar 2026 17:31:45 GMT
#> <- content-type: application/zip
#> <- content-length: 888
#> <- content-disposition: attachment; filename="Financial_expenses_and_income_for_balancing_202112312300-202201012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpLV7XFO/001-FINANCIAL_EXPENSES_AND_INCOME_FOR_BALANCING_R3202112312300-202201312300.xml has been read in

dplyr::glimpse(df)
#> Rows: 2
#> Columns: 18
#> $ control_area_domain_mrid <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N"
#> $ control_area_domain_name <chr> "Czech Republic", "Czech Republic"
#> $ doc_status_value         <chr> "A01", "A01"
#> $ doc_status               <chr> "Balance responsible schedule", "Balance responsible schedule"
#> $ type                     <chr> "A87", "A87"
#> $ type_def                 <chr> "Financial situation", "Financial situation"
#> $ process_type             <chr> "A16", "A16"
#> $ process_type_def         <chr> "Realised", "Realised"
#> $ ts_business_type         <chr> "A99", "A99"
#> $ ts_business_type_def     <chr> "Financial information", "Financial information"
#> $ created_date_time        <dttm> 2026-03-10 17:31:45, 2026-03-10 17:31:45
#> $ revision_number          <dbl> 1, 1
#> $ ts_resolution            <chr> "P1M", "P1M"
#> $ ts_time_interval_start   <dttm> 2021-12-31 23:00:00, 2021-12-31 23:00:00
#> $ ts_time_interval_end     <dttm> 2022-01-31 23:00:00, 2022-01-31 23:00:00
#> $ ts_mrid                  <dbl> 1, 1
#> $ ts_point_dt_start        <dttm> 2021-12-31 23:00:00, 2021-12-31 23:00:00
#> $ ts_currency_unit_name    <chr> "CZK", "CZK"
```
