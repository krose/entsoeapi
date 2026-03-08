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
#> <- date: Sun, 08 Mar 2026 23:47:56 GMT
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

dplyr::glimpse(df1)
#> Rows: 720
#> Columns: 22
#> $ ts_in_domain_mrid                     <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-…
#> $ ts_in_domain_name                     <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", …
#> $ ts_out_domain_mrid                    <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-…
#> $ ts_out_domain_name                    <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", …
#> $ type                                  <chr> "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A…
#> $ type_def                              <chr> "Price Document", "Price Document", "Price Document", "Price Document", …
#> $ ts_contract_market_agreement_type     <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A…
#> $ ts_contract_market_agreement_type_def <chr> "Daily contract", "Daily contract", "Daily contract", "Daily contract", …
#> $ ts_auction_type                       <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A…
#> $ ts_auction_type_def                   <chr> "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", …
#> $ ts_business_type                      <chr> "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A…
#> $ ts_business_type_def                  <chr> "Spot price", "Spot price", "Spot price", "Spot price", "Spot price", "S…
#> $ created_date_time                     <dttm> 2026-03-08 23:47:56, 2026-03-08 23:47:56, 2026-03-08 23:47:56, 2026-03-…
#> $ revision_number                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_resolution                         <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", …
#> $ ts_time_interval_start                <dttm> 2019-10-31 23:00:00, 2019-10-31 23:00:00, 2019-10-31 23:00:00, 2019-10-…
#> $ ts_time_interval_end                  <dttm> 2019-11-01 23:00:00, 2019-11-01 23:00:00, 2019-11-01 23:00:00, 2019-11-…
#> $ ts_mrid                               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_point_dt_start                     <dttm> 2019-10-31 23:00:00, 2019-11-01 00:00:00, 2019-11-01 01:00:00, 2019-11-…
#> $ ts_point_price_amount                 <dbl> 33.03, 31.81, 30.53, 30.81, 30.95, 31.18, 32.19, 34.00, 33.22, 33.30, 32…
#> $ ts_currency_unit_name                 <chr> "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "E…
#> $ ts_price_measure_unit_name            <chr> "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "M…

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
#> <- date: Sun, 08 Mar 2026 23:47:57 GMT
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
dplyr::glimpse(df2)
#> Rows: 1
#> Columns: 3
#> $ created_date_time <dttm> 2026-03-08 23:47:57
#> $ reason_code       <chr> "999"
#> $ reason_text       <chr> "No matching data found for Data item ENERGY_PRICES [12.1.D] (10Y1001A1001A82H, 10Y1001A100…
```
