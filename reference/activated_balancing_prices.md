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
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A84&processType=A16&controlArea_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 10 Mar 2026 17:30:28 GMT
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

dplyr::glimpse(df)
#> Rows: 480
#> Columns: 21
#> $ area_domain_mrid           <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10…
#> $ area_domain_name           <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", "Czech Repu…
#> $ type                       <chr> "A84", "A84", "A84", "A84", "A84", "A84", "A84", "A84", "A84", "A84", "A84", "A84",…
#> $ type_def                   <chr> "Activated balancing prices", "Activated balancing prices", "Activated balancing pr…
#> $ process_type               <chr> "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16",…
#> $ process_type_def           <chr> "Realised", "Realised", "Realised", "Realised", "Realised", "Realised", "Realised",…
#> $ ts_flow_direction          <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01",…
#> $ ts_flow_direction_def      <chr> "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP",…
#> $ ts_business_type           <chr> "A96", "A96", "A96", "A96", "A96", "A96", "A96", "A96", "A96", "A96", "A96", "A96",…
#> $ ts_business_type_def       <chr> "Automatic frequency restoration reserve", "Automatic frequency restoration reserve…
#> $ ts_mkt_psr_type            <chr> "A03", "A03", "A03", "A03", "A03", "A03", "A03", "A03", "A03", "A03", "A03", "A03",…
#> $ ts_mkt_psr_type_def        <chr> "Resource Object", "Resource Object", "Resource Object", "Resource Object", "Resour…
#> $ created_date_time          <dttm> 2026-03-10 17:30:28, 2026-03-10 17:30:28, 2026-03-10 17:30:28, 2026-03-10 17:30:28…
#> $ revision_number            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution              <chr> "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "P…
#> $ ts_time_interval_start     <dttm> 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00:00…
#> $ ts_time_interval_end       <dttm> 2024-01-01 23:00:00, 2024-01-01 23:00:00, 2024-01-01 23:00:00, 2024-01-01 23:00:00…
#> $ ts_mrid                    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start          <dttm> 2023-12-31 23:00:00, 2023-12-31 23:15:00, 2023-12-31 23:30:00, 2023-12-31 23:45:00…
#> $ ts_currency_unit_name      <chr> "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR",…
#> $ ts_price_measure_unit_name <chr> "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH",…
```
