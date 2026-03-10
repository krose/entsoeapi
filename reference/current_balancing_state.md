# Get Current Balancing State (GL EB 12.3.A)

Current balancing state of the control area. 100 day range limit
applies.

## Usage

``` r
current_balancing_state(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the area domain

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format 100 day range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format 100 day range limit applies

- tidy_output:

  Defaults to TRUE. flatten nested tables

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df <- entsoeapi::current_balancing_state(
  eic = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A86&businessType=B33&Area_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 10 Mar 2026 17:31:07 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Current_balancing_state_202312312300-202401012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived

dplyr::glimpse(df)
#> Rows: 1,440
#> Columns: 19
#> $ area_domain_mrid              <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", …
#> $ area_domain_name              <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", "Czech R…
#> $ type                          <chr> "A86", "A86", "A86", "A86", "A86", "A86", "A86", "A86", "A86", "A86", "A86", "A8…
#> $ type_def                      <chr> "Imbalance volume", "Imbalance volume", "Imbalance volume", "Imbalance volume", …
#> $ process_type                  <chr> "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A1…
#> $ process_type_def              <chr> "Realised", "Realised", "Realised", "Realised", "Realised", "Realised", "Realise…
#> $ ts_flow_direction             <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A0…
#> $ ts_flow_direction_def         <chr> "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "U…
#> $ ts_business_type              <chr> "B33", "B33", "B33", "B33", "B33", "B33", "B33", "B33", "B33", "B33", "B33", "B3…
#> $ ts_business_type_def          <chr> "Area Control error (ACE)", "Area Control error (ACE)", "Area Control error (ACE…
#> $ created_date_time             <dttm> 2026-03-10 17:31:07, 2026-03-10 17:31:07, 2026-03-10 17:31:07, 2026-03-10 17:31…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution                 <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", …
#> $ ts_time_interval_start        <dttm> 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00…
#> $ ts_time_interval_end          <dttm> 2023-12-31 23:16:00, 2023-12-31 23:16:00, 2023-12-31 23:16:00, 2023-12-31 23:16…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 3, 3, 4, 4, 4, 5, 5, 6, 7, 7,…
#> $ ts_point_dt_start             <dttm> 2023-12-31 23:00:00, 2023-12-31 23:01:00, 2023-12-31 23:02:00, 2023-12-31 23:03…
#> $ ts_point_quantity             <dbl> 44.59, 80.00, 76.08, 75.17, 51.51, 85.39, 69.69, 72.60, 84.03, 87.72, 80.89, 93.…
#> $ ts_quantity_measure_unit_name <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MA…
```
