# Get Aggregated Balancing Energy Bids (GL EB 12.3.E)

Aggregated balancing energy bids. One year range limit applies.

## Usage

``` r
aggregated_balancing_energy_bids(
  eic = NULL,
  process_type = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the area domain

- process_type:

  type of frequency restoration reserve "A51" aFRR "A46" RR "A47" mFRR
  "A60" mFRR with scheduled activation "A61" mFRR with direct activation

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
df <- entsoeapi::aggregated_balancing_energy_bids(
  eic = "10YCZ-CEPS-----N",
  process_type = "A51",
  period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A24&processType=A51&area_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 10 Mar 2026 17:30:30 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="AGGREGATED_BALANCING_ENERGY_BIDS_R3_202312312300-202401012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived

dplyr::glimpse(df)
#> Rows: 192
#> Columns: 19
#> $ area_domain_mrid              <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", …
#> $ area_domain_name              <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", "Czech R…
#> $ type                          <chr> "A24", "A24", "A24", "A24", "A24", "A24", "A24", "A24", "A24", "A24", "A24", "A2…
#> $ type_def                      <chr> "Bid document", "Bid document", "Bid document", "Bid document", "Bid document", …
#> $ process_type                  <chr> "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A5…
#> $ process_type_def              <chr> "Automatic frequency restoration reserve", "Automatic frequency restoration rese…
#> $ ts_flow_direction             <chr> "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A0…
#> $ ts_flow_direction_def         <chr> "DOWN", "DOWN", "DOWN", "DOWN", "DOWN", "DOWN", "DOWN", "DOWN", "DOWN", "DOWN", …
#> $ ts_business_type              <chr> "A14", "A14", "A14", "A14", "A14", "A14", "A14", "A14", "A14", "A14", "A14", "A1…
#> $ ts_business_type_def          <chr> "Aggregated energy data", "Aggregated energy data", "Aggregated energy data", "A…
#> $ created_date_time             <dttm> 2026-03-10 17:30:30, 2026-03-10 17:30:30, 2026-03-10 17:30:30, 2026-03-10 17:30…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution                 <chr> "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M",…
#> $ ts_time_interval_start        <dttm> 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00…
#> $ ts_time_interval_end          <dttm> 2024-01-01 23:00:00, 2024-01-01 23:00:00, 2024-01-01 23:00:00, 2024-01-01 23:00…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start             <dttm> 2023-12-31 23:00:00, 2023-12-31 23:15:00, 2023-12-31 23:30:00, 2023-12-31 23:45…
#> $ ts_point_quantity             <dbl> 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, …
#> $ ts_quantity_measure_unit_name <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MA…
```
