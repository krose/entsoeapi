# Get Redispatching Internal (13.1.A)

Changes in production and load (increase or decrease) to relieve
internal congestion lines that exceeds its capacity. 100 documents limit
applies!!

## Usage

``` r
redispatching_internal(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the control area

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format

- tidy_output:

  Defaults to TRUE. If TRUE, then flatten nested tables.

- security_token:

  Security token for ENTSO-E transparency platform

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with the queried data, or `NULL` if no data is available for the given
parameters.

## Examples

``` r
# Netherlands' internal redispatching.
df <- entsoeapi::redispatching_internal(
  eic = "10YNL----------L",
  period_start = lubridate::ymd(x = "2023-11-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2023-12-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A63&businessType=A85&in_Domain=10YNL----------L&out_Domain=10YNL----------L&periodStart=202310312300&periodEnd=202311302300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 12 Mar 2026 10:23:22 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Redispatching_Internal_202310312300-202311052300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived

dplyr::glimpse(df)
#> Rows: 384
#> Columns: 28
#> $ ts_in_domain_mrid             <chr> "10YNL----------L", "10YNL----------L", "10YNL----------L", "10YNL----------L", …
#> $ ts_in_domain_name             <chr> "Netherlands", "Netherlands", "Netherlands", "Netherlands", "Netherlands", "Neth…
#> $ ts_out_domain_mrid            <chr> "10YNL----------L", "10YNL----------L", "10YNL----------L", "10YNL----------L", …
#> $ ts_out_domain_name            <chr> "Netherlands", "Netherlands", "Netherlands", "Netherlands", "Netherlands", "Neth…
#> $ ts_asset_location_name        <chr> "Hardenberg - Ommen Dante wit 110 kV", "Hardenberg - Ommen Dante wit 110 kV", "H…
#> $ ts_asset_mrid                 <chr> "49T000000000436O", "49T000000000436O", "49T000000000436O", "49T000000000436O", …
#> $ type                          <chr> "A63", "A63", "A63", "A63", "A63", "A63", "A63", "A63", "A63", "A63", "A63", "A6…
#> $ type_def                      <chr> "Redispatch notice", "Redispatch notice", "Redispatch notice", "Redispatch notic…
#> $ process_type                  <chr> "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A1…
#> $ process_type_def              <chr> "Realised", "Realised", "Realised", "Realised", "Realised", "Realised", "Realise…
#> $ ts_flow_direction             <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A0…
#> $ ts_flow_direction_def         <chr> "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "U…
#> $ ts_business_type              <chr> "A85", "A85", "A85", "A85", "A85", "A85", "A85", "A85", "A85", "A85", "A85", "A8…
#> $ ts_business_type_def          <chr> "Internal redispatch", "Internal redispatch", "Internal redispatch", "Internal r…
#> $ ts_mkt_psr_type               <chr> "A05", "A05", "A05", "A05", "A05", "A05", "A05", "A05", "A05", "A05", "A05", "A0…
#> $ ts_mkt_psr_type_def           <chr> "Load", "Load", "Load", "Load", "Load", "Load", "Load", "Load", "Load", "Load", …
#> $ ts_psr_type                   <chr> "B21", "B21", "B21", "B21", "B21", "B21", "B21", "B21", "B21", "B21", "B21", "B2…
#> $ created_date_time             <dttm> 2026-03-12 10:23:22, 2026-03-12 10:23:22, 2026-03-12 10:23:22, 2026-03-12 10:23…
#> $ ts_reason_code                <chr> "B24", "B24", "B24", "B24", "B24", "B24", "B24", "B24", "B24", "B24", "B24", "B2…
#> $ ts_reason_text                <chr> "Load flow overload", "Load flow overload", "Load flow overload", "Load flow ove…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution                 <chr> "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M",…
#> $ ts_time_interval_start        <dttm> 2023-10-31 23:00:00, 2023-10-31 23:00:00, 2023-10-31 23:00:00, 2023-10-31 23:00…
#> $ ts_time_interval_end          <dttm> 2023-11-01 23:00:00, 2023-11-01 23:00:00, 2023-11-01 23:00:00, 2023-11-01 23:00…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start             <dttm> 2023-10-31 23:00:00, 2023-10-31 23:15:00, 2023-10-31 23:30:00, 2023-10-31 23:45…
#> $ ts_point_quantity             <dbl> 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, …
#> $ ts_quantity_measure_unit_name <chr> "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MW…
```
