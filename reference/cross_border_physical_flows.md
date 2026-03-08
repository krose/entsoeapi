# Get Cross-Border Physical Flows (12.1.G)

It is the measured real flow of energy between the neighbouring areas on
the cross borders.

## Usage

``` r
cross_border_physical_flows(
  eic_in = NULL,
  eic_out = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic_in:

  Energy Identification Code of in domain

- eic_out:

  Energy Identification Code of out domain

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format Minimum time interval in query
  response is an MTU period, but 1 year range limit applies.

- tidy_output:

  Defaults to TRUE. If TRUE, then flatten nested tables.

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df1 <- entsoeapi::cross_border_physical_flows(
  eic_in = "10Y1001A1001A83F",
  eic_out = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2020-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2020-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A11&in_Domain=10Y1001A1001A83F&out_Domain=10YCZ-CEPS-----N&periodStart=201912312300&periodEnd=202001012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Sun, 08 Mar 2026 23:47:51 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Physical Flows_201912312300-202001012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ No additional definitions added!

dplyr::glimpse(df1)
#> Rows: 24
#> Columns: 17
#> $ ts_in_domain_mrid             <chr> "10Y1001A1001A83F", "10Y1001A1001A83F", "10Y1001A1001A83F", "10Y1001A1001A83F", …
#> $ ts_in_domain_name             <chr> "Germany", "Germany", "Germany", "Germany", "Germany", "Germany", "Germany", "Ge…
#> $ ts_out_domain_mrid            <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", …
#> $ ts_out_domain_name            <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", "Czech R…
#> $ type                          <chr> "A11", "A11", "A11", "A11", "A11", "A11", "A11", "A11", "A11", "A11", "A11", "A1…
#> $ type_def                      <chr> "Aggregated energy data report", "Aggregated energy data report", "Aggregated en…
#> $ ts_business_type              <chr> "A66", "A66", "A66", "A66", "A66", "A66", "A66", "A66", "A66", "A66", "A66", "A6…
#> $ ts_business_type_def          <chr> "Energy flow", "Energy flow", "Energy flow", "Energy flow", "Energy flow", "Ener…
#> $ created_date_time             <dttm> 2026-03-08 23:47:51, 2026-03-08 23:47:51, 2026-03-08 23:47:51, 2026-03-08 23:47…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_resolution                 <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M",…
#> $ ts_time_interval_start        <dttm> 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019-12-31 23:00:…
#> $ ts_time_interval_end          <dttm> 2020-01-01 23:00:00, 2020-01-01 23:00:00, 2020-01-01 23:00:00, 2020-01-01 23:00…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_point_dt_start             <dttm> 2019-12-31 23:00:00, 2020-01-01 00:00:00, 2020-01-01 01:00:00, 2020-01-01 02:00…
#> $ ts_point_quantity             <dbl> 1076.65, 1172.35, 1503.75, 975.48, 714.69, 717.17, 786.54, 841.55, 882.89, 984.9…
#> $ ts_quantity_measure_unit_name <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "M…

df2 <- entsoeapi::cross_border_physical_flows(
  eic_in = "10YCZ-CEPS-----N",
  eic_out = "10Y1001A1001A83F",
  period_start = lubridate::ymd(x = "2020-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2020-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A11&in_Domain=10YCZ-CEPS-----N&out_Domain=10Y1001A1001A83F&periodStart=201912312300&periodEnd=202001012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Sun, 08 Mar 2026 23:47:52 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Physical Flows_201912312300-202001012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ No additional definitions added!

dplyr::glimpse(df2)
#> Rows: 24
#> Columns: 17
#> $ ts_in_domain_mrid             <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", …
#> $ ts_in_domain_name             <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", "Czech R…
#> $ ts_out_domain_mrid            <chr> "10Y1001A1001A83F", "10Y1001A1001A83F", "10Y1001A1001A83F", "10Y1001A1001A83F", …
#> $ ts_out_domain_name            <chr> "Germany", "Germany", "Germany", "Germany", "Germany", "Germany", "Germany", "Ge…
#> $ type                          <chr> "A11", "A11", "A11", "A11", "A11", "A11", "A11", "A11", "A11", "A11", "A11", "A1…
#> $ type_def                      <chr> "Aggregated energy data report", "Aggregated energy data report", "Aggregated en…
#> $ ts_business_type              <chr> "A66", "A66", "A66", "A66", "A66", "A66", "A66", "A66", "A66", "A66", "A66", "A6…
#> $ ts_business_type_def          <chr> "Energy flow", "Energy flow", "Energy flow", "Energy flow", "Energy flow", "Ener…
#> $ created_date_time             <dttm> 2026-03-08 23:47:52, 2026-03-08 23:47:52, 2026-03-08 23:47:52, 2026-03-08 23:47…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_resolution                 <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M",…
#> $ ts_time_interval_start        <dttm> 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019-12-31 23:00:…
#> $ ts_time_interval_end          <dttm> 2020-01-01 23:00:00, 2020-01-01 23:00:00, 2020-01-01 23:00:00, 2020-01-01 23:00…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_point_dt_start             <dttm> 2019-12-31 23:00:00, 2020-01-01 00:00:00, 2020-01-01 01:00:00, 2020-01-01 02:00…
#> $ ts_point_quantity             <dbl> 57.52, 0.32, 0.00, 0.00, 26.50, 16.75, 134.58, 162.03, 174.47, 7.85, 2.52, 7.57,…
#> $ ts_quantity_measure_unit_name <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "M…
```
