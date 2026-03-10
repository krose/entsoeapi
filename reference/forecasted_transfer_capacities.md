# Get Forecasted Transfer Capacities (11.1.A)

forecasted transfer capacities (MW) per direction between areas

## Usage

``` r
forecasted_transfer_capacities(
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

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- tidy_output:

  Defaults to TRUE. If TRUE, then flatten nested tables.

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df1 <- entsoeapi::forecasted_transfer_capacities(
  eic_in = "10YCZ-CEPS-----N",
  eic_out = "10YSK-SEPS-----K",
  period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2019-12-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A61&contract_MarketAgreement.Type=A01&in_Domain=10YCZ-CEPS-----N&out_Domain=10YSK-SEPS-----K&periodStart=201910312300&periodEnd=201911302300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 10 Mar 2026 19:48:33 GMT
#> <- content-type: text/xml
#> <- content-length: 1580
#> <- content-disposition: inline; filename="Forecasted Transfer Capacities_201910312300-201911302300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ No additional definitions added!

dplyr::glimpse(df1)
#> Rows: 720
#> Columns: 17
#> $ ts_in_domain_mrid             <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", …
#> $ ts_in_domain_name             <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", "Czech R…
#> $ ts_out_domain_mrid            <chr> "10YSK-SEPS-----K", "10YSK-SEPS-----K", "10YSK-SEPS-----K", "10YSK-SEPS-----K", …
#> $ ts_out_domain_name            <chr> "Slovak Republic", "Slovak Republic", "Slovak Republic", "Slovak Republic", "Slo…
#> $ type                          <chr> "A61", "A61", "A61", "A61", "A61", "A61", "A61", "A61", "A61", "A61", "A61", "A6…
#> $ type_def                      <chr> "Estimated Net Transfer Capacity", "Estimated Net Transfer Capacity", "Estimated…
#> $ ts_business_type              <chr> "A27", "A27", "A27", "A27", "A27", "A27", "A27", "A27", "A27", "A27", "A27", "A2…
#> $ ts_business_type_def          <chr> "Net transfer capacity (NTC)", "Net transfer capacity (NTC)", "Net transfer capa…
#> $ created_date_time             <dttm> 2026-03-10 19:48:33, 2026-03-10 19:48:33, 2026-03-10 19:48:33, 2026-03-10 19:48…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution                 <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M",…
#> $ ts_time_interval_start        <dttm> 2019-10-31 23:00:00, 2019-10-31 23:00:00, 2019-10-31 23:00:00, 2019-10-31 23:00…
#> $ ts_time_interval_end          <dttm> 2019-11-30 23:00:00, 2019-11-30 23:00:00, 2019-11-30 23:00:00, 2019-11-30 23:00…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start             <dttm> 2019-10-31 23:00:00, 2019-11-01 00:00:00, 2019-11-01 01:00:00, 2019-11-01 02:00…
#> $ ts_point_quantity             <dbl> 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 12…
#> $ ts_quantity_measure_unit_name <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MA…

df2 <- entsoeapi::forecasted_transfer_capacities(
  eic_in = "10YDK-1--------W",
  eic_out = "10Y1001A1001A82H",
  period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2019-12-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A61&contract_MarketAgreement.Type=A01&in_Domain=10YDK-1--------W&out_Domain=10Y1001A1001A82H&periodStart=201910312300&periodEnd=201911302300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 10 Mar 2026 19:48:33 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Forecasted Transfer Capacities_201910312300-201911302300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ No additional definitions added!

dplyr::glimpse(df2)
#> Rows: 720
#> Columns: 17
#> $ ts_in_domain_mrid             <chr> "10YDK-1--------W", "10YDK-1--------W", "10YDK-1--------W", "10YDK-1--------W", …
#> $ ts_in_domain_name             <chr> "Denmark DK1", "Denmark DK1", "Denmark DK1", "Denmark DK1", "Denmark DK1", "Denm…
#> $ ts_out_domain_mrid            <chr> "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A82H", …
#> $ ts_out_domain_name            <chr> "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Luxembur…
#> $ type                          <chr> "A61", "A61", "A61", "A61", "A61", "A61", "A61", "A61", "A61", "A61", "A61", "A6…
#> $ type_def                      <chr> "Estimated Net Transfer Capacity", "Estimated Net Transfer Capacity", "Estimated…
#> $ ts_business_type              <chr> "A27", "A27", "A27", "A27", "A27", "A27", "A27", "A27", "A27", "A27", "A27", "A2…
#> $ ts_business_type_def          <chr> "Net transfer capacity (NTC)", "Net transfer capacity (NTC)", "Net transfer capa…
#> $ created_date_time             <dttm> 2026-03-10 19:48:33, 2026-03-10 19:48:33, 2026-03-10 19:48:33, 2026-03-10 19:48…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution                 <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M",…
#> $ ts_time_interval_start        <dttm> 2019-10-31 23:00:00, 2019-10-31 23:00:00, 2019-10-31 23:00:00, 2019-10-31 23:00…
#> $ ts_time_interval_end          <dttm> 2019-11-30 23:00:00, 2019-11-30 23:00:00, 2019-11-30 23:00:00, 2019-11-30 23:00…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start             <dttm> 2019-10-31 23:00:00, 2019-11-01 00:00:00, 2019-11-01 01:00:00, 2019-11-01 02:00…
#> $ ts_point_quantity             <dbl> 1300, 1300, 1300, 1300, 1300, 1300, 1300, 1300, 1300, 1300, 1300, 1300, 1300, 13…
#> $ ts_quantity_measure_unit_name <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MA…
```
