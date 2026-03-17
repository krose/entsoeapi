# Get Total Commercial Schedules (12.1.F)

Aggregated capacity nominated for all time horizons (including
Intra-Day) corresponding to implicit and explicit allocations after each
nomination process.

## Usage

``` r
total_commercial_sched(
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

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with the queried data, or `NULL` if no data is available for the given
parameters.

## Examples

``` r
df1 <- entsoeapi::total_commercial_sched(
  eic_in = "10YCZ-CEPS-----N",
  eic_out = "10YSK-SEPS-----K",
  period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2019-12-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A09&contract_MarketAgreement.Type=A05&in_Domain=10YCZ-CEPS-----N&out_Domain=10YSK-SEPS-----K&periodStart=201910312300&periodEnd=201911302300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 17 Mar 2026 22:12:24 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Commercial schedules_201910312300-201911302300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ pulling area_eic_name table from cache
#> ℹ No additional definitions added!

dplyr::glimpse(df1)
#> Rows: 720
#> Columns: 19
#> $ ts_in_domain_mrid                     <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-…
#> $ ts_in_domain_name                     <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", …
#> $ ts_out_domain_mrid                    <chr> "10YSK-SEPS-----K", "10YSK-SEPS-----K", "10YSK-SEPS-----K", "10YSK-SEPS-…
#> $ ts_out_domain_name                    <chr> "Slovak Republic", "Slovak Republic", "Slovak Republic", "Slovak Republi…
#> $ type                                  <chr> "A09", "A09", "A09", "A09", "A09", "A09", "A09", "A09", "A09", "A09", "A…
#> $ type_def                              <chr> "Finalised schedule", "Finalised schedule", "Finalised schedule", "Final…
#> $ ts_contract_market_agreement_type     <chr> "A05", "A05", "A05", "A05", "A05", "A05", "A05", "A05", "A05", "A05", "A…
#> $ ts_contract_market_agreement_type_def <chr> "Total contract", "Total contract", "Total contract", "Total contract", …
#> $ ts_business_type                      <chr> "A06", "A06", "A06", "A06", "A06", "A06", "A06", "A06", "A06", "A06", "A…
#> $ ts_business_type_def                  <chr> "External trade without explicit capacity", "External trade without expl…
#> $ created_date_time                     <dttm> 2026-03-17 22:12:24, 2026-03-17 22:12:24, 2026-03-17 22:12:24, 2026-03-…
#> $ revision_number                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_resolution                         <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", …
#> $ ts_time_interval_start                <dttm> 2019-10-31 23:00:00, 2019-10-31 23:00:00, 2019-10-31 23:00:00, 2019-10-…
#> $ ts_time_interval_end                  <dttm> 2019-11-30 23:00:00, 2019-11-30 23:00:00, 2019-11-30 23:00:00, 2019-11-…
#> $ ts_mrid                               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_point_dt_start                     <dttm> 2019-10-31 23:00:00, 2019-11-01 00:00:00, 2019-11-01 01:00:00, 2019-11-…
#> $ ts_point_quantity                     <dbl> 21.0, 16.0, 638.0, 713.0, 35.0, 33.0, 34.0, 22.0, 35.0, 55.0, 77.0, 111.…
#> $ ts_quantity_measure_unit_name         <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "M…

df2 <- entsoeapi::total_commercial_sched(
  eic_in = "10YDK-1--------W",
  eic_out = "10Y1001A1001A82H",
  period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2019-12-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A09&contract_MarketAgreement.Type=A05&in_Domain=10YDK-1--------W&out_Domain=10Y1001A1001A82H&periodStart=201910312300&periodEnd=201911302300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 17 Mar 2026 22:12:25 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Commercial schedules_201910312300-201911302300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ pulling area_eic_name table from cache
#> ℹ No additional definitions added!

dplyr::glimpse(df2)
#> Rows: 720
#> Columns: 19
#> $ ts_in_domain_mrid                     <chr> "10YDK-1--------W", "10YDK-1--------W", "10YDK-1--------W", "10YDK-1----…
#> $ ts_in_domain_name                     <chr> "Denmark DK1", "Denmark DK1", "Denmark DK1", "Denmark DK1", "Denmark DK1…
#> $ ts_out_domain_mrid                    <chr> "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A100…
#> $ ts_out_domain_name                    <chr> "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Luxemburg", "Germany_…
#> $ type                                  <chr> "A09", "A09", "A09", "A09", "A09", "A09", "A09", "A09", "A09", "A09", "A…
#> $ type_def                              <chr> "Finalised schedule", "Finalised schedule", "Finalised schedule", "Final…
#> $ ts_contract_market_agreement_type     <chr> "A05", "A05", "A05", "A05", "A05", "A05", "A05", "A05", "A05", "A05", "A…
#> $ ts_contract_market_agreement_type_def <chr> "Total contract", "Total contract", "Total contract", "Total contract", …
#> $ ts_business_type                      <chr> "A06", "A06", "A06", "A06", "A06", "A06", "A06", "A06", "A06", "A06", "A…
#> $ ts_business_type_def                  <chr> "External trade without explicit capacity", "External trade without expl…
#> $ created_date_time                     <dttm> 2026-03-17 22:12:25, 2026-03-17 22:12:25, 2026-03-17 22:12:25, 2026-03-…
#> $ revision_number                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_resolution                         <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", …
#> $ ts_time_interval_start                <dttm> 2019-10-31 23:00:00, 2019-10-31 23:00:00, 2019-10-31 23:00:00, 2019-10-…
#> $ ts_time_interval_end                  <dttm> 2019-11-30 23:00:00, 2019-11-30 23:00:00, 2019-11-30 23:00:00, 2019-11-…
#> $ ts_mrid                               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_point_dt_start                     <dttm> 2019-10-31 23:00:00, 2019-11-01 00:00:00, 2019-11-01 01:00:00, 2019-11-…
#> $ ts_point_quantity                     <dbl> 777, 1275, 1183, 1197, 1177, 1195, 1300, 101, 1211, 1300, 101, 1284, 105…
#> $ ts_quantity_measure_unit_name         <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "M…
```
