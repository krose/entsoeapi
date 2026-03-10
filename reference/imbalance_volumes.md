# Get Imbalance Volumes (17.1.H)

Total imbalance volumes of the control area. One year range limit
applies.

## Usage

``` r
imbalance_volumes(
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
df <- entsoeapi::imbalance_volumes(
  eic = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A86&controlArea_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 10 Mar 2026 19:48:51 GMT
#> <- content-type: application/zip
#> <- content-length: 1096
#> <- content-disposition: attachment; filename="Total_Imbalance_Volumes_r3_202312312300-202401012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpDj0v66/001-TOTAL_IMBALANCE_VOLUMES_R3_202312312300-202401012300.xml has been read in

dplyr::glimpse(df)
#> Rows: 24
#> Columns: 21
#> $ control_area_domain_mrid      <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", …
#> $ control_area_domain_name      <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", "Czech R…
#> $ doc_status_value              <chr> "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A0…
#> $ doc_status                    <chr> "Allocated capacity schedule", "Allocated capacity schedule", "Allocated capacit…
#> $ type                          <chr> "A86", "A86", "A86", "A86", "A86", "A86", "A86", "A86", "A86", "A86", "A86", "A8…
#> $ type_def                      <chr> "Imbalance volume", "Imbalance volume", "Imbalance volume", "Imbalance volume", …
#> $ process_type                  <chr> "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A1…
#> $ process_type_def              <chr> "Realised", "Realised", "Realised", "Realised", "Realised", "Realised", "Realise…
#> $ ts_flow_direction             <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A0…
#> $ ts_flow_direction_def         <chr> "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "UP", "U…
#> $ ts_business_type              <chr> "A19", "A19", "A19", "A19", "A19", "A19", "A19", "A19", "A19", "A19", "A19", "A1…
#> $ ts_business_type_def          <chr> "Balance energy deviation", "Balance energy deviation", "Balance energy deviatio…
#> $ created_date_time             <dttm> 2026-03-10 19:48:51, 2026-03-10 19:48:51, 2026-03-10 19:48:51, 2026-03-10 19:48…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_resolution                 <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M",…
#> $ ts_time_interval_start        <dttm> 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00:…
#> $ ts_time_interval_end          <dttm> 2024-01-01 11:00:00, 2024-01-01 11:00:00, 2024-01-01 11:00:00, 2024-01-01 11:00…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2
#> $ ts_point_dt_start             <dttm> 2023-12-31 23:00:00, 2024-01-01 00:00:00, 2024-01-01 01:00:00, 2024-01-01 02:00…
#> $ ts_point_quantity             <dbl> 205.87, 203.35, 199.25, 196.33, 197.76, 162.05, 257.45, 216.98, 159.70, 66.15, 7…
#> $ ts_quantity_measure_unit_name <chr> "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "M…
```
