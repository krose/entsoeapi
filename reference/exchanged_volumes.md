# Get Exchanged Volumes (aFRR 3.16, mFRR 3.17)

The net position informs whether the given area imports or exports
energy. Those rows which hold the queried eic value in the
'ts_connecting_domain_mrid' column show the export value. Those rows
which hold the queried eic value in the 'ts_acquiring_domain_mrid'
column show the import value.

## Usage

``` r
exchanged_volumes(
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

  Energy Identification Code of the area

- process_type:

  type of frequency restoration reserve "A51" aFRR "A60" mFRR with
  scheduled activation "A61" mFRR with direct activation

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format One day range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format One day range limit applies

- tidy_output:

  Defaults to TRUE. flatten nested tables

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df1 <- entsoeapi::exchanged_volumes(
  eic = "10YCZ-CEPS-----N",
  process_type = "A51",
  period_start = lubridate::ymd(x = "2022-08-16", tz = "CET"),
  period_end = lubridate::ymd(x = "2022-08-17", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=B17&processType=A51&Acquiring_domain=10YCZ-CEPS-----N&Connecting_Domain=10YCZ-CEPS-----N&periodStart=202208152200&periodEnd=202208162200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 10 Mar 2026 19:48:14 GMT
#> <- content-type: application/zip
#> <- content-length: 179308
#> <- content-disposition: attachment; filename="Netted and Exchanged Volumes [IFs IN 3.10, mFRR 3.17, aFRR 3.16]_202208152200-202208162200.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpDj0v66/001-NETTED_AND_EXCHANGED_VOLUMES_202208152200-202208162200.xml has been read in

dplyr::glimpse(df1)
#> Rows: 43,200
#> Columns: 23
#> $ area_domain_mrid              <chr> "10Y1001C--00090V", "10Y1001C--00090V", "10Y1001C--00090V", "10Y1001C--00090V", …
#> $ area_domain_name              <chr> "aFRR Region", "aFRR Region", "aFRR Region", "aFRR Region", "aFRR Region", "aFRR…
#> $ ts_connecting_domain_mrid     <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", …
#> $ ts_connecting_domain_name     <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", "Czech R…
#> $ ts_acquiring_domain_mrid      <chr> "10Y1001C--00090V", "10Y1001C--00090V", "10Y1001C--00090V", "10Y1001C--00090V", …
#> $ ts_acquiring_domain_name      <chr> "aFRR Region", "aFRR Region", "aFRR Region", "aFRR Region", "aFRR Region", "aFRR…
#> $ doc_status_value              <chr> "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A0…
#> $ doc_status                    <chr> "Allocated capacity schedule", "Allocated capacity schedule", "Allocated capacit…
#> $ type                          <chr> "B17", "B17", "B17", "B17", "B17", "B17", "B17", "B17", "B17", "B17", "B17", "B1…
#> $ type_def                      <chr> "Aggregated netted external TSO schedule document", "Aggregated netted external …
#> $ process_type                  <chr> "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A5…
#> $ process_type_def              <chr> "Automatic frequency restoration reserve", "Automatic frequency restoration rese…
#> $ ts_business_type              <chr> "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B0…
#> $ ts_business_type_def          <chr> "Net position", "Net position", "Net position", "Net position", "Net position", …
#> $ created_date_time             <dttm> 2026-03-10 19:48:14, 2026-03-10 19:48:14, 2026-03-10 19:48:14, 2026-03-10 19:48…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution                 <chr> "PT4S", "PT4S", "PT4S", "PT4S", "PT4S", "PT4S", "PT4S", "PT4S", "PT4S", "PT4S", …
#> $ ts_time_interval_start        <dttm> 2022-08-15 22:00:00, 2022-08-15 22:00:00, 2022-08-15 22:00:00, 2022-08-15 22:00…
#> $ ts_time_interval_end          <dttm> 2022-08-16 22:00:00, 2022-08-16 22:00:00, 2022-08-16 22:00:00, 2022-08-16 22:00…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start             <dttm> 2022-08-15 22:00:00, 2022-08-15 22:00:04, 2022-08-15 22:00:08, 2022-08-15 22:00…
#> $ ts_point_quantity             <dbl> 0.0011, 0.0011, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0040, 0.0098, 0.0105, …
#> $ ts_quantity_measure_unit_name <chr> "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MW…

df2 <- entsoeapi::exchanged_volumes(
  eic = "10YCZ-CEPS-----N",
  process_type = "A60",
  period_start = lubridate::ymd(x = "2024-07-11", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-07-12", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=B17&processType=A60&Acquiring_domain=10YCZ-CEPS-----N&Connecting_Domain=10YCZ-CEPS-----N&periodStart=202407102200&periodEnd=202407112200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 10 Mar 2026 19:48:25 GMT
#> <- content-type: application/zip
#> <- content-length: 1173
#> <- content-disposition: attachment; filename="Netted and Exchanged Volumes [IFs IN 3.10, mFRR 3.17, aFRR 3.16]_202407102200-202407112200.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpDj0v66/001-NETTED_AND_EXCHANGED_VOLUMES_202407102200-202407112200.xml has been read in
#> ℹ No additional definitions added!

dplyr::glimpse(df2)
#> Rows: 96
#> Columns: 21
#> $ area_domain_mrid              <chr> "10Y1001C--00085O", "10Y1001C--00085O", "10Y1001C--00085O", "10Y1001C--00085O", …
#> $ area_domain_name              <chr> "mFRR region", "mFRR region", "mFRR region", "mFRR region", "mFRR region", "mFRR…
#> $ ts_connecting_domain_mrid     <chr> "10Y1001C--00085O", "10Y1001C--00085O", "10Y1001C--00085O", "10Y1001C--00085O", …
#> $ ts_connecting_domain_name     <chr> "mFRR region", "mFRR region", "mFRR region", "mFRR region", "mFRR region", "mFRR…
#> $ ts_acquiring_domain_mrid      <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", …
#> $ ts_acquiring_domain_name      <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", "Czech R…
#> $ type                          <chr> "B17", "B17", "B17", "B17", "B17", "B17", "B17", "B17", "B17", "B17", "B17", "B1…
#> $ type_def                      <chr> "Aggregated netted external TSO schedule document", "Aggregated netted external …
#> $ process_type                  <chr> "A60", "A60", "A60", "A60", "A60", "A60", "A60", "A60", "A60", "A60", "A60", "A6…
#> $ process_type_def              <chr> "Scheduled activation mFRR", "Scheduled activation mFRR", "Scheduled activation …
#> $ ts_business_type              <chr> "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B0…
#> $ ts_business_type_def          <chr> "Net position", "Net position", "Net position", "Net position", "Net position", …
#> $ created_date_time             <dttm> 2026-03-10 19:48:25, 2026-03-10 19:48:25, 2026-03-10 19:48:25, 2026-03-10 19:48…
#> $ revision_number               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution                 <chr> "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M",…
#> $ ts_time_interval_start        <dttm> 2024-07-10 22:00:00, 2024-07-10 22:00:00, 2024-07-10 22:00:00, 2024-07-10 22:00…
#> $ ts_time_interval_end          <dttm> 2024-07-11 22:00:00, 2024-07-11 22:00:00, 2024-07-11 22:00:00, 2024-07-11 22:00…
#> $ ts_mrid                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start             <dttm> 2024-07-10 22:00:00, 2024-07-10 22:15:00, 2024-07-10 22:30:00, 2024-07-10 22:45…
#> $ ts_point_quantity             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ ts_quantity_measure_unit_name <chr> "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MW…
```
