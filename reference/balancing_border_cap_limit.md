# Balancing Border Capacity Limitations (IFs 4.3 & 4.4)

This data item publish limitations on borders requested by participating
or affected TSOs.

## Usage

``` r
balancing_border_cap_limit(
  eic_in = NULL,
  eic_out = NULL,
  process_type = "A51",
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic_in:

  Energy Identification Code of an in LFC Area (LFA) or in Scheduling
  area (SCA)

- eic_out:

  Energy Identification Code of out an out LFC Area (LFA) or out
  Scheduling area (SCA)

- process_type:

  type of frequency restoration reserve "A47" mFRR "A51" aFRR "A63"
  Imbalance Netting

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
df <- entsoeapi::balancing_border_cap_limit(
  eic_in = "10YDE-RWENET---I",
  eic_out = "10YBE----------2",
  process_type = "A51",
  period_start = lubridate::ymd(x = "2022-06-22", tz = "CET"),
  period_end = lubridate::ymd(x = "2022-06-23", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A31&BusinessType=A26&processType=A51&In_Domain=10YDE-RWENET---I&Out_Domain=10YBE----------2&periodStart=202206212200&periodEnd=202206222200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 09 Mar 2026 20:03:52 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Balancing Border Capacity Limitations_202206212200-202206222200.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived

dplyr::glimpse(df)
#> Rows: 96
#> Columns: 24
#> $ domain_mrid            <chr> "10YDOM-REGION-1V", "10YDOM-REGION-1V", "10YDOM-REGION-1V", "10YDOM-REGION-1V", "10YDOM…
#> $ domain_name            <chr> "CWE Region", "CWE Region", "CWE Region", "CWE Region", "CWE Region", "CWE Region", "CW…
#> $ ts_in_domain_mrid      <chr> "10YDE-RWENET---I", "10YDE-RWENET---I", "10YDE-RWENET---I", "10YDE-RWENET---I", "10YDE-…
#> $ ts_in_domain_name      <chr> "Amprion GmbH", "Amprion GmbH", "Amprion GmbH", "Amprion GmbH", "Amprion GmbH", "Amprio…
#> $ ts_out_domain_mrid     <chr> "10YBE----------2", "10YBE----------2", "10YBE----------2", "10YBE----------2", "10YBE-…
#> $ ts_out_domain_name     <chr> "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Belgium",…
#> $ ts_product             <chr> "8716867000016", "8716867000016", "8716867000016", "8716867000016", "8716867000016", "8…
#> $ ts_product_def         <chr> "Active power", "Active power", "Active power", "Active power", "Active power", "Active…
#> $ type                   <chr> "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A3…
#> $ type_def               <chr> "Agreed capacity", "Agreed capacity", "Agreed capacity", "Agreed capacity", "Agreed cap…
#> $ process_type           <chr> "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A51", "A5…
#> $ process_type_def       <chr> "Automatic frequency restoration reserve", "Automatic frequency restoration reserve", "…
#> $ ts_business_type       <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A2…
#> $ ts_business_type_def   <chr> "Available transfer capacity (ATC)", "Available transfer capacity (ATC)", "Available tr…
#> $ created_date_time      <dttm> 2026-03-09 20:03:52, 2026-03-09 20:03:52, 2026-03-09 20:03:52, 2026-03-09 20:03:52, 20…
#> $ ts_reason_code         <chr> "B47", "B47", "B47", "B47", "B47", "B47", "B47", "B47", "B47", "B47", "B47", "B47", "B4…
#> $ ts_reason_text         <chr> "Operational security constraints", "Operational security constraints", "Operational se…
#> $ revision_number        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_resolution          <chr> "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M…
#> $ ts_time_interval_start <dttm> 2022-06-21 22:00:00, 2022-06-21 22:00:00, 2022-06-21 22:00:00, 2022-06-21 22:00:00, 20…
#> $ ts_time_interval_end   <dttm> 2022-06-22 22:00:00, 2022-06-22 22:00:00, 2022-06-22 22:00:00, 2022-06-22 22:00:00, 20…
#> $ ts_mrid                <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_point_dt_start      <dttm> 2022-06-21 22:00:00, 2022-06-21 22:15:00, 2022-06-21 22:30:00, 2022-06-21 22:45:00, 20…
#> $ ts_point_quantity      <dbl> 150, 150, 150, 150, 150, 150, 150, 150, 113, 114, 107, 108, 92, 96, 86, 79, 0, 2, 8, 19…
```
