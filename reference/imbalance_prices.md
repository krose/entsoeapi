# Get Imbalance Prices (17.1.G)

Imbalance prices of the control area. One year range limit applies.

## Usage

``` r
imbalance_prices(
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

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with the queried data, or `NULL` if no data is available for the given
parameters.

## Examples

``` r
df <- entsoeapi::imbalance_prices(
  eic = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A85&controlArea_Domain=10YCZ-CEPS-----N&periodStart=202312312300&periodEnd=202401012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 12 Mar 2026 10:22:29 GMT
#> <- content-type: application/zip
#> <- content-length: 1563
#> <- content-disposition: attachment; filename="Imbalance Prices_202312312300-202401012300.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpqMxwAp/001-IMBALANCE_PRICES_R3_202312312300-202401012300.xml has been read in

dplyr::glimpse(df)
#> Rows: 72
#> Columns: 19
#> $ area_domain_mrid           <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10…
#> $ area_domain_name           <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", "Czech Repu…
#> $ doc_status_value           <chr> "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02",…
#> $ doc_status                 <chr> "Allocated capacity schedule", "Allocated capacity schedule", "Allocated capacity s…
#> $ type                       <chr> "A85", "A85", "A85", "A85", "A85", "A85", "A85", "A85", "A85", "A85", "A85", "A85",…
#> $ type_def                   <chr> "Imbalance prices", "Imbalance prices", "Imbalance prices", "Imbalance prices", "Im…
#> $ process_type               <chr> "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16",…
#> $ process_type_def           <chr> "Realised", "Realised", "Realised", "Realised", "Realised", "Realised", "Realised",…
#> $ ts_business_type           <chr> "A19", "A19", "A19", "A19", "A19", "A19", "A19", "A19", "A19", "A19", "A19", "A19",…
#> $ ts_business_type_def       <chr> "Balance energy deviation", "Balance energy deviation", "Balance energy deviation",…
#> $ created_date_time          <dttm> 2026-03-12 10:22:29, 2026-03-12 10:22:29, 2026-03-12 10:22:29, 2026-03-12 10:22:29…
#> $ revision_number            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution              <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "P…
#> $ ts_time_interval_start     <dttm> 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2023-12-31 23:00:00…
#> $ ts_time_interval_end       <dttm> 2024-01-01 23:00:00, 2024-01-01 23:00:00, 2024-01-01 23:00:00, 2024-01-01 23:00:00…
#> $ ts_mrid                    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start          <dttm> 2023-12-31 23:00:00, 2023-12-31 23:00:00, 2024-01-01 00:00:00, 2024-01-01 00:00:00…
#> $ ts_currency_unit_name      <chr> "CZK", "CZK", "CZK", "CZK", "CZK", "CZK", "CZK", "CZK", "CZK", "CZK", "CZK", "CZK",…
#> $ ts_price_measure_unit_name <chr> "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH",…
```
