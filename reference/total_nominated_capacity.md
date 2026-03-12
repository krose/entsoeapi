# Get Total Nominated Capacity (12.1.B)

Aggregated capacity nominated by market participants from time horizons
(including Intra-Day) corresponding to explicit allocations, agreed
between the TSOs and confirmed to the market.

## Usage

``` r
total_nominated_capacity(
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
df <- entsoeapi::total_nominated_capacity(
  eic_in = "10YDE-VE-------2",
  eic_out = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2019-02-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2019-03-01", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A26&businessType=B08&in_Domain=10YDE-VE-------2&out_Domain=10YCZ-CEPS-----N&periodStart=201901312300&periodEnd=201902282300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 12 Mar 2026 09:24:50 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Total Capacity Nominated_201901312300-201902282300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ No additional definitions added!

dplyr::glimpse(df)
#> Rows: 2,016
#> Columns: 19
#> $ ts_in_domain_mrid                     <chr> "10YDE-VE-------2", "10YDE-VE-------2", "10YDE-VE-------2", "10YDE-VE---…
#> $ ts_in_domain_name                     <chr> "Germany 50Hertz", "Germany 50Hertz", "Germany 50Hertz", "Germany 50Hert…
#> $ ts_out_domain_mrid                    <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-…
#> $ ts_out_domain_name                    <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", …
#> $ type                                  <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A…
#> $ type_def                              <chr> "Capacity document", "Capacity document", "Capacity document", "Capacity…
#> $ ts_contract_market_agreement_type     <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A…
#> $ ts_contract_market_agreement_type_def <chr> "Daily contract", "Daily contract", "Daily contract", "Daily contract", …
#> $ ts_business_type                      <chr> "B08", "B08", "B08", "B08", "B08", "B08", "B08", "B08", "B08", "B08", "B…
#> $ ts_business_type_def                  <chr> "Total nominated capacity", "Total nominated capacity", "Total nominated…
#> $ created_date_time                     <dttm> 2026-03-12 09:24:50, 2026-03-12 09:24:50, 2026-03-12 09:24:50, 2026-03-…
#> $ revision_number                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_resolution                         <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", …
#> $ ts_time_interval_start                <dttm> 2019-01-31 23:00:00, 2019-01-31 23:00:00, 2019-01-31 23:00:00, 2019-01-…
#> $ ts_time_interval_end                  <dttm> 2019-02-28 23:00:00, 2019-02-28 23:00:00, 2019-02-28 23:00:00, 2019-02-…
#> $ ts_mrid                               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_point_dt_start                     <dttm> 2019-01-31 23:00:00, 2019-02-01 00:00:00, 2019-02-01 01:00:00, 2019-02-…
#> $ ts_point_quantity                     <dbl> 123, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 189, 315, 270, 185,…
#> $ ts_quantity_measure_unit_name         <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "M…
```
