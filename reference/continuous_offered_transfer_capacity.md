# Get Continuous Offered Transfer Capacity (11.1)

Continuous offered transfer capacity values.

## Usage

``` r
continuous_offered_transfer_capacity(
  eic_in = NULL,
  eic_out = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic_in:

  Energy Identification Code of the bidding zone or control area (TSO)

- eic_out:

  Energy Identification Code of the bidding zone or control area (TSO)

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
df <- entsoeapi::continuous_offered_transfer_capacity(
  eic_in = "10YNL----------L",
  eic_out = "10YBE----------2",
  period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A31&auction.Type=A08&contract_MarketAgreement.Type=A07&in_Domain=10YNL----------L&out_Domain=10YBE----------2&periodStart=202405152200&periodEnd=202405162200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 17 Mar 2026 13:12:06 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Offered Transfer Capacities Continuous_202405152200-202405162200.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ pulling area_eic_name table from cache
#> ℹ No additional definitions added!

dplyr::glimpse(df)
#> Rows: 96
#> Columns: 21
#> $ ts_in_domain_mrid                     <chr> "10YNL----------L", "10YNL----------L", "10YNL----------L", "10YNL------…
#> $ ts_in_domain_name                     <chr> "Netherlands", "Netherlands", "Netherlands", "Netherlands", "Netherlands…
#> $ ts_out_domain_mrid                    <chr> "10YBE----------2", "10YBE----------2", "10YBE----------2", "10YBE------…
#> $ ts_out_domain_name                    <chr> "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Belgium", "Belgi…
#> $ type                                  <chr> "B33", "B33", "B33", "B33", "B33", "B33", "B33", "B33", "B33", "B33", "B…
#> $ type_def                              <chr> "Published offered capacity", "Published offered capacity", "Published o…
#> $ ts_contract_market_agreement_type     <chr> "A07", "A07", "A07", "A07", "A07", "A07", "A07", "A07", "A07", "A07", "A…
#> $ ts_contract_market_agreement_type_def <chr> "Intraday contract", "Intraday contract", "Intraday contract", "Intraday…
#> $ ts_auction_type                       <chr> "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A…
#> $ ts_auction_type_def                   <chr> "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "C…
#> $ ts_business_type                      <chr> "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A…
#> $ ts_business_type_def                  <chr> "Offered Capacity", "Offered Capacity", "Offered Capacity", "Offered Cap…
#> $ created_date_time                     <dttm> 2026-03-17 13:12:06, 2026-03-17 13:12:06, 2026-03-17 13:12:06, 2026-03-…
#> $ revision_number                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_resolution                         <chr> "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", …
#> $ ts_time_interval_start                <dttm> 2024-05-15 22:00:00, 2024-05-15 22:00:00, 2024-05-15 22:00:00, 2024-05-…
#> $ ts_time_interval_end                  <dttm> 2024-05-16 22:00:00, 2024-05-16 22:00:00, 2024-05-16 22:00:00, 2024-05-…
#> $ ts_mrid                               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_point_dt_start                     <dttm> 2024-05-15 22:00:00, 2024-05-15 22:15:00, 2024-05-15 22:30:00, 2024-05-…
#> $ ts_point_quantity                     <dbl> 5.1, 0.0, 1.6, 0.0, 126.9, 166.4, 129.4, 107.7, 0.0, 0.1, 14.3, 0.5, 0.0…
#> $ ts_quantity_measure_unit_name         <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "M…
```
