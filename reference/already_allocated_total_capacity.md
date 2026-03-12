# Get Already Allocated Total Capacity (12.1.C)

Total capacity allocated, for all time horizons (including Intra-Day)
after each allocation process per market time unit.

## Usage

``` r
already_allocated_total_capacity(
  eic_in = NULL,
  eic_out = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  auction_category = "A04",
  contract_type = "A01",
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

- auction_category:

  A01 = Base A02 = Peak A03 = Off Peak A04 = Hourly Defaults to "A04"
  (Hourly)

- contract_type:

  Contract market agreement type, valid values can be checked from
  contract_types table; A01 = Daily A02 = Weekly A03 = Monthly A04 =
  Yearly A06 = Long Term A07 = Intraday A08 = Quarterly Defaults to
  "A01" (Daily)

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
df <- entsoeapi::already_allocated_total_capacity(
  eic_in = "10YDE-VE-------2",
  eic_out = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2025-02-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2025-02-02", tz = "CET"),
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A26&businessType=A29&contract_MarketAgreement.Type=A01&auction.Category=A04&in_Domain=10YDE-VE-------2&out_Domain=10YCZ-CEPS-----N&periodStart=202501312300&periodEnd=202502012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 12 Mar 2026 10:21:11 GMT
#> <- content-type: text/xml
#> <- content-length: 1581
#> <- content-disposition: inline; filename="TOTAL_CAPACITY_ALLOCATED_202501312300-202502012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived

dplyr::glimpse(df)
#> Rows: 24
#> Columns: 23
#> $ ts_in_domain_mrid                     <chr> "10YDE-VE-------2", "10YDE-VE-------2", "10YDE-VE-------2", "10YDE-VE---…
#> $ ts_in_domain_name                     <chr> "Germany 50Hertz", "Germany 50Hertz", "Germany 50Hertz", "Germany 50Hert…
#> $ ts_out_domain_mrid                    <chr> "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N", "10YCZ-CEPS-…
#> $ ts_out_domain_name                    <chr> "Czech Republic", "Czech Republic", "Czech Republic", "Czech Republic", …
#> $ type                                  <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A…
#> $ type_def                              <chr> "Capacity document", "Capacity document", "Capacity document", "Capacity…
#> $ ts_contract_market_agreement_type     <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A…
#> $ ts_contract_market_agreement_type_def <chr> "Daily contract", "Daily contract", "Daily contract", "Daily contract", …
#> $ ts_auction_type                       <chr> "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A…
#> $ ts_auction_type_def                   <chr> "Explicit", "Explicit", "Explicit", "Explicit", "Explicit", "Explicit", …
#> $ ts_auction_category                   <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A…
#> $ ts_auction_category_def               <chr> "Base", "Base", "Base", "Base", "Base", "Base", "Base", "Base", "Base", …
#> $ ts_business_type                      <chr> "A29", "A29", "A29", "A29", "A29", "A29", "A29", "A29", "A29", "A29", "A…
#> $ ts_business_type_def                  <chr> "Already allocated capacity (AAC)", "Already allocated capacity (AAC)", …
#> $ created_date_time                     <dttm> 2026-03-12 10:21:11, 2026-03-12 10:21:11, 2026-03-12 10:21:11, 2026-03-…
#> $ revision_number                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_resolution                         <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", …
#> $ ts_time_interval_start                <dttm> 2025-01-31 23:00:00, 2025-01-31 23:00:00, 2025-01-31 23:00:00, 2025-01-3…
#> $ ts_time_interval_end                  <dttm> 2025-02-01 23:00:00, 2025-02-01 23:00:00, 2025-02-01 23:00:00, 2025-02-…
#> $ ts_mrid                               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_point_dt_start                     <dttm> 2025-01-31 23:00:00, 2025-02-01 00:00:00, 2025-02-01 01:00:00, 2025-02-…
#> $ ts_point_quantity                     <dbl> 449, 449, 449, 449, 449, 449, 449, 449, 449, 449, 449, 449, 449, 449, 44…
#> $ ts_quantity_measure_unit_name         <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "…
```
