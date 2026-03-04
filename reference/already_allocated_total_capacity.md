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
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A26&businessType=A29&contract_MarketAgreement.Type=A01&auction.Category=A04&in_Domain=10YDE-VE-------2&out_Domain=10YCZ-CEPS-----N&periodStart=202501312300&periodEnd=202502012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:12:41 GMT
#> <- content-type: text/xml
#> <- content-length: 1581
#> <- content-disposition: inline; filename="TOTAL_CAPACITY_ALLOCATED_202501312300-202502012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived

str(df)
#> tibble [24 × 23] (S3: tbl_df/tbl/data.frame)
#>  $ ts_in_domain_mrid                    : chr [1:24] "10YDE-VE-------2" "10YDE-VE-------2" "10YDE-VE-------2" "10YDE-VE-------2" ...
#>  $ ts_in_domain_name                    : chr [1:24] "Germany 50Hertz" "Germany 50Hertz" "Germany 50Hertz" "Germany 50Hertz" ...
#>  $ ts_out_domain_mrid                   : chr [1:24] "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" ...
#>  $ ts_out_domain_name                   : chr [1:24] "Czech Republic" "Czech Republic" "Czech Republic" "Czech Republic" ...
#>  $ type                                 : chr [1:24] "A26" "A26" "A26" "A26" ...
#>  $ type_def                             : chr [1:24] "Capacity document" "Capacity document" "Capacity document" "Capacity document" ...
#>  $ ts_contract_market_agreement_type    : chr [1:24] "A01" "A01" "A01" "A01" ...
#>  $ ts_contract_market_agreement_type_def: chr [1:24] "Daily contract" "Daily contract" "Daily contract" "Daily contract" ...
#>  $ ts_auction_type                      : chr [1:24] "A02" "A02" "A02" "A02" ...
#>  $ ts_auction_type_def                  : chr [1:24] "Explicit" "Explicit" "Explicit" "Explicit" ...
#>  $ ts_auction_category                  : chr [1:24] "A01" "A01" "A01" "A01" ...
#>  $ ts_auction_category_def              : chr [1:24] "Base" "Base" "Base" "Base" ...
#>  $ ts_business_type                     : chr [1:24] "A29" "A29" "A29" "A29" ...
#>  $ ts_business_type_def                 : chr [1:24] "Already allocated capacity (AAC)" "Already allocated capacity (AAC)" "Already allocated capacity (AAC)" "Already allocated capacity (AAC)" ...
#>  $ created_date_time                    : POSIXct[1:24], format: "2026-03-04 22:12:41" "2026-03-04 22:12:41" ...
#>  $ revision_number                      : num [1:24] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution                        : chr [1:24] "PT60M" "PT60M" "PT60M" "PT60M" ...
#>  $ ts_time_interval_start               : POSIXct[1:24], format: "2025-01-31 23:00:00" "2025-01-31 23:00:00" ...
#>  $ ts_time_interval_end                 : POSIXct[1:24], format: "2025-02-01 23:00:00" "2025-02-01 23:00:00" ...
#>  $ ts_mrid                              : num [1:24] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start                    : POSIXct[1:24], format: "2025-01-31 23:00:00" "2025-02-01 00:00:00" ...
#>  $ ts_point_quantity                    : num [1:24] 449 449 449 449 449 449 449 449 449 449 ...
#>  $ ts_quantity_measure_unit_name        : chr [1:24] "MAW" "MAW" "MAW" "MAW" ...
```
