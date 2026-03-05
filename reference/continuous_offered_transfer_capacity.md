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
#> <- date: Thu, 05 Mar 2026 16:11:53 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Offered Transfer Capacities Continuous_202405152200-202405162200.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ No additional definitions added!

str(df)
#> tibble [96 × 21] (S3: tbl_df/tbl/data.frame)
#>  $ ts_in_domain_mrid                    : chr [1:96] "10YNL----------L" "10YNL----------L" "10YNL----------L" "10YNL----------L" ...
#>  $ ts_in_domain_name                    : chr [1:96] "Netherlands" "Netherlands" "Netherlands" "Netherlands" ...
#>  $ ts_out_domain_mrid                   : chr [1:96] "10YBE----------2" "10YBE----------2" "10YBE----------2" "10YBE----------2" ...
#>  $ ts_out_domain_name                   : chr [1:96] "Belgium" "Belgium" "Belgium" "Belgium" ...
#>  $ type                                 : chr [1:96] "B33" "B33" "B33" "B33" ...
#>  $ type_def                             : chr [1:96] "Published offered capacity" "Published offered capacity" "Published offered capacity" "Published offered capacity" ...
#>  $ ts_contract_market_agreement_type    : chr [1:96] "A07" "A07" "A07" "A07" ...
#>  $ ts_contract_market_agreement_type_def: chr [1:96] "Intraday contract" "Intraday contract" "Intraday contract" "Intraday contract" ...
#>  $ ts_auction_type                      : chr [1:96] "A08" "A08" "A08" "A08" ...
#>  $ ts_auction_type_def                  : chr [1:96] "Continuous" "Continuous" "Continuous" "Continuous" ...
#>  $ ts_business_type                     : chr [1:96] "A31" "A31" "A31" "A31" ...
#>  $ ts_business_type_def                 : chr [1:96] "Offered Capacity" "Offered Capacity" "Offered Capacity" "Offered Capacity" ...
#>  $ created_date_time                    : POSIXct[1:96], format: "2026-03-05 16:11:53" "2026-03-05 16:11:53" "2026-03-05 16:11:53" "2026-03-05 16:11:53" ...
#>  $ revision_number                      : num [1:96] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution                        : chr [1:96] "PT15M" "PT15M" "PT15M" "PT15M" ...
#>  $ ts_time_interval_start               : POSIXct[1:96], format: "2024-05-15 22:00:00" "2024-05-15 22:00:00" "2024-05-15 22:00:00" "2024-05-15 22:00:00" ...
#>  $ ts_time_interval_end                 : POSIXct[1:96], format: "2024-05-16 22:00:00" "2024-05-16 22:00:00" "2024-05-16 22:00:00" "2024-05-16 22:00:00" ...
#>  $ ts_mrid                              : num [1:96] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start                    : POSIXct[1:96], format: "2024-05-15 22:00:00" "2024-05-15 22:15:00" "2024-05-15 22:30:00" "2024-05-15 22:45:00" ...
#>  $ ts_point_quantity                    : num [1:96] 5.1 0 1.6 0 126.9 ...
#>  $ ts_quantity_measure_unit_name        : chr [1:96] "MAW" "MAW" "MAW" "MAW" ...
```
