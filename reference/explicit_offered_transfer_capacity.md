# Get Explicit Offered Transfer Capacity (11.1.A)

Explicit offered transfer capacity values.

## Usage

``` r
explicit_offered_transfer_capacity(
  eic_in = NULL,
  eic_out = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
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

- contract_type:

  Contract market agreement type, valid values can be checked from
  contract_types table; A01 = Day ahead A02 = Weekly A03 = Monthly A04 =
  Yearly A06 = Long Term A07 = Intraday A08 = Quarterly Defaults to
  "A01" (Day ahead)

- tidy_output:

  Defaults to TRUE. If TRUE, then flatten nested tables.

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df <- entsoeapi::explicit_offered_transfer_capacity(
  eic_in = "10YBE----------2",
  eic_out = "10YGB----------A",
  period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
  period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
  contract_type = "A01",
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A31&auction.Type=A02&contract_MarketAgreement.Type=A01&in_Domain=10YBE----------2&out_Domain=10YGB----------A&periodStart=202308152200&periodEnd=202308162200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 05 Mar 2026 16:12:26 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="OFFERED_TRANSFER_CAPACITIES_EXPLICIT_202308152200-202308162200.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived

str(df)
#> tibble [24 × 24] (S3: tbl_df/tbl/data.frame)
#>  $ ts_in_domain_mrid                    : chr [1:24] "10YBE----------2" "10YBE----------2" "10YBE----------2" "10YBE----------2" ...
#>  $ ts_in_domain_name                    : chr [1:24] "Belgium" "Belgium" "Belgium" "Belgium" ...
#>  $ ts_out_domain_mrid                   : chr [1:24] "10YGB----------A" "10YGB----------A" "10YGB----------A" "10YGB----------A" ...
#>  $ ts_out_domain_name                   : chr [1:24] "Great Britain" "Great Britain" "Great Britain" "Great Britain" ...
#>  $ type                                 : chr [1:24] "A31" "A31" "A31" "A31" ...
#>  $ type_def                             : chr [1:24] "Agreed capacity" "Agreed capacity" "Agreed capacity" "Agreed capacity" ...
#>  $ ts_contract_market_agreement_type    : chr [1:24] "A01" "A01" "A01" "A01" ...
#>  $ ts_contract_market_agreement_type_def: chr [1:24] "Daily contract" "Daily contract" "Daily contract" "Daily contract" ...
#>  $ ts_auction_type                      : chr [1:24] "A02" "A02" "A02" "A02" ...
#>  $ ts_auction_type_def                  : chr [1:24] "Explicit" "Explicit" "Explicit" "Explicit" ...
#>  $ ts_auction_category                  : chr [1:24] "A04" "A04" "A04" "A04" ...
#>  $ ts_auction_category_def              : chr [1:24] "Hourly" "Hourly" "Hourly" "Hourly" ...
#>  $ ts_business_type                     : chr [1:24] "A31" "A31" "A31" "A31" ...
#>  $ ts_business_type_def                 : chr [1:24] "Offered Capacity" "Offered Capacity" "Offered Capacity" "Offered Capacity" ...
#>  $ created_date_time                    : POSIXct[1:24], format: "2026-03-05 16:12:26" "2026-03-05 16:12:26" "2026-03-05 16:12:26" "2026-03-05 16:12:26" ...
#>  $ revision_number                      : num [1:24] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution                        : chr [1:24] "PT60M" "PT60M" "PT60M" "PT60M" ...
#>  $ ts_time_interval_start               : POSIXct[1:24], format: "2023-08-15 22:00:00" "2023-08-15 22:00:00" "2023-08-15 22:00:00" "2023-08-15 22:00:00" ...
#>  $ ts_time_interval_end                 : POSIXct[1:24], format: "2023-08-16 22:00:00" "2023-08-16 22:00:00" "2023-08-16 22:00:00" "2023-08-16 22:00:00" ...
#>  $ ts_mrid                              : num [1:24] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start                    : POSIXct[1:24], format: "2023-08-15 22:00:00" "2023-08-15 23:00:00" "2023-08-16 00:00:00" "2023-08-16 01:00:00" ...
#>  $ ts_point_quantity                    : num [1:24] 1012 1012 1012 1012 1012 ...
#>  $ ts_quantity_measure_unit_name        : chr [1:24] "MAW" "MAW" "MAW" "MAW" ...
#>  $ ts_classification_sequence_position  : num [1:24] 1 1 1 1 1 1 1 1 1 1 ...
```
