# Get Transfer Capacities Allocated with Third Countries — Explicit (12.1.H)

Capacity allocated outside the EU via explicit auction
(auction.Type=A02). Minimum time interval in query response ranges from
part of day to year, depending on the selected contract type. A
100-document limit applies.

## Usage

``` r
allocated_transfer_capacities_3rd_countries(
  eic_in = NULL,
  eic_out = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  contract_type = "A01",
  auction_category = "A04",
  position = 1L,
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic_in:

  Energy Identification Code of the in domain

- eic_out:

  Energy Identification Code of the out domain

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format

- contract_type:

  Contract market agreement type, valid values can be checked from
  contract_types table; "A01" = Day ahead "A02" = Weekly "A03" = Monthly
  "A04" = Yearly "A06" = Long Term "A07" = Intraday "A08" = Quarterly
  Defaults to "A01" (Day ahead)

- auction_category:

  Optional auction category; "A01" = Base "A02" = Peak "A03" = Off Peak
  "A04" = Hourly Defaults to "A04"

- position:

  Integer position for ts_classification_sequence_position. Defaults to
  1.

- tidy_output:

  Defaults to FALSE. If TRUE, then flatten nested tables.

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df <- entsoeapi::allocated_transfer_capacities_3rd_countries(
  eic_in = "10YSK-SEPS-----K",
  eic_out = "10YUA-WEPS-----0",
  period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2016-01-02", tz = "CET"),
  contract_type = "A01",
  auction_category = "A04",
  position = 1L,
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A94&auction.Type=A02&contract_MarketAgreement.Type=A01&in_Domain=10YSK-SEPS-----K&out_Domain=10YUA-WEPS-----0&auction.Category=A04&classificationSequence_AttributeInstanceComponent.Position=1&periodStart=201512312300&periodEnd=201601012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:07:49 GMT
#> <- content-type: text/xml
#> <- content-length: 1852
#> <- content-disposition: inline; filename="Transfer_capacities_allocated_with_third_countries_201512312300-201601012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived

str(df)
#> tibble [24 × 25] (S3: tbl_df/tbl/data.frame)
#>  $ ts_in_domain_mrid                    : chr [1:24] "10YSK-SEPS-----K" "10YSK-SEPS-----K" "10YSK-SEPS-----K" "10YSK-SEPS-----K" ...
#>  $ ts_in_domain_name                    : chr [1:24] "Slovak Republic" "Slovak Republic" "Slovak Republic" "Slovak Republic" ...
#>  $ ts_out_domain_mrid                   : chr [1:24] "10YUA-WEPS-----0" "10YUA-WEPS-----0" "10YUA-WEPS-----0" "10YUA-WEPS-----0" ...
#>  $ ts_out_domain_name                   : chr [1:24] "Ukrainian Area of Burshtyn island" "Ukrainian Area of Burshtyn island" "Ukrainian Area of Burshtyn island" "Ukrainian Area of Burshtyn island" ...
#>  $ type                                 : chr [1:24] "A94" "A94" "A94" "A94" ...
#>  $ type_def                             : chr [1:24] "Non EU allocations" "Non EU allocations" "Non EU allocations" "Non EU allocations" ...
#>  $ ts_contract_market_agreement_type    : chr [1:24] "A01" "A01" "A01" "A01" ...
#>  $ ts_contract_market_agreement_type_def: chr [1:24] "Daily contract" "Daily contract" "Daily contract" "Daily contract" ...
#>  $ ts_auction_mrid                      : chr [1:24] "A_E_D_SK-UA" "A_E_D_SK-UA" "A_E_D_SK-UA" "A_E_D_SK-UA" ...
#>  $ ts_auction_type                      : chr [1:24] "A02" "A02" "A02" "A02" ...
#>  $ ts_auction_type_def                  : chr [1:24] "Explicit" "Explicit" "Explicit" "Explicit" ...
#>  $ ts_auction_category                  : chr [1:24] "A04" "A04" "A04" "A04" ...
#>  $ ts_auction_category_def              : chr [1:24] "Hourly" "Hourly" "Hourly" "Hourly" ...
#>  $ ts_business_type                     : chr [1:24] "A34" "A34" "A34" "A34" ...
#>  $ ts_business_type_def                 : chr [1:24] "Capacity rights" "Capacity rights" "Capacity rights" "Capacity rights" ...
#>  $ created_date_time                    : POSIXct[1:24], format: "2026-03-04 22:07:49" "2026-03-04 22:07:49" ...
#>  $ revision_number                      : num [1:24] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution                        : chr [1:24] "PT60M" "PT60M" "PT60M" "PT60M" ...
#>  $ ts_time_interval_start               : POSIXct[1:24], format: "2015-12-31 23:00:00" "2015-12-31 23:00:00" ...
#>  $ ts_time_interval_end                 : POSIXct[1:24], format: "2016-01-01 23:00:00" "2016-01-01 23:00:00" ...
#>  $ ts_mrid                              : num [1:24] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start                    : POSIXct[1:24], format: "2015-12-31 23:00:00" "2016-01-01 00:00:00" ...
#>  $ ts_point_quantity                    : num [1:24] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ ts_quantity_measure_unit_name        : chr [1:24] "MAW" "MAW" "MAW" "MAW" ...
#>  $ ts_classification_sequence_position  : num [1:24] 1 1 1 1 1 1 1 1 1 1 ...
```
