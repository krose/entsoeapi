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
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A94&auction.Type=A02&contract_MarketAgreement.Type=A01&in_Domain=10YSK-SEPS-----K&out_Domain=10YUA-WEPS-----0&auction.Category=A04&classificationSequence_AttributeInstanceComponent.Position=1&periodStart=201512312300&periodEnd=201601012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 10 Mar 2026 19:47:46 GMT
#> <- content-type: text/xml
#> <- content-length: 1852
#> <- content-disposition: inline; filename="Transfer_capacities_allocated_with_third_countries_201512312300-201601012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived

dplyr::glimpse(df)
#> Rows: 24
#> Columns: 25
#> $ ts_in_domain_mrid                     <chr> "10YSK-SEPS-----K", "10YSK-SEPS-----K", "10YSK-SEPS-----K", "10YSK-SEPS-…
#> $ ts_in_domain_name                     <chr> "Slovak Republic", "Slovak Republic", "Slovak Republic", "Slovak Republi…
#> $ ts_out_domain_mrid                    <chr> "10YUA-WEPS-----0", "10YUA-WEPS-----0", "10YUA-WEPS-----0", "10YUA-WEPS-…
#> $ ts_out_domain_name                    <chr> "Ukrainian Area of Burshtyn island", "Ukrainian Area of Burshtyn island"…
#> $ type                                  <chr> "A94", "A94", "A94", "A94", "A94", "A94", "A94", "A94", "A94", "A94", "A…
#> $ type_def                              <chr> "Non EU allocations", "Non EU allocations", "Non EU allocations", "Non E…
#> $ ts_contract_market_agreement_type     <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A…
#> $ ts_contract_market_agreement_type_def <chr> "Daily contract", "Daily contract", "Daily contract", "Daily contract", …
#> $ ts_auction_mrid                       <chr> "A_E_D_SK-UA", "A_E_D_SK-UA", "A_E_D_SK-UA", "A_E_D_SK-UA", "A_E_D_SK-UA…
#> $ ts_auction_type                       <chr> "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A02", "A…
#> $ ts_auction_type_def                   <chr> "Explicit", "Explicit", "Explicit", "Explicit", "Explicit", "Explicit", …
#> $ ts_auction_category                   <chr> "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A04", "A…
#> $ ts_auction_category_def               <chr> "Hourly", "Hourly", "Hourly", "Hourly", "Hourly", "Hourly", "Hourly", "H…
#> $ ts_business_type                      <chr> "A34", "A34", "A34", "A34", "A34", "A34", "A34", "A34", "A34", "A34", "A…
#> $ ts_business_type_def                  <chr> "Capacity rights", "Capacity rights", "Capacity rights", "Capacity right…
#> $ created_date_time                     <dttm> 2026-03-10 19:47:46, 2026-03-10 19:47:46, 2026-03-10 19:47:46, 2026-03-…
#> $ revision_number                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_resolution                         <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", …
#> $ ts_time_interval_start                <dttm> 2015-12-31 23:00:00, 2015-12-31 23:00:00, 2015-12-31 23:00:00, 2015-12-3…
#> $ ts_time_interval_end                  <dttm> 2016-01-01 23:00:00, 2016-01-01 23:00:00, 2016-01-01 23:00:00, 2016-01-…
#> $ ts_mrid                               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_point_dt_start                     <dttm> 2015-12-31 23:00:00, 2016-01-01 00:00:00, 2016-01-01 01:00:00, 2016-01-…
#> $ ts_point_quantity                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
#> $ ts_quantity_measure_unit_name         <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "M…
#> $ ts_classification_sequence_position   <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
```
