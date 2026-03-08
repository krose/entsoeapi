# Get Implicit Auction — Net Positions (12.1.E)

Net positions resulting from implicit auctions per bidding zone.

## Usage

``` r
net_positions(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  contract_type = "A01",
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the bidding zone

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- contract_type:

  Contract market agreement type; "A01" = Day ahead "A07" = Intraday
  Defaults to "A01" (Day ahead)

- tidy_output:

  Defaults to FALSE. If TRUE, then flatten nested tables.

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df <- entsoeapi::net_positions(
  eic = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2015-12-31", tz = "CET"),
  period_end = lubridate::ymd(x = "2016-01-01", tz = "CET"),
  contract_type = "A01",
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A25&businessType=B09&contract_MarketAgreement.Type=A01&in_Domain=10YCZ-CEPS-----N&out_Domain=10YCZ-CEPS-----N&periodStart=201512302300&periodEnd=201512312300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Sun, 08 Mar 2026 23:53:08 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Implicit Allocations Net Positions Daily_201512302300-201512312300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ No additional definitions added!

dplyr::glimpse(df)
#> Rows: 24
#> Columns: 21
#> $ ts_in_domain_mrid                     <chr> "10YCZ-CEPS-----N", "REGION_CODE-----", "10YCZ-CEPS-----N", "10YCZ-CEPS-…
#> $ ts_in_domain_name                     <chr> "Czech Republic", NA, "Czech Republic", "Czech Republic", "Czech Republi…
#> $ ts_out_domain_mrid                    <chr> "REGION_CODE-----", "10YCZ-CEPS-----N", "REGION_CODE-----", "REGION_CODE…
#> $ ts_out_domain_name                    <chr> NA, "Czech Republic", NA, NA, NA, "Czech Republic", NA, NA, NA, NA, NA, …
#> $ type                                  <chr> "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A25", "A…
#> $ type_def                              <chr> "Allocation result document", "Allocation result document", "Allocation …
#> $ ts_contract_market_agreement_type     <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A…
#> $ ts_contract_market_agreement_type_def <chr> "Daily contract", "Daily contract", "Daily contract", "Daily contract", …
#> $ ts_auction_type                       <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A…
#> $ ts_auction_type_def                   <chr> "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", …
#> $ ts_business_type                      <chr> "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B09", "B…
#> $ ts_business_type_def                  <chr> "Net position", "Net position", "Net position", "Net position", "Net pos…
#> $ created_date_time                     <dttm> 2026-03-08 23:53:08, 2026-03-08 23:53:08, 2026-03-08 23:53:08, 2026-03-…
#> $ revision_number                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_resolution                         <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", …
#> $ ts_time_interval_start                <dttm> 2015-12-30 23:00:00, 2015-12-31 00:00:00, 2015-12-31 01:00:00, 2015-12-3…
#> $ ts_time_interval_end                  <dttm> 2015-12-31 00:00:00, 2015-12-31 01:00:00, 2015-12-31 04:00:00, 2015-12-…
#> $ ts_mrid                               <dbl> 1, 2, 3, 3, 3, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 7, 7, 7, 7, 7, 7, 7
#> $ ts_point_dt_start                     <dttm> 2015-12-30 23:00:00, 2015-12-31 00:00:00, 2015-12-31 01:00:00, 2015-12-…
#> $ ts_point_quantity                     <dbl> 44.8, 158.4, 65.8, 178.6, 26.2, 63.4, 198.4, 301.9, 157.2, 102.3, 111.6,…
#> $ ts_quantity_measure_unit_name         <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "…
```
