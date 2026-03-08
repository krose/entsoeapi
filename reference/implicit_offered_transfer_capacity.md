# Get Implicit Offered Transfer Capacity (11.1)

Implicit offered transfer capacity values.

## Usage

``` r
implicit_offered_transfer_capacity(
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

  Contract market agreement type A01 = Day ahead A07 = Intraday Defaults
  to "A01" (Day ahead)

- tidy_output:

  Defaults to TRUE. If TRUE, then flatten nested tables.

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df <- entsoeapi::implicit_offered_transfer_capacity(
  eic_in = "10Y1001A1001A82H",
  eic_out = "10YDK-1--------W",
  period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
  period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
  contract_type = "A01",
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A31&auction.Type=A01&contract_MarketAgreement.Type=A01&in_Domain=10Y1001A1001A82H&out_Domain=10YDK-1--------W&periodStart=202308152200&periodEnd=202308162200&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Sun, 08 Mar 2026 23:53:00 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="OFFERED_TRANSFER_CAPACITIES_IMPLICIT_202308152200-202308162200.xml"
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
#> $ ts_in_domain_mrid                     <chr> "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A100…
#> $ ts_in_domain_name                     <chr> "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Luxemburg", "Germany_…
#> $ ts_out_domain_mrid                    <chr> "10YDK-1--------W", "10YDK-1--------W", "10YDK-1--------W", "10YDK-1----…
#> $ ts_out_domain_name                    <chr> "Denmark DK1", "Denmark DK1", "Denmark DK1", "Denmark DK1", "Denmark DK1…
#> $ type                                  <chr> "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A…
#> $ type_def                              <chr> "Agreed capacity", "Agreed capacity", "Agreed capacity", "Agreed capacit…
#> $ ts_contract_market_agreement_type     <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A…
#> $ ts_contract_market_agreement_type_def <chr> "Daily contract", "Daily contract", "Daily contract", "Daily contract", …
#> $ ts_auction_type                       <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A…
#> $ ts_auction_type_def                   <chr> "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", …
#> $ ts_business_type                      <chr> "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A31", "A…
#> $ ts_business_type_def                  <chr> "Offered Capacity", "Offered Capacity", "Offered Capacity", "Offered Cap…
#> $ created_date_time                     <dttm> 2026-03-08 23:53:00, 2026-03-08 23:53:00, 2026-03-08 23:53:00, 2026-03-…
#> $ revision_number                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_resolution                         <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", …
#> $ ts_time_interval_start                <dttm> 2023-08-15 22:00:00, 2023-08-15 22:00:00, 2023-08-15 22:00:00, 2023-08-1…
#> $ ts_time_interval_end                  <dttm> 2023-08-16 22:00:00, 2023-08-16 22:00:00, 2023-08-16 22:00:00, 2023-08-…
#> $ ts_mrid                               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_point_dt_start                     <dttm> 2023-08-15 22:00:00, 2023-08-15 23:00:00, 2023-08-16 00:00:00, 2023-08-…
#> $ ts_point_quantity                     <dbl> 2500, 2500, 2500, 2500, 2500, 2500, 2500, 2500, 2500, 2500, 2380, 2190, …
#> $ ts_quantity_measure_unit_name         <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "…
```
