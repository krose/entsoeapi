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
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A26&businessType=B08&in_Domain=10YDE-VE-------2&out_Domain=10YCZ-CEPS-----N&periodStart=201901312300&periodEnd=201902282300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:10:09 GMT
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

str(df)
#> tibble [2,016 × 19] (S3: tbl_df/tbl/data.frame)
#>  $ ts_in_domain_mrid                    : chr [1:2016] "10YDE-VE-------2" "10YDE-VE-------2" "10YDE-VE-------2" "10YDE-VE-------2" ...
#>  $ ts_in_domain_name                    : chr [1:2016] "Germany 50Hertz" "Germany 50Hertz" "Germany 50Hertz" "Germany 50Hertz" ...
#>  $ ts_out_domain_mrid                   : chr [1:2016] "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" "10YCZ-CEPS-----N" ...
#>  $ ts_out_domain_name                   : chr [1:2016] "Czech Republic" "Czech Republic" "Czech Republic" "Czech Republic" ...
#>  $ type                                 : chr [1:2016] "A26" "A26" "A26" "A26" ...
#>  $ type_def                             : chr [1:2016] "Capacity document" "Capacity document" "Capacity document" "Capacity document" ...
#>  $ ts_contract_market_agreement_type    : chr [1:2016] "A01" "A01" "A01" "A01" ...
#>  $ ts_contract_market_agreement_type_def: chr [1:2016] "Daily contract" "Daily contract" "Daily contract" "Daily contract" ...
#>  $ ts_business_type                     : chr [1:2016] "B08" "B08" "B08" "B08" ...
#>  $ ts_business_type_def                 : chr [1:2016] "Total nominated capacity" "Total nominated capacity" "Total nominated capacity" "Total nominated capacity" ...
#>  $ created_date_time                    : POSIXct[1:2016], format: "2026-03-04 22:10:09" "2026-03-04 22:10:09" ...
#>  $ revision_number                      : num [1:2016] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_resolution                        : chr [1:2016] "PT60M" "PT60M" "PT60M" "PT60M" ...
#>  $ ts_time_interval_start               : POSIXct[1:2016], format: "2019-01-31 23:00:00" "2019-01-31 23:00:00" ...
#>  $ ts_time_interval_end                 : POSIXct[1:2016], format: "2019-02-28 23:00:00" "2019-02-28 23:00:00" ...
#>  $ ts_mrid                              : num [1:2016] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start                    : POSIXct[1:2016], format: "2019-01-31 23:00:00" "2019-02-01 00:00:00" ...
#>  $ ts_point_quantity                    : num [1:2016] 123 0 0 0 0 0 0 0 0 0 ...
#>  $ ts_quantity_measure_unit_name        : chr [1:2016] "MAW" "MAW" "MAW" "MAW" ...
```
