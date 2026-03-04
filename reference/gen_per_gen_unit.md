# Get Aggregated Generation per Generation Unit (16.1.A)

Actual net generation output (MW) and optionally consumption data from
all generation units. Data are aggregated as an average of generation
outputs or consumption.

## Usage

``` r
gen_per_gen_unit(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  gen_type = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the control area or bidding zone

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format

- gen_type:

  Defaults to NULL, otherwise list of generation type codes from
  asset_types table

- tidy_output:

  Defaults to TRUE. If TRUE, then flatten nested tables.

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df <- entsoeapi::gen_per_gen_unit(
  eic          = "10YDE-VE-------2",
  period_start = lubridate::ymd(x = "2020-01-31", tz = "CET"),
  period_end   = lubridate::ymd(x = "2020-02-06", tz = "CET"),
  gen_type     = c("B04", "B05"),
  tidy_output  = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B04&periodStart=202001302300&periodEnd=202001312300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:13:26 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202001302300-202001312300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B05&periodStart=202001302300&periodEnd=202001312300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:13:26 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202001302300-202001312300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B04&periodStart=202001312300&periodEnd=202002012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:13:27 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202001312300-202002012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B05&periodStart=202001312300&periodEnd=202002012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:13:27 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202001312300-202002012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B04&periodStart=202002012300&periodEnd=202002022300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:13:28 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202002012300-202002022300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B05&periodStart=202002012300&periodEnd=202002022300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:13:28 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202002012300-202002022300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B04&periodStart=202002022300&periodEnd=202002032300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:13:29 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202002022300-202002032300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B05&periodStart=202002022300&periodEnd=202002032300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:13:29 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202002022300-202002032300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B04&periodStart=202002032300&periodEnd=202002042300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:13:30 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202002032300-202002042300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B05&periodStart=202002032300&periodEnd=202002042300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:13:30 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202002032300-202002042300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B04&periodStart=202002042300&periodEnd=202002052300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:13:31 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202002042300-202002052300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B05&periodStart=202002042300&periodEnd=202002052300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:13:31 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202002042300-202002052300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
str(df)
#> tibble [2,039 × 27] (S3: tbl_df/tbl/data.frame)
#>  $ ts_in_bidding_zone_domain_mrid : chr [1:2039] "10YDE-VE-------2" "10YDE-VE-------2" "10YDE-VE-------2" "10YDE-VE-------2" ...
#>  $ ts_in_bidding_zone_domain_name : chr [1:2039] "Germany 50Hertz" "Germany 50Hertz" "Germany 50Hertz" "Germany 50Hertz" ...
#>  $ ts_mkt_psr_type_psr_mrid       : chr [1:2039] "11W0-0000-0103-9" "11W0-0000-0103-9" "11W0-0000-0103-9" "11W0-0000-0103-9" ...
#>  $ ts_mkt_psr_type_psr_name       : chr [1:2039] "Lichterfelde GUD" "Lichterfelde GUD" "Lichterfelde GUD" "Lichterfelde GUD" ...
#>  $ ts_registered_resource_mrid    : chr [1:2039] "11WD8LICH5G----1" "11WD8LICH5G----1" "11WD8LICH5G----1" "11WD8LICH5G----1" ...
#>  $ ts_registered_resource_name    : chr [1:2039] "Lichterfelde" "Lichterfelde" "Lichterfelde" "Lichterfelde" ...
#>  $ type                           : chr [1:2039] "A73" "A73" "A73" "A73" ...
#>  $ type_def                       : chr [1:2039] "Actual generation" "Actual generation" "Actual generation" "Actual generation" ...
#>  $ process_type                   : chr [1:2039] "A16" "A16" "A16" "A16" ...
#>  $ process_type_def               : chr [1:2039] "Realised" "Realised" "Realised" "Realised" ...
#>  $ ts_object_aggregation          : chr [1:2039] "A06" "A06" "A06" "A06" ...
#>  $ ts_object_aggregation_def      : chr [1:2039] "Resource Object" "Resource Object" "Resource Object" "Resource Object" ...
#>  $ ts_business_type               : chr [1:2039] "A01" "A01" "A01" "A01" ...
#>  $ ts_business_type_def           : chr [1:2039] "Production" "Production" "Production" "Production" ...
#>  $ ts_mkt_psr_type                : chr [1:2039] "B04" "B04" "B04" "B04" ...
#>  $ ts_mkt_psr_type_def            : chr [1:2039] "Fossil Gas" "Fossil Gas" "Fossil Gas" "Fossil Gas" ...
#>  $ created_date_time              : POSIXct[1:2039], format: "2026-03-04 22:13:26" "2026-03-04 22:13:26" ...
#>  $ revision_number                : num [1:2039] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ time_period_time_interval_start: POSIXct[1:2039], format: "2020-01-30 23:00:00" "2020-01-30 23:00:00" ...
#>  $ time_period_time_interval_end  : POSIXct[1:2039], format: "2020-01-31 23:00:00" "2020-01-31 23:00:00" ...
#>  $ ts_resolution                  : chr [1:2039] "PT60M" "PT60M" "PT60M" "PT60M" ...
#>  $ ts_time_interval_start         : POSIXct[1:2039], format: "2020-01-30 23:00:00" "2020-01-30 23:00:00" ...
#>  $ ts_time_interval_end           : POSIXct[1:2039], format: "2020-01-31 23:00:00" "2020-01-31 23:00:00" ...
#>  $ ts_mrid                        : num [1:2039] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start              : POSIXct[1:2039], format: "2020-01-30 23:00:00" "2020-01-31 00:00:00" ...
#>  $ ts_point_quantity              : num [1:2039] 201 201 200 200 201 ...
#>  $ ts_quantity_measure_unit_name  : chr [1:2039] "MAW" "MAW" "MAW" "MAW" ...
```
