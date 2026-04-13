# Get Aggregated Generation per Generation Unit (16.1.A)

Actual net generation output (MW) and optionally consumption data from
all generation units. Data are aggregated as an average of generation
outputs or consumption.

## Usage

``` r
gen_per_gen_unit(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
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

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with the queried data.

## See also

Other generation endpoints:
[`gen_day_ahead_forecast()`](https://krose.github.io/entsoeapi/reference/gen_day_ahead_forecast.md),
[`gen_installed_capacity_per_pt()`](https://krose.github.io/entsoeapi/reference/gen_installed_capacity_per_pt.md),
[`gen_installed_capacity_per_pu()`](https://krose.github.io/entsoeapi/reference/gen_installed_capacity_per_pu.md),
[`gen_per_prod_type()`](https://krose.github.io/entsoeapi/reference/gen_per_prod_type.md),
[`gen_storage_mean_filling_rate()`](https://krose.github.io/entsoeapi/reference/gen_storage_mean_filling_rate.md),
[`gen_wind_solar_forecasts()`](https://krose.github.io/entsoeapi/reference/gen_wind_solar_forecasts.md)

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
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B04&periodStart=202001302300&periodEnd=202001312300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:51:35 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202001302300-202001312300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling resource_object_eic table from cache
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B05&periodStart=202001302300&periodEnd=202001312300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:51:36 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202001302300-202001312300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling resource_object_eic table from cache
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B04&periodStart=202001312300&periodEnd=202002012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:51:36 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202001312300-202002012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling resource_object_eic table from cache
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B05&periodStart=202001312300&periodEnd=202002012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:51:37 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202001312300-202002012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling resource_object_eic table from cache
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B04&periodStart=202002012300&periodEnd=202002022300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:51:37 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202002012300-202002022300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling resource_object_eic table from cache
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B05&periodStart=202002012300&periodEnd=202002022300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:51:38 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202002012300-202002022300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling resource_object_eic table from cache
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B04&periodStart=202002022300&periodEnd=202002032300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:51:39 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202002022300-202002032300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling resource_object_eic table from cache
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B05&periodStart=202002022300&periodEnd=202002032300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:51:39 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202002022300-202002032300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling resource_object_eic table from cache
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B04&periodStart=202002032300&periodEnd=202002042300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:51:40 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202002032300-202002042300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling resource_object_eic table from cache
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B05&periodStart=202002032300&periodEnd=202002042300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:51:40 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202002032300-202002042300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling resource_object_eic table from cache
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B04&periodStart=202002042300&periodEnd=202002052300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:51:40 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202002042300-202002052300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling resource_object_eic table from cache
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?In_Domain=10YDE-VE-------2&documentType=A73&processType=A16&psrType=B05&periodStart=202002042300&periodEnd=202002052300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:51:41 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Generation Output per Generation Unit_202002042300-202002052300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling resource_object_eic table from cache
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!

dplyr::glimpse(df)
#> Rows: 2,039
#> Columns: 27
#> $ ts_in_bidding_zone_domain_mrid  <chr> "10YDE-VE-------2", "10YDE-VE-------2", "10YDE-VE-------2", "10YDE-VE-------2"…
#> $ ts_in_bidding_zone_domain_name  <chr> "Germany 50Hertz", "Germany 50Hertz", "Germany 50Hertz", "Germany 50Hertz", "G…
#> $ ts_mkt_psr_type_psr_mrid        <chr> "11W0-0000-0103-9", "11W0-0000-0103-9", "11W0-0000-0103-9", "11W0-0000-0103-9"…
#> $ ts_mkt_psr_type_psr_name        <chr> "Lichterfelde GUD", "Lichterfelde GUD", "Lichterfelde GUD", "Lichterfelde GUD"…
#> $ ts_registered_resource_mrid     <chr> "11WD8LICH5G----1", "11WD8LICH5G----1", "11WD8LICH5G----1", "11WD8LICH5G----1"…
#> $ ts_registered_resource_name     <chr> "Lichterfelde", "Lichterfelde", "Lichterfelde", "Lichterfelde", "Lichterfelde"…
#> $ type                            <chr> "A73", "A73", "A73", "A73", "A73", "A73", "A73", "A73", "A73", "A73", "A73", "…
#> $ type_def                        <chr> "Actual generation", "Actual generation", "Actual generation", "Actual generat…
#> $ process_type                    <chr> "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "…
#> $ process_type_def                <chr> "Realised", "Realised", "Realised", "Realised", "Realised", "Realised", "Reali…
#> $ ts_object_aggregation           <chr> "A06", "A06", "A06", "A06", "A06", "A06", "A06", "A06", "A06", "A06", "A06", "…
#> $ ts_object_aggregation_def       <chr> "Resource Object", "Resource Object", "Resource Object", "Resource Object", "R…
#> $ ts_business_type                <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "…
#> $ ts_business_type_def            <chr> "Production", "Production", "Production", "Production", "Production", "Product…
#> $ ts_mkt_psr_type                 <chr> "B04", "B04", "B04", "B04", "B04", "B04", "B04", "B04", "B04", "B04", "B04", "…
#> $ ts_mkt_psr_type_def             <chr> "Fossil Gas", "Fossil Gas", "Fossil Gas", "Fossil Gas", "Fossil Gas", "Fossil …
#> $ created_date_time               <dttm> 2026-04-13 08:51:35, 2026-04-13 08:51:35, 2026-04-13 08:51:35, 2026-04-13 08:…
#> $ revision_number                 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ time_period_time_interval_start <dttm> 2020-01-30 23:00:00, 2020-01-30 23:00:00, 2020-01-30 23:00:00, 2020-01-30 23:…
#> $ time_period_time_interval_end   <dttm> 2020-01-31 23:00:00, 2020-01-31 23:00:00, 2020-01-31 23:00:00, 2020-01-31 23:…
#> $ ts_resolution                   <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M…
#> $ ts_time_interval_start          <dttm> 2020-01-30 23:00:00, 2020-01-30 23:00:00, 2020-01-30 23:00:00, 2020-01-30 23:…
#> $ ts_time_interval_end            <dttm> 2020-01-31 23:00:00, 2020-01-31 23:00:00, 2020-01-31 23:00:00, 2020-01-31 23:…
#> $ ts_mrid                         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, …
#> $ ts_point_dt_start               <dttm> 2020-01-30 23:00:00, 2020-01-31 00:00:00, 2020-01-31 01:00:00, 2020-01-31 02:…
#> $ ts_point_quantity               <dbl> 200.750, 201.000, 200.500, 201.000, 200.750, 201.000, 200.500, 201.000, 200.75…
#> $ ts_quantity_measure_unit_name   <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "…
```
