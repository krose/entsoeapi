# Get Installed Generation Capacity per Production Unit (14.1.B)

The installed generation capacities (MW) at the beginning of the year
for all the production units, including the planned ones.

## Usage

``` r
gen_installed_capacity_per_pu(
  eic = NULL,
  year = lubridate::year(Sys.Date()),
  psr_type = NULL,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the control area, bidding zone or
  country

- year:

  YYYY format Cannot be shown more than 3 years ahead as required by the
  law.

- psr_type:

  Defaults to NULL, otherwise list of generation type codes from
  asset_types table

- security_token:

  Security token

## Examples

``` r
df <- entsoeapi::gen_installed_capacity_per_pu(
  eic      = "10YDE-VE-------2",
  year     = 2020,
  psr_type = "B05"
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A71&processType=A33&in_Domain=10YDE-VE-------2&psrType=B05&periodStart=202001010000&periodEnd=202101010000&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 05 Mar 2026 16:12:35 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="EXPORT_PRODUCTION_AND_GENERATION_UNITS_202011100 - 202111100.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ pulling W_eicCodes.csv file from cache
str(df)
#> tibble [13 × 26] (S3: tbl_df/tbl/data.frame)
#>  $ ts_in_bidding_zone_domain_mrid : chr [1:13] "10Y1001A1001A82H" "10Y1001A1001A82H" "10Y1001A1001A82H" "10Y1001A1001A82H" ...
#>  $ ts_in_bidding_zone_domain_name : chr [1:13] "Germany_Luxemburg" "Germany_Luxemburg" "Germany_Luxemburg" "Germany_Luxemburg" ...
#>  $ ts_mkt_psr_type_psr_mrid       : chr [1:13] "11WD8MOOR1C---AV" "11WD8MOOR1C---BT" "11WD8ROST1C----K" "11WD8WED15C---1M" ...
#>  $ ts_mkt_psr_type_psr_name       : chr [1:13] "HKW Moorburg Block A" "HKW Moorburg Block B" "Kraftwerk Rostock" "HKW Wedel Block 1" ...
#>  $ ts_registered_resource_mrid    : chr [1:13] "11WD8MOOR1C----G" "11WD8MOOR1C----G" "11WD8ROST1C---P5" "11WD8WED15C----Q" ...
#>  $ ts_registered_resource_name    : chr [1:13] "Moorburg" "Moorburg" "Rostock Kohlekraftwerk Produktionsanlage" "Wedel HKW" ...
#>  $ type                           : chr [1:13] "A71" "A71" "A71" "A71" ...
#>  $ type_def                       : chr [1:13] "Generation forecast" "Generation forecast" "Generation forecast" "Generation forecast" ...
#>  $ process_type                   : chr [1:13] "A33" "A33" "A33" "A33" ...
#>  $ process_type_def               : chr [1:13] "Year ahead" "Year ahead" "Year ahead" "Year ahead" ...
#>  $ ts_object_aggregation          : chr [1:13] "A06" "A06" "A06" "A06" ...
#>  $ ts_object_aggregation_def      : chr [1:13] "Resource Object" "Resource Object" "Resource Object" "Resource Object" ...
#>  $ ts_business_type               : chr [1:13] "B11" "B11" "B11" "B11" ...
#>  $ ts_business_type_def           : chr [1:13] "Production unit" "Production unit" "Production unit" "Production unit" ...
#>  $ ts_mkt_psr_type                : chr [1:13] "B05" "B05" "B05" "B05" ...
#>  $ ts_mkt_psr_type_def            : chr [1:13] "Fossil Hard coal" "Fossil Hard coal" "Fossil Hard coal" "Fossil Hard coal" ...
#>  $ created_date_time              : POSIXct[1:13], format: "2026-03-05 16:12:35" "2026-03-05 16:12:35" "2026-03-05 16:12:35" "2026-03-05 16:12:35" ...
#>  $ time_period_time_interval_start: POSIXct[1:13], format: "2019-12-31 23:00:00" "2019-12-31 23:00:00" "2019-12-31 23:00:00" "2019-12-31 23:00:00" ...
#>  $ time_period_time_interval_end  : POSIXct[1:13], format: "2021-01-01" "2021-01-01" "2021-01-01" "2021-01-01" ...
#>  $ ts_resolution                  : chr [1:13] "P1Y" "P1Y" "P1Y" "P1Y" ...
#>  $ ts_time_interval_start         : POSIXct[1:13], format: "2019-12-31 23:00:00" "2019-12-31 23:00:00" "2019-12-31 23:00:00" "2019-12-31 23:00:00" ...
#>  $ ts_time_interval_end           : POSIXct[1:13], format: "2020-12-31 23:00:00" "2020-12-31 23:00:00" "2020-12-31 23:00:00" "2020-12-31 23:00:00" ...
#>  $ ts_mrid                        : chr [1:13] "137c7867945b4bd1" "137c7867945b4bd1" "6d3cfc47c783460c" "6d6b5bed18d94657" ...
#>  $ ts_point_dt_start              : POSIXct[1:13], format: "2019-12-31 23:00:00" "2019-12-31 23:00:00" "2019-12-31 23:00:00" "2019-12-31 23:00:00" ...
#>  $ ts_point_quantity              : num [1:13] 1600 1600 514 250 250 316 316 564 564 140 ...
#>  $ ts_quantity_measure_unit_name  : chr [1:13] "MAW" "MAW" "MAW" "MAW" ...
```
