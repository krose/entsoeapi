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
#> <- date: Tue, 10 Mar 2026 17:31:53 GMT
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

dplyr::glimpse(df)
#> Rows: 13
#> Columns: 26
#> $ ts_in_bidding_zone_domain_mrid  <chr> "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A82H"…
#> $ ts_in_bidding_zone_domain_name  <chr> "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Luxemb…
#> $ ts_mkt_psr_type_psr_mrid        <chr> "11WD8REUW1C---DD", "11WD8REUW1C---EB", "11WD8ROST1C----K", "11WD8TIEF5C----9"…
#> $ ts_mkt_psr_type_psr_name        <chr> "HKW Reuter West Block D", "HKW Reuter West Block E", "Kraftwerk Rostock", "HK…
#> $ ts_registered_resource_mrid     <chr> "11WD8REUW1C----4", "11WD8REUW1C----4", "11WD8ROST1C---P5", "11WD8TIEF5X----V"…
#> $ ts_registered_resource_name     <chr> "Reuter West", "Reuter West", "Rostock Kohlekraftwerk Produktionsanlage", "Tie…
#> $ type                            <chr> "A71", "A71", "A71", "A71", "A71", "A71", "A71", "A71", "A71", "A71", "A71", "…
#> $ type_def                        <chr> "Generation forecast", "Generation forecast", "Generation forecast", "Generati…
#> $ process_type                    <chr> "A33", "A33", "A33", "A33", "A33", "A33", "A33", "A33", "A33", "A33", "A33", "…
#> $ process_type_def                <chr> "Year ahead", "Year ahead", "Year ahead", "Year ahead", "Year ahead", "Year ah…
#> $ ts_object_aggregation           <chr> "A06", "A06", "A06", "A06", "A06", "A06", "A06", "A06", "A06", "A06", "A06", "…
#> $ ts_object_aggregation_def       <chr> "Resource Object", "Resource Object", "Resource Object", "Resource Object", "R…
#> $ ts_business_type                <chr> "B11", "B11", "B11", "B11", "B11", "B11", "B11", "B11", "B11", "B11", "B11", "…
#> $ ts_business_type_def            <chr> "Production unit", "Production unit", "Production unit", "Production unit", "P…
#> $ ts_mkt_psr_type                 <chr> "B05", "B05", "B05", "B05", "B05", "B05", "B05", "B05", "B05", "B05", "B05", "…
#> $ ts_mkt_psr_type_def             <chr> "Fossil Hard coal", "Fossil Hard coal", "Fossil Hard coal", "Fossil Hard coal"…
#> $ created_date_time               <dttm> 2026-03-10 17:31:53, 2026-03-10 17:31:53, 2026-03-10 17:31:53, 2026-03-10 17:…
#> $ time_period_time_interval_start <dttm> 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019-12-31 23:…
#> $ time_period_time_interval_end   <dttm> 2021-01-01, 2021-01-01, 2021-01-01, 2021-01-01, 2021-01-01, 2021-01-01, 2021-…
#> $ ts_resolution                   <chr> "P1Y", "P1Y", "P1Y", "P1Y", "P1Y", "P1Y", "P1Y", "P1Y", "P1Y", "P1Y", "P1Y", "…
#> $ ts_time_interval_start          <dttm> 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019-12-31 23:…
#> $ ts_time_interval_end            <dttm> 2020-12-31 23:00:00, 2020-12-31 23:00:00, 2020-12-31 23:00:00, 2020-12-31 23:…
#> $ ts_mrid                         <chr> "18a4cd9d58b743b0", "18a4cd9d58b743b0", "2c40f661522748a8", "3d66521025404a32"…
#> $ ts_point_dt_start               <dttm> 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019-12-31 23:…
#> $ ts_point_quantity               <dbl> 564, 564, 514, 316, 316, 140, 140, 140, 140, 1600, 1600, 250, 250
#> $ ts_quantity_measure_unit_name   <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "…
```
