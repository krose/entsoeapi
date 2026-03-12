# Get Installed Generation Capacity per Production Type (14.1.A)

The sum of installed net generation capacity (MW) for all existing
production units equal to or exceeding 1 MW installed generation
capacity, per production type.

## Usage

``` r
gen_installed_capacity_per_pt(
  eic = NULL,
  psr_type = NULL,
  year = lubridate::year(Sys.Date()),
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the control area, bidding zone or
  country

- psr_type:

  Defaults to NULL, otherwise list of generation type codes from
  asset_types table

- year:

  YYYY format

- security_token:

  Security token

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with the queried data, or `NULL` if no data is available for the given
parameters.

## Examples

``` r
df <- entsoeapi::gen_installed_capacity_per_pt(
  eic = "10YFR-RTE------C",
  psr_type = "B05",
  year = 2020
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A68&processType=A33&in_Domain=10YFR-RTE------C&psrType=B05&periodStart=202001010000&periodEnd=202101010000&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 12 Mar 2026 09:22:50 GMT
#> <- content-type: text/xml
#> <- content-length: 1692
#> <- content-disposition: inline; filename="Installed Generation Capacity Aggregated_202001010000-202101010000.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived

dplyr::glimpse(df)
#> Rows: 1
#> Columns: 23
#> $ ts_in_bidding_zone_domain_mrid  <chr> "10YFR-RTE------C"
#> $ ts_in_bidding_zone_domain_name  <chr> "France"
#> $ type                            <chr> "A68"
#> $ type_def                        <chr> "Installed generation per type"
#> $ process_type                    <chr> "A33"
#> $ process_type_def                <chr> "Year ahead"
#> $ ts_object_aggregation           <chr> "A08"
#> $ ts_object_aggregation_def       <chr> "Resource type"
#> $ ts_business_type                <chr> "A37"
#> $ ts_business_type_def            <chr> "Installed generation"
#> $ ts_mkt_psr_type                 <chr> "B05"
#> $ ts_mkt_psr_type_def             <chr> "Fossil Hard coal"
#> $ created_date_time               <dttm> 2026-03-12 09:22:50
#> $ revision_number                 <dbl> 1
#> $ time_period_time_interval_start <dttm> 2019-12-31 23:00:00
#> $ time_period_time_interval_end   <dttm> 2020-12-31 23:00:00
#> $ ts_resolution                   <chr> "P1Y"
#> $ ts_time_interval_start          <dttm> 2019-12-31 23:00:00
#> $ ts_time_interval_end            <dttm> 2020-12-31 23:00:00
#> $ ts_mrid                         <dbl> 1
#> $ ts_point_dt_start               <dttm> 2019-12-31 23:00:00
#> $ ts_point_quantity               <dbl> 1812
#> $ ts_quantity_measure_unit_name   <chr> "MAW"
```
