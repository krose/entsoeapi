# Get Installed Generation Capacity per Production Unit (14.1.B)

The installed generation capacities (MW) at the beginning of the year
for all the production units, including the planned ones.

## Usage

``` r
gen_installed_capacity_per_pu(
  eic = NULL,
  year = year(Sys.Date()),
  psr_type = NULL,
  tidy_output = TRUE,
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

- tidy_output:

  Defaults to TRUE. If TRUE, then flatten nested tables.

- security_token:

  Security token

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with the queried data.

## See also

Other generation endpoints:
[`gen_day_ahead_forecast()`](https://krose.github.io/entsoeapi/reference/gen_day_ahead_forecast.md),
[`gen_installed_capacity_per_pt()`](https://krose.github.io/entsoeapi/reference/gen_installed_capacity_per_pt.md),
[`gen_per_gen_unit()`](https://krose.github.io/entsoeapi/reference/gen_per_gen_unit.md),
[`gen_per_prod_type()`](https://krose.github.io/entsoeapi/reference/gen_per_prod_type.md),
[`gen_storage_mean_filling_rate()`](https://krose.github.io/entsoeapi/reference/gen_storage_mean_filling_rate.md),
[`gen_wind_solar_forecasts()`](https://krose.github.io/entsoeapi/reference/gen_wind_solar_forecasts.md)

## Examples

``` r
df <- entsoeapi::gen_installed_capacity_per_pu(
  eic = "10YDE-VE-------2",
  year = 2020,
  psr_type = "B05"
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A71&processType=A33&in_Domain=10YDE-VE-------2&psrType=B05&periodStart=202001010000&periodEnd=202101010000&securityToken=<...>
#> <- HTTP/1.1 200 OK
#> <- Date: Fri, 16 Oct 2026 07:43:29 GMT
#> <- Content-Type: text/xml
#> <- Transfer-Encoding: chunked
#> <- Connection: keep-alive
#> <- Content-Disposition: inline; filename="EXPORT_PRODUCTION_AND_GENERATION_UNITS_202011100 - 202111100.xml"
#> <- Content-Encoding: gzip
#> <- Strict-Transport-Security: max-age=15724800
#> <- Vary: accept-encoding
#> <- X-Content-Type-Options: nosniff
#> <- X-Xss-Protection: 0
#> <- 
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading resource_object_eic table ...
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling W_eicCodes.csv file from cache
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!

dplyr::glimpse(df)
#> Rows: 20
#> Columns: 26
#> $ ts_in_bidding_zone_domain_mrid  <chr> "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A82H"…
#> $ ts_in_bidding_zone_domain_name  <chr> "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Luxemb…
#> $ ts_mkt_psr_type_psr_mrid        <chr> "11WD8ROST1C----K", "11WD8ROST1C----K", "11WD8WED15C---1M", "11WD8WED15C---2K"…
#> $ ts_mkt_psr_type_psr_name        <chr> "Kraftwerk Rostock", "Kraftwerk Rostock", "HKW Wedel Block 1", "HKW Wedel Bloc…
#> $ ts_registered_resource_mrid     <chr> "11WD8ROST1C---P5", "11WD8ROST1C---P5", "11WD8WED15C----Q", "11WD8WED15C----Q"…
#> $ ts_registered_resource_name     <chr> "Rostock Kohlekraftwerk Produktionsanlage", "Rostock Kohlekraftwerk Produktion…
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
#> $ created_date_time               <dttm> 2026-10-16 07:43:29, 2026-10-16 07:43:29, 2026-10-16 07:43:29, 2026-10-16 07:…
#> $ time_period_time_interval_start <dttm> 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019-12-31 23:…
#> $ time_period_time_interval_end   <dttm> 2021-12-31 23:00:00, 2021-12-31 23:00:00, 2021-12-31 23:00:00, 2021-12-31 23:…
#> $ ts_resolution                   <chr> "P1Y", "P1Y", "P1Y", "P1Y", "P1Y", "P1Y", "P1Y", "P1Y", "P1Y", "P1Y", "P1Y", "…
#> $ ts_time_interval_start          <dttm> 2020-12-31 23:00:00, 2019-12-31 23:00:00, 2020-12-31 23:00:00, 2020-12-31 23:…
#> $ ts_time_interval_end            <dttm> 2021-12-31 23:00:00, 2020-12-31 23:00:00, 2021-12-31 23:00:00, 2021-12-31 23:…
#> $ ts_mrid                         <chr> "148dc1d550e244a2", "16d90a568b5942cc", "1d56188c7a744cc7", "1d56188c7a744cc7"…
#> $ ts_point_dt_start               <dttm> 2020-12-31 23:00:00, 2019-12-31 23:00:00, 2020-12-31 23:00:00, 2020-12-31 23:…
#> $ ts_point_quantity               <dbl> 514, 514, 250, 250, 140, 140, 1600, 1600, 316, 316, 1600, 1600, 316, 316, 564,…
#> $ ts_quantity_measure_unit_name   <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "…
```
