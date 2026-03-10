# Get Aggregated Generation per Production Type (16.1.B&C)

Actual aggregated net generation output (MW) or consumption per market
time unit and per production type.

## Usage

``` r
gen_per_prod_type(
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

  Energy Identification Code of the control area, bidding zone or
  country

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- gen_type:

  Defaults to NULL, otherwise list of generation type codes from
  asset_types table

- tidy_output:

  Defaults to TRUE. If TRUE, then flatten nested tables.

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
df <- entsoeapi::gen_per_prod_type(
  eic          = "10YFR-RTE------C",
  period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
  period_end   = lubridate::ymd(x = "2020-03-01", tz = "CET"),
  gen_type     = NULL,
  tidy_output  = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A75&processType=A16&in_Domain=10YFR-RTE------C&periodStart=202001312300&periodEnd=202002292300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 10 Mar 2026 19:48:44 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Aggregated Generation per Type_202001312300-202002292300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived

dplyr::glimpse(df)
#> Rows: 7,656
#> Columns: 25
#> $ ts_in_bidding_zone_domain_mrid  <chr> "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C", "10YFR-RTE------C"…
#> $ ts_in_bidding_zone_domain_name  <chr> "France", "France", "France", "France", "France", "France", "France", "France"…
#> $ ts_out_bidding_zone_domain_mrid <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ ts_out_bidding_zone_domain_name <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ type                            <chr> "A75", "A75", "A75", "A75", "A75", "A75", "A75", "A75", "A75", "A75", "A75", "…
#> $ type_def                        <chr> "Actual generation per type", "Actual generation per type", "Actual generation…
#> $ process_type                    <chr> "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "…
#> $ process_type_def                <chr> "Realised", "Realised", "Realised", "Realised", "Realised", "Realised", "Reali…
#> $ ts_object_aggregation           <chr> "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "…
#> $ ts_object_aggregation_def       <chr> "Resource type", "Resource type", "Resource type", "Resource type", "Resource …
#> $ ts_business_type                <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "…
#> $ ts_business_type_def            <chr> "Production", "Production", "Production", "Production", "Production", "Product…
#> $ ts_mkt_psr_type                 <chr> "B01", "B01", "B01", "B01", "B01", "B01", "B01", "B01", "B01", "B01", "B01", "…
#> $ ts_mkt_psr_type_def             <chr> "Biomass", "Biomass", "Biomass", "Biomass", "Biomass", "Biomass", "Biomass", "…
#> $ created_date_time               <dttm> 2026-03-10 19:48:44, 2026-03-10 19:48:44, 2026-03-10 19:48:44, 2026-03-10 19:…
#> $ revision_number                 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ time_period_time_interval_start <dttm> 2020-01-31 23:00:00, 2020-01-31 23:00:00, 2020-01-31 23:00:00, 2020-01-31 23:…
#> $ time_period_time_interval_end   <dttm> 2020-02-29 23:00:00, 2020-02-29 23:00:00, 2020-02-29 23:00:00, 2020-02-29 23:…
#> $ ts_resolution                   <chr> "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M", "PT60M…
#> $ ts_time_interval_start          <dttm> 2020-01-31 23:00:00, 2020-01-31 23:00:00, 2020-01-31 23:00:00, 2020-01-31 23:…
#> $ ts_time_interval_end            <dttm> 2020-02-29 23:00:00, 2020-02-29 23:00:00, 2020-02-29 23:00:00, 2020-02-29 23:…
#> $ ts_mrid                         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_point_dt_start               <dttm> 2020-01-31 23:00:00, 2020-02-01 00:00:00, 2020-02-01 01:00:00, 2020-02-01 02:…
#> $ ts_point_quantity               <dbl> 335, 334, 335, 335, 334, 332, 335, 334, 334, 333, 332, 332, 333, 334, 334, 333…
#> $ ts_quantity_measure_unit_name   <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "…
```
