---
title: "README - entsoeapi"
---

<!-- badges: start -->
![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)
![Windows](https://img.shields.io/badge/Windows-0078D6?style=for-the-badge&logo=windows&logoColor=white)
![macOS](https://img.shields.io/badge/mac%20os-000000?style=for-the-badge&logo=macos&logoColor=F0F0F0)
![Linux](https://img.shields.io/badge/Linux-FCC624?style=for-the-badge&logo=linux&logoColor=black)
<!-- badges: end -->

The goal of `entsoeapi` package is to create an easy wrapper for querying the
ENTSO-E [API](https://transparency.entsoe.eu/content/static_content/Static%20content/web%20api/Guide.html)'s
Load, Generation, Transmission, Balancing & Outages data which are available on the ENTSO-E [transparency
platform](https://transparency.entsoe.eu/) website as well.    

-   Already available ENTSO-E API endpoints:
    -   BALANCING
        -   balancing_accepted_aggr_offers (17.1.D)
        -   balancing_activated_reserves (17.1.E)
    -   GENERATION
        -   gen_day_ahead (14.1.C)
        -   gen_installed_capacity_per_pt (14.1.A)
        -   the gen_installed_capacity_per_pu (14.1.B)
        -   gen_per_gen_unit (16.1.A)
        -   gen_per_prod_type (16.1.B&C)
        -   gen_wind_solar_forecasts (14.1.D)
        -   the gen_storage_mean_filling_rate (16.1.D)
    -   LOAD
        -   load_actual_total (6.1.A)
        -   load_day_ahead_total_forecast (6.1.B)
        -   load_week_ahead_total_forecast (6.1.C)
        -   load_month_ahead_total_forecast (6.1.D)
        -   load_year_ahead_total_forecast (6.1.E)
        -   load_year_ahead_forecast_margin (8.1)
    -   UNAVAILABILITY
        -   outages_gen_units (15.1.A&B)
        -   outages_prod_units (15.1.C&D)
        -   outages_both (15.1.A&B + 15.1.C&D)
        -   outages_cons_units (7.1.A&B)
        -   outages_fallbacks (IFs IN 7.2, mFRR 3.11, aFRR 3.10)
        -   outages_offshore_grid (10.1.A&B)
        -   outages_transmission_grid (10.1.A&B)
    -   TRANSMISSION
        -   transm_already_allocated_cap (12.1.C)
        -   transm_day_ahead_comm_sched (12.1.F)
        -   transm_day_ahead_prices (12.1.D)
        -   transm_day_ahead_transf_cap (11.1)
        -   transm_total_comm_sched (12.1.F)
        -   transm_total_nominated_cap (12.1.B)
        -   transm_x_border_phys_flow (12.1.G)

All the function calls convert the xml responses to tabular data. Be aware, that not all endpoints are implemented.
If you want to use an unimplemented endpoint, please submit an [issue](https://github.com/krose/entsoeapi/issues) 
and we'll do our best to resolve it.    
<b>IMPORTANT!</b>    
Since the underlying engine has fairly been standardized with the introduction of version 0.7.0.0, 
there are significant (breaking) changes between the 0.7.0.0 and the previous versions.

## Installation

You can install the development version from [GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("krose/entsoeapi")
```

## Security token

Read
[here](https://transparency.entsoe.eu/content/static_content/Static%20content/web%20api/Guide.html#_authentication_and_authorisation)
how to get a security token. You should also create a `.Renviron` file in your
working directory with a security token and call it ENTSOE_PAT.

```         
ENTSOE_PAT = "your_security_token"
```

## Example

You use the eic codes to get the data. Let’s try to find the eic code for
Germany.

``` r
library(dplyr)
library(lubridate)
library(entsoeapi)

en_eic() |>
  filter(AreaTypeCode == "CTY", 
         AreaName == "Germany") |>
  glimpse()
#> Observations: 1
#> Variables: 4
#> $ AreaCode     <chr> "10Y1001A1001A83F"
#> $ AreaTypeCode <chr> "CTY"
#> $ AreaName     <chr> "Germany"
#> $ MapCode      <chr> "DE"
```

For some of the data you need to translate the generation codes.

``` r
en_generation_codes() |>
  glimpse()
#> Rows: 27
#> Columns: 4
#> $ codes      <chr> "A03", "A04", "A05", "B01", "B02", "B03", "B04", "B...
#> $ meaning    <chr> "Mixed", "Generation", "Load", "Biomass", "Fossil B...
#> $ co2_g_kwh  <dbl> NA, NA, NA, 390, 360, NA, 200, 340, 260, NA, 380, N...
#> $ efficiency <dbl> NA, NA, NA, NA, 0.35, NA, 0.50, 0.38, NA, NA, NA, N...
```

Let’s get the demand in Germany.

``` r
en_load_actual_total(eic = "10Y1001A1001A83F",
                     period_start = ymd("2020-01-01", tz = "CET"),
                     period_end = ymd("2020-01-02", tz = "CET")) |>
  glimpse()
#> Rows: 1,632
#> Columns: 25
#> $ ts_in_bidding_zone_domain_mrid  <chr> "10Y1001A1001A83F", "10Y1001A1001A8…
#> $ ts_in_bidding_zone_domain_name  <chr> "Germany", "Germany", "Germany", "G…
#> $ ts_out_bidding_zone_domain_mrid <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ ts_out_bidding_zone_domain_name <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ type                            <chr> "A75", "A75", "A75", "A75", "A75", …
#> $ type_def                        <chr> "Actual generation per type", "Actu…
#> $ process_type                    <chr> "A16", "A16", "A16", "A16", "A16", …
#> $ process_type_def                <chr> "Realised", "Realised", "Realised",…
#> $ ts_object_aggregation           <chr> "A08", "A08", "A08", "A08", "A08", …
#> $ ts_object_aggregation_def       <chr> "Resource type", "Resource type", "…
#> $ ts_business_type                <chr> "A01", "A01", "A01", "A01", "A01", …
#> $ ts_business_type_def            <chr> "Production", "Production", "Produc…
#> $ ts_mkt_psr_type                 <chr> "B01", "B01", "B01", "B01", "B01", …
#> $ ts_mkt_psr_type_def             <chr> "Biomass", "Biomass", "Biomass", "B…
#> $ created_date_time               <dttm> 2024-07-16 10:16:58, 2024-07-16 10…
#> $ revision_number                 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ time_period_time_interval_start <dttm> 2019-12-31 23:00:00, 2019-12-31 23…
#> $ time_period_time_interval_end   <dttm> 2020-01-01 23:00:00, 2020-01-01 23…
#> $ ts_resolution                   <chr> "PT15M", "PT15M", "PT15M", "PT15M",…
#> $ ts_time_interval_start          <dttm> 2019-12-31 23:00:00, 2019-12-31 23…
#> $ ts_time_interval_end            <dttm> 2020-01-01 23:00:00, 2020-01-01 23…
#> $ ts_mrid                         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start               <dttm> 2019-12-31 23:00:00, 2019-12-31 23…
#> $ ts_point_quantity               <dbl> 4918, 4913, 4899, 4897, 4896, 4877,…
#> $ ts_quantity_measure_unit_name   <chr> "MAW", "MAW", "MAW", "MAW", "MAW", …
```

This is basically how all the functions work, so let’s try to get the production
data too.

``` r
en_generation_agg_gen_per_type(eic = "10Y1001A1001A83F", 
                               period_start = ymd("2020-01-01", tz = "CET"),
                               period_end = ymd("2020-01-02", tz = "CET"),
                               gen_type = NULL,
                               tidy_output = TRUE) |>
  glimpse()
#> Rows: 1,564
#> Columns: 25
#> $ ts_in_bidding_zone_domain_mrid  <chr> "10Y1001A1001A83F", "10Y1001A1001A8…
#> $ ts_in_bidding_zone_domain_name  <chr> "Germany", "Germany", "Germany", "G…
#> $ ts_out_bidding_zone_domain_mrid <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ ts_out_bidding_zone_domain_name <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ type                            <chr> "A75", "A75", "A75", "A75", "A75", …
#> $ type_def                        <chr> "Actual generation per type", "Actu…
#> $ process_type                    <chr> "A16", "A16", "A16", "A16", "A16", …
#> $ process_type_def                <chr> "Realised", "Realised", "Realised",…
#> $ ts_object_aggregation           <chr> "A08", "A08", "A08", "A08", "A08", …
#> $ ts_object_aggregation_def       <chr> "Resource type", "Resource type", "…
#> $ ts_business_type                <chr> "A01", "A01", "A01", "A01", "A01", …
#> $ ts_business_type_def            <chr> "Production", "Production", "Produc…
#> $ ts_mkt_psr_type                 <chr> "B01", "B01", "B01", "B01", "B01", …
#> $ ts_mkt_psr_type_def             <chr> "Biomass", "Biomass", "Biomass", "B…
#> $ created_date_time               <dttm> 2024-07-16 10:16:58, 2024-07-16 10…
#> $ revision_number                 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ time_period_time_interval_start <dttm> 2019-12-31 23:00:00, 2019-12-31 23…
#> $ time_period_time_interval_end   <dttm> 2020-01-01 23:00:00, 2020-01-01 23…
#> $ ts_resolution                   <chr> "PT15M", "PT15M", "PT15M", "PT15M",…
#> $ ts_time_interval_start          <dttm> 2019-12-31 23:00:00, 2019-12-31 23…
#> $ ts_time_interval_end            <dttm> 2020-01-01 23:00:00, 2020-01-01 23…
#> $ ts_mrid                         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start               <dttm> 2019-12-31 23:00:00, 2019-12-31 23…
#> $ ts_point_quantity               <dbl> 4918, 4913, 4899, 4897, 4896, 4877,…
#> $ ts_quantity_measure_unit_name   <chr> "MAW", "MAW", "MAW", "MAW", "MAW", …
```
