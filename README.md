# README - entsoeapi <img src="man/figures/logo.png" align="right" height="139"/>

<!-- badges: start -->

[![Lifecycle:experimental](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)\
[![lint-project.yml](https://github.com/krose/entsoeapi/actions/workflows/lint-project.yml/badge.svg)](https://github.com/krose/entsoeapi/actions/workflows/lint-project.yml)\
[![R-CMD-check](https://github.com/krose/entsoeapi/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/krose/entsoeapi/actions/workflows/R-CMD-check.yml)\
[![test-coverage.yml](https://github.com/krose/entsoeapi/actions/workflows/test-coverage.yml/badge.svg)](https://github.com/krose/entsoeapi/actions/workflows/test-coverage.yml)

<!-- badges: end -->

The goal of `entsoeapi` package is to create an easy wrapper for querying the ENTSO-E [API](https://documenter.getpostman.com/view/7009892/2s93JtP3F6)'s Market, Load, Generation, Transmission, Outages & Balancing data which are available on the ENTSO-E [transparency platform](https://transparency.entsoe.eu/) website as well.

------------------------------------------------------------------------

-   Already available ENTSO-E API endpoints:
    -   MARKET
        -   implicit_offered_transfer_capacities (11.1) (beta test version)
        -   explicit_offered_transfer_capacities (11.1.A) (beta test version)
        -   continuous_offered_transfer_capacities (11.1) (beta test version)
        -   flow_based_allocations (11.1.B) (beta test version)
        -   auction_revenue (12.1.A) (beta test version)
        -   total_nominated_capacity (12.1.B)
        -   already_allocated_total_capacity (12.1.C)
        -   day_ahead_prices (12.1.D)
        -   net_positions (12.1.E) (beta test version)
        -   congestion_income (12.1.E) (beta test version)
        -   allocated_transfer_capacities_3rd_countries (12.1.H) (beta test version)
    -   LOAD
        -   load_actual_total (6.1.A)
        -   load_day_ahead_total_forecast (6.1.B)
        -   load_week_ahead_total_forecast (6.1.C)
        -   load_month_ahead_total_forecast (6.1.D)
        -   load_year_ahead_total_forecast (6.1.E)
        -   load_year_ahead_forecast_margin (8.1)
    -   GENERATION
        -   gen_installed_capacity_per_pt (14.1.A)
        -   the gen_installed_capacity_per_pu (14.1.B)
        -   gen_day_ahead (14.1.C)
        -   gen_wind_solar_forecasts (14.1.D)
        -   gen_per_gen_unit (16.1.A)
        -   gen_per_prod_type (16.1.B&C)
        -   the gen_storage_mean_filling_rate (16.1.D)
    -   TRANSMISSION
        -   expansion_and_dismantling_project (9.1) (beta test version)
        -   intraday_cross_border_transfer_limits (11.3) (beta test version)
        -   forecasted_transfer_capacities (11.1.A)
        -   day_ahead_commercial_sched (12.1.F)
        -   total_commercial_sched (12.1.F)
        -   cross_border_physical_flows (12.1.G)
        -   redispatching_internal (13.1.A)
        -   redispatching_cross_border (13.1.A)
        -   countertrading (13.1.B)
        -   costs_of_congestion_management (13.1.C)
    -   OUTAGES
        -   outages_cons_units (7.1.A&B)
        -   outages_fallbacks (IFs IN 7.2, mFRR 3.11, aFRR 3.10)
        -   outages_transmission_grid (10.1.A&B)
        -   outages_offshore_grid (10.1.C)
        -   outages_gen_units (15.1.A&B)
        -   outages_prod_units (15.1.C&D)
        -   outages_both (15.1.A&B + 15.1.C&D)
    -   BALANCING
        -   exchanged_volumes (aFRR 3.16, mFRR 3.17)
        -   netted_volumes (IFs IN 3.10)
        -   elastic_demands (IF mFRR 3.4)
        -   balancing_border_cap_limit (IFs 4.3 & 4.4)

All the function calls convert the xml responses to tabular data. Be aware, that not all endpoints are implemented. If you want to use an unimplemented endpoint, please submit an [issue](https://github.com/krose/entsoeapi/issues/new/choose) and we'll do our best to resolve it.\
<b>IMPORTANT!</b>\
Since the underlying engine has fairly been standardized with the introduction of version 0.7.0.0, there are significant (breaking) changes between the 0.7.0.0 and the previous versions.

## Installation

You can install this under development version from [GitHub](https://github.com/krose/entsoeapi) with:

``` r
install.packages("devtools")
devtools::install_github(repo = "krose/entsoeapi", ref = "master")
```

## Security token

Read [here](https://transparencyplatform.zendesk.com/hc/en-us/articles/12845911031188-How-to-get-security-token) how to get a security token. You should also create a `.Renviron` file in your working directory with a security token and call it ENTSOE_PAT.

``` shell
ENTSOE_PAT = "your_security_token"
```

## Examples

You use the eic codes to get the data. Let’s try to find the eic code for Germany.

``` r
entsoeapi::all_approved_eic() |>
  dplyr::filter(EicLongName == "Germany") |>
  dplyr::glimpse()
#> Rows: 1
#> Columns: 11
#> $ EicCode                         <chr> "10Y1001A1001A83F"
#> $ EicDisplayName                  <chr> "DE"
#> $ EicLongName                     <chr> "Germany"
#> $ EicParent                       <chr> NA
#> $ EicResponsibleParty             <chr> NA
#> $ EicStatus                       <chr> "Active"
#> $ MarketParticipantPostalCode     <chr> NA
#> $ MarketParticipantIsoCountryCode <chr> NA
#> $ MarketParticipantVatCode        <chr> NA
#> $ EicTypeFunctionList             <chr> "Member State"
#> $ type                            <chr> "Y"
```

For some of the data you need to translate the generation codes.

``` r
dplyr::glimpse(entsoeapi::asset_types)
#> Rows: 29
#> Columns: 3
#> $ Code        <chr> "A01", "A02", "A03", "A04", "A05", "B01", "B02", "B0…
#> $ Title       <chr> "Tieline", "Line", "Resource Object", "Generation", …
#> $ Description <chr> "A high voltage line used for cross border energy in…
```

Let’s get the demand in Germany.

``` r
entsoeapi::load_actual_total(
  eic = "10Y1001A1001A83F",
  period_start = lubridate::ymd("2020-01-01", tz = "CET"),
  period_end = lubridate::ymd("2020-01-02", tz = "CET")
) |>
  dplyr::glimpse()
#> Rows: 96
#> Columns: 21
#> $ ts_out_bidding_zone_domain_mrid <chr> "10Y1001A1001A83F", "10Y1001A1001A83F", "1…
#> $ ts_out_bidding_zone_domain_name <chr> "Germany", "Germany", "Germany", "Germany"…
#> $ type                            <chr> "A65", "A65", "A65", "A65", "A65", "A65", …
#> $ type_def                        <chr> "System total load", "System total load", …
#> $ process_type                    <chr> "A16", "A16", "A16", "A16", "A16", "A16", …
#> $ process_type_def                <chr> "Realised", "Realised", "Realised", "Reali…
#> $ ts_object_aggregation           <chr> "A01", "A01", "A01", "A01", "A01", "A01", …
#> $ ts_object_aggregation_def       <chr> "Area", "Area", "Area", "Area", "Area", "A…
#> $ ts_business_type                <chr> "A04", "A04", "A04", "A04", "A04", "A04", …
#> $ ts_business_type_def            <chr> "Consumption", "Consumption", "Consumption…
#> $ created_date_time               <dttm> 2024-10-05 19:48:09, 2024-10-05 19:48:09,…
#> $ revision_number                 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ time_period_time_interval_start <dttm> 2019-12-31 23:00:00, 2019-12-31 23:00:00,…
#> $ time_period_time_interval_end   <dttm> 2020-01-01 23:00:00, 2020-01-01 23:00:00,…
#> $ ts_resolution                   <chr> "PT15M", "PT15M", "PT15M", "PT15M", "PT15M…
#> $ ts_time_interval_start          <dttm> 2019-12-31 23:00:00, 2019-12-31 23:00:00,…
#> $ ts_time_interval_end            <dttm> 2020-01-01 23:00:00, 2020-01-01 23:00:00,…
#> $ ts_mrid                         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_point_dt_start               <dttm> 2019-12-31 23:00:00, 2019-12-31 23:15:00,…
#> $ ts_point_quantity               <dbl> 43881.80, 43639.59, 43330.90, 43149.49, 43…
#> $ ts_quantity_measure_unit_name   <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", …
```

This is basically how all the functions work, so let’s try to get the production data too.

``` r
entsoeapi::gen_per_prod_type(
  eic = "10Y1001A1001A83F", 
  period_start = lubridate::ymd("2020-01-01", tz = "CET"),
  period_end = lubridate::ymd("2020-01-02", tz = "CET"),
  gen_type = NULL,
  tidy_output = TRUE
) |>
  dplyr::glimpse()
#> Rows: 1,581
#> Columns: 25
#> $ ts_in_bidding_zone_domain_mrid  <chr> "10Y1001A1001A83F", "10Y1001A1001A83F", "10Y100…
#> $ ts_in_bidding_zone_domain_name  <chr> "Germany", "Germany", "Germany", "Germany", "Ge…
#> $ ts_out_bidding_zone_domain_mrid <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ ts_out_bidding_zone_domain_name <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ type                            <chr> "A75", "A75", "A75", "A75", "A75", "A75", "A75"…
#> $ type_def                        <chr> "Actual generation per type", "Actual generatio…
#> $ process_type                    <chr> "A16", "A16", "A16", "A16", "A16", "A16", "A16"…
#> $ process_type_def                <chr> "Realised", "Realised", "Realised", "Realised",…
#> $ ts_object_aggregation           <chr> "A08", "A08", "A08", "A08", "A08", "A08", "A08"…
#> $ ts_object_aggregation_def       <chr> "Resource type", "Resource type", "Resource typ…
#> $ ts_business_type                <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01"…
#> $ ts_business_type_def            <chr> "Production", "Production", "Production", "Prod…
#> $ ts_mkt_psr_type                 <chr> "B01", "B01", "B01", "B01", "B01", "B01", "B01"…
#> $ ts_mkt_psr_type_def             <chr> "Biomass", "Biomass", "Biomass", "Biomass", "Bi…
#> $ created_date_time               <dttm> 22026-02-16 08:56:28, 2026-02-16 08:56:28, 202…
#> $ revision_number                 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ time_period_time_interval_start <dttm> 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019…
#> $ time_period_time_interval_end   <dttm> 2020-01-01 23:00:00, 2020-01-01 23:00:00, 2020…
#> $ ts_resolution                   <chr> "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "P…
#> $ ts_time_interval_start          <dttm> 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019…
#> $ ts_time_interval_end            <dttm> 2020-01-01 23:00:00, 2020-01-01 23:00:00, 2020…
#> $ ts_mrid                         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start               <dttm> 2019-12-31 23:00:00, 2019-12-31 23:15:00, 2019…
#> $ ts_point_quantity               <dbl> 4809.13, 4803.04, 4787.15, 4787.26, 4784.65, 47…
#> $ ts_quantity_measure_unit_name   <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"…
```
