
<!-- README.md is generated from README.Rmd. Please edit that file -->

# entsoeapi <img src="man/figures/logo.png" width="120" height="120" alt="hexa sticker" />

<!-- badges: start -->

[![Lifecycle:experimental](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)  
[![lint-project.yml](https://github.com/krose/entsoeapi/actions/workflows/lint-project.yml/badge.svg)](https://github.com/krose/entsoeapi/actions/workflows/lint-project.yml)  
[![R-CMD-check](https://github.com/krose/entsoeapi/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/krose/entsoeapi/actions/workflows/R-CMD-check.yml)  
[![test-coverage.yml](https://github.com/krose/entsoeapi/actions/workflows/test-coverage.yml/badge.svg)](https://github.com/krose/entsoeapi/actions/workflows/test-coverage.yml)

<!-- badges: end -->

The goal of `entsoeapi` package is to create an easy wrapper around the
ENTSO-E
[API](https://documenter.getpostman.com/view/7009892/2s93JtP3F6)’s data
and transform them to tabular format without effort. (The downloadable
data are available interactively on the ENTSO-E [transparency
platform](https://transparency.entsoe.eu/) website as well.)

The package helps with

- displaying the queried endpoint URL to easier double check
- upfront checking of function arguments’ validity to avoid useless API
  calls
- query pagination, by allowing the user to not worry about it at all
  since the package does all necessary requests
- unpacking compressed file responses
- caching data to enhance processing speed
- converting XML structures to tabular ones
- composing consistent and detailed outputs
- providing related, but not API accessible data (for instance:
  business_types)
- automatic assigning definitions to codes
- calculating and adding timestamps to data points (the response xml
  does not contain such information explicitly)

------------------------------------------------------------------------

- Already available ENTSO-E API endpoints:
  - MARKET
    - implicit_offered_transfer_capacities (11.1)
    - explicit_offered_transfer_capacities (11.1.A)
    - continuous_offered_transfer_capacities (11.1)
    - flow_based_allocations (11.1.B)
    - auction_revenue (12.1.A)
    - total_nominated_capacity (12.1.B)
    - already_allocated_total_capacity (12.1.C)
    - day_ahead_prices (12.1.D)
    - net_positions (12.1.E)
    - congestion_income (12.1.E)
    - allocated_transfer_capacities_3rd_countries (12.1.H)
  - LOAD
    - load_actual_total (6.1.A)
    - load_day_ahead_total_forecast (6.1.B)
    - load_week_ahead_total_forecast (6.1.C)
    - load_month_ahead_total_forecast (6.1.D)
    - load_year_ahead_total_forecast (6.1.E)
    - load_year_ahead_forecast_margin (8.1)
  - GENERATION
    - gen_installed_capacity_per_pt (14.1.A)
    - the gen_installed_capacity_per_pu (14.1.B)
    - gen_day_ahead (14.1.C)
    - gen_wind_solar_forecasts (14.1.D)
    - gen_per_gen_unit (16.1.A)
    - gen_per_prod_type (16.1.B&C)
    - the gen_storage_mean_filling_rate (16.1.D)
  - TRANSMISSION
    - expansion_and_dismantling_project (9.1) (no data available on the
      ENTSO-E API yet, as of 24 March 2026)
    - intraday_cross_border_transfer_limits (11.3)
    - forecasted_transfer_capacities (11.1.A)
    - day_ahead_commercial_sched (12.1.F)
    - total_commercial_sched (12.1.F)
    - cross_border_physical_flows (12.1.G)
    - redispatching_internal (13.1.A)
    - redispatching_cross_border (13.1.A)
    - countertrading (13.1.B)
    - costs_of_congestion_management (13.1.C)
  - OUTAGES
    - outages_cons_units (7.1.A&B)
    - outages_fallbacks (IFs IN 7.2, mFRR 3.11, aFRR 3.10)
    - outages_transmission_grid (10.1.A&B)
    - outages_offshore_grid (10.1.C)
    - outages_gen_units (15.1.A&B)
    - outages_prod_units (15.1.C&D)
    - outages_both (15.1.A&B + 15.1.C&D)
  - BALANCING
    - exchanged_volumes (aFRR 3.16, mFRR 3.17)
    - netted_volumes (IFs IN 3.10)
    - elastic_demands (IF mFRR 3.4)
    - balancing_border_cap_limit (IFs 4.3 & 4.4)
    - exchanged_volumes_per_border (3.10, 3.16 & 3.17)
    - netted_volumes_per_border (3.10, 3.16 & 3.17) (no data available
      on the ENTSO-E API yet, as of 24 March 2026)
    - hvdc_link_constrains (4.5) (no data available on the ENTSO-E API
      yet, as of 24 March 2026)
    - changes_to_bid_availability (mFRR 9.9, aFRR 9.6&9.8) (no data
      available on the ENTSO-E API yet, as of 24 March 2026)
    - current_balancing_state (12.3.A)
    - balancing_energy_bids (12.3.B&C) (no data available on the ENTSO-E
      API yet, as of 24 March 2026)
    - (12.3.D)
    - aggregated_balancing_energy_bids (12.3.E)
    - procured_balancing_capacity (12.3.F)
    - allocation_of_cross_zonal_balancing_cap (12.3.H&I) (no data
      available on the ENTSO-E API yet, as of 24 March 2026)
    - contracted_reserves (17.1.B&C)
    - activated_balancing_prices (17.1.F)
    - imbalance_prices (17.1.G)
    - imbalance_volumes (17.1.H)
    - financial_expenses_and_income (17.1.I)
    - fcr_total_capacity (187.2)
    - rr_and_frr_actual_capacity (188.4 & 189.3)
    - shares_of_fcr_capacity (187.2)
    - sharing_of_rr_and_frr_capacity (SO GL 190.1) (no data available on
      the ENTSO-E API yet, as of 24 March 2026)

Be aware, that not all API endpoints are implemented in this package,
and not every endpoint provides data.

If you would like to use an unimplemented endpoint, please submit an
[issue](https://github.com/krose/entsoeapi/issues/new/choose) and we’ll
do our best to resolve it.

If the endpoint is already implemented, but the related function gives
back an empty table, then check the response XML in a browser using the
request URL displayed in the console just after issuing the function
call. Another verification option might be to check the response on the
[Entsoe-e Transparency Platform](https://transparency.entsoe.eu/).

In case of beta test version functions there may be unique quirks which
are not handled yet. So please, compare the resulting tables to the data
fn the [Entsoe-e Transparency
Platform](https://transparency.entsoe.eu/).

  
<b>IMPORTANT!</b>  
Since the underlying engine has fairly been standardized with the
introduction of version 0.7.0.0, there are significant (breaking)
changes between the 0.7.0.0 and the previous versions.

## Installation

You can install the development version of entsoeapi from
[GitHub](https://github.com/krose/entsoeapi) with:

``` r
if (!require("devtools", quietly = TRUE)) install.packages("devtools", quiet = TRUE)
devtools::install_github(repo = "krose/entsoeapi", ref = "main")
```

## Security token

Read the related ENTSO-E Transparency Platform documentation
[here](https://transparencyplatform.zendesk.com/hc/en-us/articles/12845911031188-How-to-get-security-token)
about how to get a security token. You should also create a `.Renviron`
file in your working directory with a security token and call it
`ENTSOE_PAT`.

``` r
if (!require("usethis", quietly = TRUE)) install.packages("usethis", quiet = TRUE)
usethis::edit_r_environ()
```

`ENTSOE_PAT = "your_security_token"`

## Examples

You use the eic codes to get the data. Let’s try to find the eic code
for Germany.

``` r
if (!require("dplyr", quietly = TRUE)) install.packages("dplyr", quiet = TRUE)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
entsoeapi::all_approved_eic() |>
  dplyr::filter(eic_long_name == "Germany") |>
  dplyr::glimpse()
#> 
#> ── public download ─────────────────────────────────────────────────────────────
#> ℹ downloading X_eicCodes.csv file ...
#> 
#> ── public download ─────────────────────────────────────────────────────────────
#> ℹ downloading Y_eicCodes.csv file ...
#> 
#> ── public download ─────────────────────────────────────────────────────────────
#> ℹ downloading Z_eicCodes.csv file ...
#> 
#> ── public download ─────────────────────────────────────────────────────────────
#> ℹ downloading T_eicCodes.csv file ...
#> 
#> ── public download ─────────────────────────────────────────────────────────────
#> ℹ downloading V_eicCodes.csv file ...
#> 
#> ── public download ─────────────────────────────────────────────────────────────
#> ℹ downloading W_eicCodes.csv file ...
#> 
#> ── public download ─────────────────────────────────────────────────────────────
#> ℹ downloading A_eicCodes.csv file ...
#> Rows: 1
#> Columns: 11
#> $ eic_code                            <chr> "10Y1001A1001A83F"
#> $ eic_display_name                    <chr> "DE"
#> $ eic_long_name                       <chr> "Germany"
#> $ eic_parent                          <chr> NA
#> $ eic_responsible_party               <chr> NA
#> $ eic_status                          <chr> "Active"
#> $ market_participant_postal_code      <chr> NA
#> $ market_participant_iso_country_code <chr> NA
#> $ market_participant_vat_code         <chr> NA
#> $ eic_type_function_list              <chr> "Member State"
#> $ type                                <chr> "Y"
```

For some of the data you need to translate the generation codes.

``` r
if (!require("knitr", quietly = TRUE)) install.packages("knitr", quiet = TRUE)
entsoeapi::asset_types |>
  head(n = 12L) |>
  knitr::kable(format = "html")
```

<table>

<thead>

<tr>

<th style="text-align:left;">

code
</th>

<th style="text-align:left;">

title
</th>

<th style="text-align:left;">

description
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

A01
</td>

<td style="text-align:left;">

Tie line
</td>

<td style="text-align:left;">

A high voltage line used for cross border energy interconnections.
</td>

</tr>

<tr>

<td style="text-align:left;">

A02
</td>

<td style="text-align:left;">

Line
</td>

<td style="text-align:left;">

A specific electric line within a country.
</td>

</tr>

<tr>

<td style="text-align:left;">

A03
</td>

<td style="text-align:left;">

Resource Object
</td>

<td style="text-align:left;">

A resource that can either produce or consume energy.
</td>

</tr>

<tr>

<td style="text-align:left;">

A04
</td>

<td style="text-align:left;">

Generation
</td>

<td style="text-align:left;">

A resource that can produce energy.
</td>

</tr>

<tr>

<td style="text-align:left;">

A05
</td>

<td style="text-align:left;">

Load
</td>

<td style="text-align:left;">

A resource that can consume energy.
</td>

</tr>

<tr>

<td style="text-align:left;">

A06
</td>

<td style="text-align:left;">

Phase Shift Transformer
</td>

<td style="text-align:left;">

An electrical device for controlling the power flow through specific
lines in a power transmission network.
</td>

</tr>

<tr>

<td style="text-align:left;">

A07
</td>

<td style="text-align:left;">

Circuit Breaker
</td>

<td style="text-align:left;">

An electrical switch designed to protect an electrical circuit from
damage caused by overcurrent/overload or short circuit.
</td>

</tr>

<tr>

<td style="text-align:left;">

A08
</td>

<td style="text-align:left;">

Busbar
</td>

<td style="text-align:left;">

A specific element within a substation to connect grid elements for
energy distribution purposes.
</td>

</tr>

<tr>

<td style="text-align:left;">

A09
</td>

<td style="text-align:left;">

Capacitor
</td>

<td style="text-align:left;">

A transmission element designed to inject reactive power into the
transmission network.
</td>

</tr>

<tr>

<td style="text-align:left;">

A10
</td>

<td style="text-align:left;">

Inductor
</td>

<td style="text-align:left;">

A transmission element designed to compensate reactive power in the
transmission network.
</td>

</tr>

<tr>

<td style="text-align:left;">

A11
</td>

<td style="text-align:left;">

Power plant connection
</td>

<td style="text-align:left;">

All the network equipment that link the generating unit to the grid.
</td>

</tr>

<tr>

<td style="text-align:left;">

A12
</td>

<td style="text-align:left;">

FACTS
</td>

<td style="text-align:left;">

Flexible Alternating Current Transmission System
</td>

</tr>

</tbody>

</table>

Let’s get the demand of 2020-01-01 in Germany.

``` r
if (!require("dplyr", quietly = TRUE)) install.packages("dplyr")
entsoeapi::load_actual_total(
  eic = "10Y1001A1001A83F",
  period_start = lubridate::ymd("2020-01-01", tz = "CET"),
  period_end = lubridate::ymd("2020-01-02", tz = "CET")
) |>
  dplyr::glimpse()
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A65&processType=A16&outBiddingZone_Domain=10Y1001A1001A83F&periodStart=201912312300&periodEnd=202001012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 30 Mar 2026 21:56:04 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Total Load_201912312300-202001012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <-
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> 
#> ── public download ─────────────────────────────────────────────────────────────
#> ℹ pulling Y_eicCodes.csv file from cache
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> Rows: 96
#> Columns: 21
#> $ ts_out_bidding_zone_domain_mrid <chr> "10Y1001A1001A83F", "10Y1001A1001A83F"…
#> $ ts_out_bidding_zone_domain_name <chr> "Germany", "Germany", "Germany", "Germ…
#> $ type                            <chr> "A65", "A65", "A65", "A65", "A65", "A6…
#> $ type_def                        <chr> "System total load", "System total loa…
#> $ process_type                    <chr> "A16", "A16", "A16", "A16", "A16", "A1…
#> $ process_type_def                <chr> "Realised", "Realised", "Realised", "R…
#> $ ts_object_aggregation           <chr> "A01", "A01", "A01", "A01", "A01", "A0…
#> $ ts_object_aggregation_def       <chr> "Area", "Area", "Area", "Area", "Area"…
#> $ ts_business_type                <chr> "A04", "A04", "A04", "A04", "A04", "A0…
#> $ ts_business_type_def            <chr> "Consumption", "Consumption", "Consump…
#> $ created_date_time               <dttm> 2026-03-30 21:56:04, 2026-03-30 21:56…
#> $ revision_number                 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ time_period_time_interval_start <dttm> 2019-12-31 23:00:00, 2019-12-31 23:00…
#> $ time_period_time_interval_end   <dttm> 2020-01-01 23:00:00, 2020-01-01 23:00…
#> $ ts_resolution                   <chr> "PT15M", "PT15M", "PT15M", "PT15M", "P…
#> $ ts_time_interval_start          <dttm> 2019-12-31 23:00:00, 2019-12-31 23:00…
#> $ ts_time_interval_end            <dttm> 2020-01-01 23:00:00, 2020-01-01 23:00…
#> $ ts_mrid                         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start               <dttm> 2019-12-31 23:00:00, 2019-12-31 23:15…
#> $ ts_point_quantity               <dbl> 43881.80, 43639.59, 43330.90, 43149.49…
#> $ ts_quantity_measure_unit_name   <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MA…
```

This is basically how all the functions work, so let’s try to get the
production data too.

``` r
if (!require("dplyr", quietly = TRUE)) install.packages("dplyr")
entsoeapi::gen_per_prod_type(
  eic = "10Y1001A1001A83F",
  period_start = lubridate::ymd("2020-01-01", tz = "CET"),
  period_end = lubridate::ymd("2020-01-02", tz = "CET"),
  gen_type = NULL,
  tidy_output = TRUE
) |>
  dplyr::glimpse()
#> 
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A75&processType=A16&in_Domain=10Y1001A1001A83F&periodStart=201912312300&periodEnd=202001012300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 30 Mar 2026 21:56:06 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Aggregated Generation per Type_201912312300-202001012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <-
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!
#> ✔ Additional definitions have been added!
#> Rows: 1,632
#> Columns: 25
#> $ ts_in_bidding_zone_domain_mrid  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ ts_in_bidding_zone_domain_name  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ ts_out_bidding_zone_domain_mrid <chr> "10Y1001A1001A83F", "10Y1001A1001A83F"…
#> $ ts_out_bidding_zone_domain_name <chr> "Germany", "Germany", "Germany", "Germ…
#> $ type                            <chr> "A75", "A75", "A75", "A75", "A75", "A7…
#> $ type_def                        <chr> "Actual generation per type", "Actual …
#> $ process_type                    <chr> "A16", "A16", "A16", "A16", "A16", "A1…
#> $ process_type_def                <chr> "Realised", "Realised", "Realised", "R…
#> $ ts_object_aggregation           <chr> "A08", "A08", "A08", "A08", "A08", "A0…
#> $ ts_object_aggregation_def       <chr> "Resource type", "Resource type", "Res…
#> $ ts_business_type                <chr> "A01", "A01", "A01", "A01", "A01", "A0…
#> $ ts_business_type_def            <chr> "Production", "Production", "Productio…
#> $ ts_mkt_psr_type                 <chr> "B10", "B10", "B10", "B10", "B10", "B1…
#> $ ts_mkt_psr_type_def             <chr> "Hydro-electric pure pumped storage he…
#> $ created_date_time               <dttm> 2026-03-30 21:56:06, 2026-03-30 21:56…
#> $ revision_number                 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ time_period_time_interval_start <dttm> 2019-12-31 23:00:00, 2019-12-31 23:00…
#> $ time_period_time_interval_end   <dttm> 2020-01-01 23:00:00, 2020-01-01 23:00…
#> $ ts_resolution                   <chr> "PT15M", "PT15M", "PT15M", "PT15M", "P…
#> $ ts_time_interval_start          <dttm> 2019-12-31 23:00:00, 2019-12-31 23:00…
#> $ ts_time_interval_end            <dttm> 2020-01-01 23:00:00, 2020-01-01 23:00…
#> $ ts_mrid                         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start               <dttm> 2019-12-31 23:00:00, 2019-12-31 23:15…
#> $ ts_point_quantity               <dbl> 194.92, 225.11, 286.19, 397.02, 284.99…
#> $ ts_quantity_measure_unit_name   <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MA…
```

## Code of Conduct

## Code of Conduct

Please note that the entsoeapi project is released with a [Contributor
Code of
Conduct](https://krose.github.io/entsoeapi/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
