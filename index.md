# entsoeapi

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
    - implicit_offered_transfer_capacities (11.1) (beta test version)
    - explicit_offered_transfer_capacities (11.1.A) (beta test version)
    - continuous_offered_transfer_capacities (11.1) (beta test version)
    - flow_based_allocations (11.1.B) (beta test version)
    - auction_revenue (12.1.A) (beta test version)
    - total_nominated_capacity (12.1.B)
    - already_allocated_total_capacity (12.1.C)
    - day_ahead_prices (12.1.D)
    - net_positions (12.1.E) (beta test version)
    - congestion_income (12.1.E) (beta test version)
    - allocated_transfer_capacities_3rd_countries (12.1.H) (beta test
      version)
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
    - expansion_and_dismantling_project (9.1) (beta test version)
    - intraday_cross_border_transfer_limits (11.3) (beta test version)
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
    - exchanged_volumes_per_border (3.10, 3.16 & 3.17) (beta test
      version)
    - netted_volumes_per_border (3.10, 3.16 & 3.17) (beta test version)
    - hvdc_link_constrains (4.5) (beta test version)
    - changes_to_bid_availability (mFRR 9.9, aFRR 9.6&9.8) (beta test
      version)
    - current_balancing_state (12.3.A) (beta test version)
    - balancing_energy_bids (12.3.B&C) (beta test version)
    - aggregated_balancing_energy_bids (12.3.E) (beta test version)
    - procured_balancing_capacity (12.3.F) (beta test version)
    - allocation_of_cross_zonal_balancing_cap (12.3.H&I) (beta test
      version)
    - contracted_reserves (17.1.B&C) (beta test version)
    - activated_balancing_prices (17.1.F) (beta test version)
    - imbalance_prices (17.1.G) (beta test version)
    - imbalance_volumes (17.1.H) (beta test version)
    - financial_expenses_and_income_for_balancing (17.1.I) (beta test
      version)
    - fcr_total_capacity (187.2) (beta test version)
    - shares_of_fcr_capacity (187.2) (beta test version)
    - rr_and_frr_actual_capacity (188.4 & 189.3) (beta test version)
    - rr_actual_capacity (189.3) (beta test version)
    - sharing_of_frr_capacity (SO GL 190.1) (beta test version)

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

**IMPORTANT!**  
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

Read
[here](https://transparencyplatform.zendesk.com/hc/en-us/articles/12845911031188-How-to-get-security-token)
how to get a security token. You should also create a `.Renviron` file
in your working directory with a security token and call it
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
  dplyr::filter(EicLongName == "Germany") |>
  dplyr::glimpse()
#> ℹ downloading X_eicCodes.csv file ...
#> ℹ downloading Y_eicCodes.csv file ...
#> ℹ downloading Z_eicCodes.csv file ...
#> ℹ downloading T_eicCodes.csv file ...
#> ℹ downloading V_eicCodes.csv file ...
#> ℹ downloading W_eicCodes.csv file ...
#> ℹ downloading A_eicCodes.csv file ...
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
if (!require("knitr", quietly = TRUE)) install.packages("knitr", quiet = TRUE)
entsoeapi::asset_types |>
  head(n = 12L) |>
  knitr::kable(format = "html")
```

| code | title                   | description                                                                                                                 |
|:-----|:------------------------|:----------------------------------------------------------------------------------------------------------------------------|
| A01  | Tie line                | A high voltage line used for cross border energy interconnections.                                                          |
| A02  | Line                    | A specific electric line within a country.                                                                                  |
| A03  | Resource Object         | A resource that can either produce or consume energy.                                                                       |
| A04  | Generation              | A resource that can produce energy.                                                                                         |
| A05  | Load                    | A resource that can consume energy.                                                                                         |
| A06  | Phase Shift Transformer | An electrical device for controlling the power flow through specific lines in a power transmission network.                 |
| A07  | Circuit Breaker         | An electrical switch designed to protect an electrical circuit from damage caused by overcurrent/overload or short circuit. |
| A08  | Busbar                  | A specific element within a substation to connect grid elements for energy distribution purposes.                           |
| A09  | Capacitor               | A transmission element designed to inject reactive power into the transmission network.                                     |
| A10  | Inductor                | A transmission element designed to compensate reactive power in the transmission network.                                   |
| A11  | Power plant connection  | All the network equipment that link the generating unit to the grid.                                                        |
| A12  | FACTS                   | Flexible Alternating Current Transmission System                                                                            |

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
#> <- date: Tue, 10 Mar 2026 18:15:50 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Actual Total Load_201912312300-202001012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <-
#> ✔ response has arrived
#> ℹ pulling Y_eicCodes.csv file from cache
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
#> $ created_date_time               <dttm> 2026-03-10 18:15:50, 2026-03-10 18:15…
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
#> <- date: Tue, 10 Mar 2026 18:15:52 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Aggregated Generation per Type_201912312300-202001012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <-
#> ✔ response has arrived
#> Rows: 1,632
#> Columns: 25
#> $ ts_in_bidding_zone_domain_mrid  <chr> "10Y1001A1001A83F", "10Y1001A1001A83F"…
#> $ ts_in_bidding_zone_domain_name  <chr> "Germany", "Germany", "Germany", "Germ…
#> $ ts_out_bidding_zone_domain_mrid <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ ts_out_bidding_zone_domain_name <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ type                            <chr> "A75", "A75", "A75", "A75", "A75", "A7…
#> $ type_def                        <chr> "Actual generation per type", "Actual …
#> $ process_type                    <chr> "A16", "A16", "A16", "A16", "A16", "A1…
#> $ process_type_def                <chr> "Realised", "Realised", "Realised", "R…
#> $ ts_object_aggregation           <chr> "A08", "A08", "A08", "A08", "A08", "A0…
#> $ ts_object_aggregation_def       <chr> "Resource type", "Resource type", "Res…
#> $ ts_business_type                <chr> "A01", "A01", "A01", "A01", "A01", "A0…
#> $ ts_business_type_def            <chr> "Production", "Production", "Productio…
#> $ ts_mkt_psr_type                 <chr> "B01", "B01", "B01", "B01", "B01", "B0…
#> $ ts_mkt_psr_type_def             <chr> "Biomass", "Biomass", "Biomass", "Biom…
#> $ created_date_time               <dttm> 2026-03-10 18:15:52, 2026-03-10 18:15…
#> $ revision_number                 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ time_period_time_interval_start <dttm> 2019-12-31 23:00:00, 2019-12-31 23:00…
#> $ time_period_time_interval_end   <dttm> 2020-01-01 23:00:00, 2020-01-01 23:00…
#> $ ts_resolution                   <chr> "PT15M", "PT15M", "PT15M", "PT15M", "P…
#> $ ts_time_interval_start          <dttm> 2019-12-31 23:00:00, 2019-12-31 23:00…
#> $ ts_time_interval_end            <dttm> 2020-01-01 23:00:00, 2020-01-01 23:00…
#> $ ts_mrid                         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start               <dttm> 2019-12-31 23:00:00, 2019-12-31 23:15…
#> $ ts_point_quantity               <dbl> 4809.13, 4803.04, 4787.15, 4787.26, 47…
#> $ ts_quantity_measure_unit_name   <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MA…
```

## Code of Conduct

Please note that the entsoeapi project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
