
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

<tr>

<td style="text-align:left;">

A13
</td>

<td style="text-align:left;">

Production unit
</td>

<td style="text-align:left;">

A production unit is a composition of one or several generation units.
</td>

</tr>

<tr>

<td style="text-align:left;">

A14
</td>

<td style="text-align:left;">

Internal tie line
</td>

<td style="text-align:left;">

An internal tie line is a line between two scheduling areas within the
same bidding zone.
</td>

</tr>

<tr>

<td style="text-align:left;">

B01
</td>

<td style="text-align:left;">

Biomass
</td>

<td style="text-align:left;">

A resource using biomass for energy.
</td>

</tr>

<tr>

<td style="text-align:left;">

B02
</td>

<td style="text-align:left;">

Fossil Brown coal/Lignite
</td>

<td style="text-align:left;">

A resource using Fossil Brown coal/Lignite for energy.
</td>

</tr>

<tr>

<td style="text-align:left;">

B03
</td>

<td style="text-align:left;">

Fossil Coal-derived gas
</td>

<td style="text-align:left;">

A resource using Fossil Coal-derived gas for energy.
</td>

</tr>

<tr>

<td style="text-align:left;">

B04
</td>

<td style="text-align:left;">

Fossil Gas
</td>

<td style="text-align:left;">

A resource using Fossil Gas for energy.
</td>

</tr>

<tr>

<td style="text-align:left;">

B05
</td>

<td style="text-align:left;">

Fossil Hard coal
</td>

<td style="text-align:left;">

A resource using Fossil Hard coal for energy.
</td>

</tr>

<tr>

<td style="text-align:left;">

B06
</td>

<td style="text-align:left;">

Fossil Oil
</td>

<td style="text-align:left;">

A resource using Fossil Oil for energy.
</td>

</tr>

<tr>

<td style="text-align:left;">

B07
</td>

<td style="text-align:left;">

Fossil Oil shale
</td>

<td style="text-align:left;">

A resource using Fossil Oil shale for energy.
</td>

</tr>

<tr>

<td style="text-align:left;">

B08
</td>

<td style="text-align:left;">

Fossil Peat
</td>

<td style="text-align:left;">

A resource using Fossil Peat for energy.
</td>

</tr>

<tr>

<td style="text-align:left;">

B09
</td>

<td style="text-align:left;">

Geothermal
</td>

<td style="text-align:left;">

A resource using Geothermal for energy.
</td>

</tr>

<tr>

<td style="text-align:left;">

B10
</td>

<td style="text-align:left;">

Hydro-electric pure pumped storage head installation
</td>

<td style="text-align:left;">

Unit in which moving water energy is converted to electricity using
flowing water to generate electricity with a large dam and reservoirs.
Pure pumped storage plants store water in an upper reservoir with no
natural inflows.
</td>

</tr>

<tr>

<td style="text-align:left;">

B11
</td>

<td style="text-align:left;">

Hydro Run-of-river head installation
</td>

<td style="text-align:left;">

Unit in which moving water energy is converted to electricity using
flowing water to generate electricity in the absence of a large dam and
reservoirs.
</td>

</tr>

<tr>

<td style="text-align:left;">

B12
</td>

<td style="text-align:left;">

Hydro-electric storage head installation
</td>

<td style="text-align:left;">

Unit in which moving water energy is converted to electricity using
flowing water to generate electricity with a large dam and reservoirs.
</td>

</tr>

<tr>

<td style="text-align:left;">

B13
</td>

<td style="text-align:left;">

Marine unspecified
</td>

<td style="text-align:left;">

Unit in which marine energy is converted to electricity with
equipment/devices not specified.
</td>

</tr>

<tr>

<td style="text-align:left;">

B14
</td>

<td style="text-align:left;">

Nuclear unspecified
</td>

<td style="text-align:left;">

A unit in which the heat source is a nuclear reactor of type that is not
specified in other nuclear types.
</td>

</tr>

<tr>

<td style="text-align:left;">

B15
</td>

<td style="text-align:left;">

Other renewable
</td>

<td style="text-align:left;">

A resource using Other renewable for energy.
</td>

</tr>

<tr>

<td style="text-align:left;">

B16
</td>

<td style="text-align:left;">

Solar unspecified
</td>

<td style="text-align:left;">

Unit in which solar energy is converted to electricity with
equipment/devices not specified.
</td>

</tr>

<tr>

<td style="text-align:left;">

B17
</td>

<td style="text-align:left;">

Waste
</td>

<td style="text-align:left;">

A resource using Waste for energy.
</td>

</tr>

<tr>

<td style="text-align:left;">

B18
</td>

<td style="text-align:left;">

Wind Offshore
</td>

<td style="text-align:left;">

Unit in which wind energy is converted to electricity using wind farms
constructed in bodies of water, usually in the ocean.
</td>

</tr>

<tr>

<td style="text-align:left;">

B19
</td>

<td style="text-align:left;">

Wind Onshore
</td>

<td style="text-align:left;">

Unit in which wind energy is converted to electricity using wind farms
constructed on land.
</td>

</tr>

<tr>

<td style="text-align:left;">

B20
</td>

<td style="text-align:left;">

Other unspecified
</td>

<td style="text-align:left;">

Other unspecified technology.
</td>

</tr>

<tr>

<td style="text-align:left;">

B21
</td>

<td style="text-align:left;">

AC Link
</td>

<td style="text-align:left;">

Overhead line or cable which is used to transmit electrical power via
Alternative Current.
</td>

</tr>

<tr>

<td style="text-align:left;">

B22
</td>

<td style="text-align:left;">

DC Link
</td>

<td style="text-align:left;">

Overhead line or cable which is used to transmit electrical power via
Direct Current.
</td>

</tr>

<tr>

<td style="text-align:left;">

B23
</td>

<td style="text-align:left;">

Substation
</td>

<td style="text-align:left;">

An assembly of equipment in an electric power system through which
electric energy is passed for transmission, transformation, distribution
or switching.
</td>

</tr>

<tr>

<td style="text-align:left;">

B24
</td>

<td style="text-align:left;">

Transformer
</td>

<td style="text-align:left;">

Electrical device that transfers energy from one voltage level to
another voltage level.
</td>

</tr>

<tr>

<td style="text-align:left;">

B25
</td>

<td style="text-align:left;">

Energy storage
</td>

<td style="text-align:left;">

A resource that stores energy. It could be gas, electricity, etc.
</td>

</tr>

<tr>

<td style="text-align:left;">

B26
</td>

<td style="text-align:left;">

Demand Side Response
</td>

<td style="text-align:left;">

A resource that change its electricity consumption patterns in response
to a signal or incentive.
</td>

</tr>

<tr>

<td style="text-align:left;">

B27
</td>

<td style="text-align:left;">

Dispatchable hydro resource
</td>

<td style="text-align:left;">

A resource referring to dispatchable hydro generation.
</td>

</tr>

<tr>

<td style="text-align:left;">

B28
</td>

<td style="text-align:left;">

Solar photovoltaic
</td>

<td style="text-align:left;">

Unit in which solar energy is converted to electricity using a
technology based on the photoelectric effect.
</td>

</tr>

<tr>

<td style="text-align:left;">

B29
</td>

<td style="text-align:left;">

Solar concentration
</td>

<td style="text-align:left;">

Unit in which solar energy is converted to electricity using mirrors to
concentrate the sun’s energy to drive traditional steam turbines or
engines.
</td>

</tr>

<tr>

<td style="text-align:left;">

B30
</td>

<td style="text-align:left;">

Wind unspecified
</td>

<td style="text-align:left;">

Unit in which wind energy is converted to electricity with
equipment/devices not specified.
</td>

</tr>

<tr>

<td style="text-align:left;">

B31
</td>

<td style="text-align:left;">

Hydro-electric unspecified
</td>

<td style="text-align:left;">

Unit in which moving water energy is converted to electricity with
equipment/devices not specified.
</td>

</tr>

<tr>

<td style="text-align:left;">

B32
</td>

<td style="text-align:left;">

Hydro-electric mixed pumped storage head installation
</td>

<td style="text-align:left;">

Unit in which moving water energy is converted to electricity using
flowing water to generate electricity with a large dam and reservoirs.
Mixed pumped storage plants use a combination of pumped storage and
conventional hydroelectric plants with an upper reservoir that is
replenished in part by natural inflows from a stream or river.
</td>

</tr>

<tr>

<td style="text-align:left;">

B33
</td>

<td style="text-align:left;">

Marine tidal
</td>

<td style="text-align:left;">

Unit in which marine energy from tides is converted to electricity.
</td>

</tr>

<tr>

<td style="text-align:left;">

B34
</td>

<td style="text-align:left;">

Marine wave
</td>

<td style="text-align:left;">

Unit in which marine energy from waves is converted to electricity.
</td>

</tr>

<tr>

<td style="text-align:left;">

B35
</td>

<td style="text-align:left;">

Marine currents
</td>

<td style="text-align:left;">

Unit in which marine energy from currents is converted to electricity.
</td>

</tr>

<tr>

<td style="text-align:left;">

B36
</td>

<td style="text-align:left;">

Marine pressure
</td>

<td style="text-align:left;">

Unit in which marine energy from pressure is converted to electricity.
</td>

</tr>

<tr>

<td style="text-align:left;">

B37
</td>

<td style="text-align:left;">

Thermal unspecified
</td>

<td style="text-align:left;">

Unit in which heat energy is converted to electricity with
equipment/devices not specified in other thermal types.
</td>

</tr>

<tr>

<td style="text-align:left;">

B38
</td>

<td style="text-align:left;">

Thermal combined cycle gas turbine with heat recovery
</td>

<td style="text-align:left;">

Unit in which heat energy is converted to electricity called Combined
Cycle Gas Turbine. The power is generated by the single or multiple gas
turbine(s) in combination with the steam turbine(s). The unit might be
equipped with waste heat recovery (e.g. to district heating network).
</td>

</tr>

<tr>

<td style="text-align:left;">

B39
</td>

<td style="text-align:left;">

Thermal steam turbine with back-pressure turbine (open cycle)
</td>

<td style="text-align:left;">

Unit in which heat energy is converted to electricity. The power is
generated with the steam that is expanded in the back-pressure steam
turbine with or without heat output (e.g. to district heating network).
</td>

</tr>

<tr>

<td style="text-align:left;">

B40
</td>

<td style="text-align:left;">

Thermal steam turbine with condensation turbine (closed cycle)
</td>

<td style="text-align:left;">

Unit in which heat energy is converted to electricity. The power is
generated with the steam that is expanded in the condensation steam
turbine with or without heat output (e.g. to district heating network).
</td>

</tr>

<tr>

<td style="text-align:left;">

B41
</td>

<td style="text-align:left;">

Thermal gas turbine with heat recovery
</td>

<td style="text-align:left;">

Unit in which heat energy is converted to electricity called Simple
Cycle Gas Turbine. The power is generated by the gas turbine and the
flue gas waste heat is recovered (e.g. to district heating network).
</td>

</tr>

<tr>

<td style="text-align:left;">

B42
</td>

<td style="text-align:left;">

Thermal internal combustion engine
</td>

<td style="text-align:left;">

An internal combustion engine is a heat engine in which the combustion
of a fuel occurs with an oxidizer (usually air) in a combustion chamber
that is an integral part of the working fluid flow circuit
(e.g. reciprocating engine). The unit might be equipped with waste heat
recovery (e.g. to district heating network).
</td>

</tr>

<tr>

<td style="text-align:left;">

B43
</td>

<td style="text-align:left;">

Thermal micro-turbine
</td>

<td style="text-align:left;">

Unit in which heat energy is converted to electricity called Simple
Cycle Gas Turbine. The power is generated by the gas turbine (capacity
less than 500kWe). The unit might be equipped with waste heat recovery
(e.g. to district heating network).
</td>

</tr>

<tr>

<td style="text-align:left;">

B44
</td>

<td style="text-align:left;">

Thermal Stirling engine
</td>

<td style="text-align:left;">

A Stirling engine is a heat engine that is operated by the cyclic
compression and expansion of air or other gas (the working fluid) at
different temperatures, resulting in a net conversion of heat energy to
mechanical work.
</td>

</tr>

<tr>

<td style="text-align:left;">

B45
</td>

<td style="text-align:left;">

Thermal fuel cell
</td>

<td style="text-align:left;">

A fuel cell is an electrochemical cell that converts the chemical energy
of a fuel (e.g. hydrogen) and an oxidizing agent (e.g. oxygen) into
electricity through a pair of redox reactions.
</td>

</tr>

<tr>

<td style="text-align:left;">

B46
</td>

<td style="text-align:left;">

Thermal steam engine
</td>

<td style="text-align:left;">

A steam engine is a heat engine that performs mechanical work using
steam as its working fluid. The steam engine uses the force produced by
steam pressure to push a piston back and forth inside a cylinder.
</td>

</tr>

<tr>

<td style="text-align:left;">

B47
</td>

<td style="text-align:left;">

Thermal organic Rankine cycle
</td>

<td style="text-align:left;">

The Organic Rankine Cycle (ORC) is named for its use of an organic, high
molecular mass fluid with a liquid-vapor phase change, or boiling point,
occurring at a lower temperature than the water-steam phase change. The
fluid allows Rankine cycle heat recovery from lower temperature sources
such as biomass combustion, industrial waste heat, geothermal heat,
solar ponds etc. The low-temperature heat is converted into useful work,
that can itself be converted into electricity.
</td>

</tr>

<tr>

<td style="text-align:left;">

B48
</td>

<td style="text-align:left;">

Thermal gas turbine without heat recovery
</td>

<td style="text-align:left;">

Unit in which heat energy is converted to electricity called Simple
Cycle Gas Turbine. The power is generated by the gas turbine and there
is no flue gas waste heat recovery.
</td>

</tr>

<tr>

<td style="text-align:left;">

B49
</td>

<td style="text-align:left;">

Nuclear heavy water reactor
</td>

<td style="text-align:left;">

A unit in which the heat source is a pressurized heavy-water reactor
(PHWR) that is a nuclear reactor that uses heavy water (deuterium oxide
D2O) as its coolant and neutron moderator.
</td>

</tr>

<tr>

<td style="text-align:left;">

B50
</td>

<td style="text-align:left;">

Nuclear light water reactor
</td>

<td style="text-align:left;">

A unit in which the heat source is a light-water reactor (LWR) that is a
type of thermal-neutron reactor that uses normal water, as both its
coolant and neutron moderator � furthermore a solid form of fissile
elements is used as fuel.
</td>

</tr>

<tr>

<td style="text-align:left;">

B51
</td>

<td style="text-align:left;">

Nuclear breeder
</td>

<td style="text-align:left;">

A unit in which the heat source is a nuclear reactor that generates more
fissile material than it consumes.
</td>

</tr>

<tr>

<td style="text-align:left;">

B52
</td>

<td style="text-align:left;">

Nuclear graphite reactor
</td>

<td style="text-align:left;">

A unit in which the heat source is a graphite-moderated reactor that is
a nuclear reactor that uses carbon as a neutron moderator, which allows
natural uranium to be used as nuclear fuel.
</td>

</tr>

<tr>

<td style="text-align:left;">

B53
</td>

<td style="text-align:left;">

Temporary energy storage
</td>

<td style="text-align:left;">

A resource that is temporarily connected to the grid and that may store
energy when connected, such as an electric vehicle.
</td>

</tr>

<tr>

<td style="text-align:left;">

B54
</td>

<td style="text-align:left;">

Permanent energy storage
</td>

<td style="text-align:left;">

A resource that is permanently connected to the grid and that may store
energy when connected, such as a pumped hydro.
</td>

</tr>

<tr>

<td style="text-align:left;">

B55
</td>

<td style="text-align:left;">

Electric vehicle battery
</td>

<td style="text-align:left;">

A resource using electric vehicle batteries, commercial and private. The
reason for separating vehicle batteries and non-vehicle batteries is
that the vehicle batteries not necessarily is connected to the charger.
</td>

</tr>

<tr>

<td style="text-align:left;">

B56
</td>

<td style="text-align:left;">

Heat pump specified
</td>

<td style="text-align:left;">

A heat pump is a device that uses work to transfer heat from a cool
space to a warm space by transferring thermal energy using a
refrigeration cycle
</td>

</tr>

<tr>

<td style="text-align:left;">

B57
</td>

<td style="text-align:left;">

Heat pump electrical
</td>

<td style="text-align:left;">

A heat pump is a device that uses electricity to transfer heat from a
cool space to a warm space by transferring thermal energy using a
refrigeration cycle.
</td>

</tr>

<tr>

<td style="text-align:left;">

B58
</td>

<td style="text-align:left;">

Heat pump absorption
</td>

<td style="text-align:left;">

A heat pump is a device that uses absorption technology to transfer heat
from a cool space to a warm space by transferring thermal energy using a
refrigeration cycle.
</td>

</tr>

<tr>

<td style="text-align:left;">

B59
</td>

<td style="text-align:left;">

Auxiliary power unit
</td>

<td style="text-align:left;">

A technology that provides energy as a backup.
</td>

</tr>

<tr>

<td style="text-align:left;">

B60
</td>

<td style="text-align:left;">

Water electrolysis unspecified
</td>

<td style="text-align:left;">

Unspecified water electrolysis.
</td>

</tr>

<tr>

<td style="text-align:left;">

B61
</td>

<td style="text-align:left;">

Water electrolysis low temperature unspecified
</td>

<td style="text-align:left;">

Unspecified water electrolysis at low temperature as in an Alkaline or
Proto-Exchange Membrane (PEM) fuel cell.
</td>

</tr>

<tr>

<td style="text-align:left;">

B62
</td>

<td style="text-align:left;">

Water electrolysis low temperature main product
</td>

<td style="text-align:left;">

Main product water electrolysis at low temperature as in an Alkaline or
Proto-Exchange Membrane (PEM) fuel cell.
</td>

</tr>

<tr>

<td style="text-align:left;">

B63
</td>

<td style="text-align:left;">

Water electrolysis high temperature unspecified
</td>

<td style="text-align:left;">

Unspecified water electrolysis at high temperature as in a Solid Oxide
Electrolysis Cell (SOEC) fuel cell.
</td>

</tr>

<tr>

<td style="text-align:left;">

B64
</td>

<td style="text-align:left;">

Steam methane reforming unspecified
</td>

<td style="text-align:left;">

Unspecified methane reforming.
</td>

</tr>

<tr>

<td style="text-align:left;">

B65
</td>

<td style="text-align:left;">

Steam methane reforming without CCS/CCU unspecified
</td>

<td style="text-align:left;">

Main product methane reforming without Carbon Capture and Sequestration
(CCS)/Carbon Capture and Use (CCU).
</td>

</tr>

<tr>

<td style="text-align:left;">

B66
</td>

<td style="text-align:left;">

Steam methane reforming with CCS/CCU unspecified
</td>

<td style="text-align:left;">

Unspecified methane reforming with Carbon Capture and Sequestration
(CCS)/Carbon Capture and Use (CCU).
</td>

</tr>

<tr>

<td style="text-align:left;">

B67
</td>

<td style="text-align:left;">

Steam methane reforming with CCS/CCU main product
</td>

<td style="text-align:left;">

Main product methane reforming with Carbon Capture and Sequestration
(CCS)/Carbon Capture and Use (CCU).
</td>

</tr>

<tr>

<td style="text-align:left;">

B68
</td>

<td style="text-align:left;">

Partial oxidation unspecified
</td>

<td style="text-align:left;">

Unspecified partial oxidation.
</td>

</tr>

<tr>

<td style="text-align:left;">

B69
</td>

<td style="text-align:left;">

Autothermal reforming unspecified
</td>

<td style="text-align:left;">

Unspecified autothermal reforming.
</td>

</tr>

<tr>

<td style="text-align:left;">

B70
</td>

<td style="text-align:left;">

Methanol reforming unspecified
</td>

<td style="text-align:left;">

Unspecified methanol reforming.
</td>

</tr>

<tr>

<td style="text-align:left;">

B71
</td>

<td style="text-align:left;">

Ammonia reforming unspecified
</td>

<td style="text-align:left;">

Unspecified ammonia reforming.
</td>

</tr>

<tr>

<td style="text-align:left;">

B72
</td>

<td style="text-align:left;">

Ammonia gasification
</td>

<td style="text-align:left;">

Unspecified gasification.
</td>

</tr>

<tr>

<td style="text-align:left;">

B73
</td>

<td style="text-align:left;">

Chlor-alkali electrolysis unspecified
</td>

<td style="text-align:left;">

Unspecified alkali electrolysis.
</td>

</tr>

<tr>

<td style="text-align:left;">

B74
</td>

<td style="text-align:left;">

Chlor-alkali electrolysis by-product
</td>

<td style="text-align:left;">

Alkali electrolysis product.
</td>

</tr>

<tr>

<td style="text-align:left;">

B75
</td>

<td style="text-align:left;">

ACDC converter
</td>

<td style="text-align:left;">

ACDC converters are electrical circuits that transform alternating
current (AC) into direct current (DC) and vice versa.
</td>

</tr>

<tr>

<td style="text-align:left;">

B76
</td>

<td style="text-align:left;">

Converter
</td>

<td style="text-align:left;">

Electrical device that converts current between AC and DC.
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
#> <- date: Sun, 08 Mar 2026 23:29:58 GMT
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
#> $ created_date_time               <dttm> 2026-03-08 23:29:58, 2026-03-08 23:29…
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
#> <- date: Sun, 08 Mar 2026 23:29:59 GMT
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
#> $ created_date_time               <dttm> 2026-03-08 23:29:59, 2026-03-08 23:29…
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

Please note that the entsoeapi project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
