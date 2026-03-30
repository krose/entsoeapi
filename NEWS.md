# entsoeapi v1.1.0 (2026-03-31)

## New functionality

-   4 new vignettes (Generation Mix Analysis, Getting Started with entsoeapi, Time Series Restructuring Explained, Working with EIC Codes) have been introduced.
-   The `all_allocated_eic()` function has been exported.
-   The `get_news()` function has been introduced.
-   The ENTSO-E API is known for returning 503 (Service Unavailable) response when it's under heavy load or when a client sends too many requests. Hence we have built-in an automated backoff-and-retry mechanism.

## Changes

-   Typos fixed in the documentations.
-   The package `DESCRIPTION` document slightly adjusted.

## Miscellaneous

-   An orphaned, not exported function and its related unit tests have been removed.

# entsoeapi v1.0.0 (2026-03-25)

## New functionality

-   The `implicit_offered_transfer_capacities()` function has been introduced (we incorrectly stated earlier that this function had been introduced)
-   The `explicit_offered_transfer_capacities()` function has been introduced (we incorrectly stated earlier that this function had been introduced)
-   The `continuous_offered_transfer_capacities()` function has been introduced (we incorrectly stated earlier that this function had been introduced)
-   The `netted_volumes_per_border()` function has been introduced (we incorrectly stated earlier that this function had been introduced)
-   The `eic_functions()` and `all_allocated_eic()` functions have been introduced.
-   The processing capability of `P3M` data resolution has been added to the engine.

## Changes

-   The `aggregated_balancing_energy_bids()` function's `process_type` argument additionally can be set as `A67` or `A68` from now on.
-   The "See Also" blocks have been added to each help.
-   The result table of `imbalance_prices()` call got more columns to display.
-   The `deactivation_requested_date_and_or_time_date` and the `eic_code_market_participant_street_address` columns have been added to `all_allocated_eic()` function's resulting table.
-   The `gen_installed_capacity_per_pt` and the `gen_installed_capacity_per_pu` functions got a new `tidy_output` argument.
-   The XML extracting engine further improved by which the `flow_based_allocations` function's resulting table composed properly from now on.
-   The `tibble`, `purrr`, `tidyselect` package dependencies (Imports) removed from the code.

## Possibly breaking changes

-   The `day_ahead_prices()` function has been renamed to `energy_prices()` and it allows to query day-ahead and intraday prices too.
-   The resulting table's column names converted to snake case in the `party_eic()`, `area_eic()`, `accounting_point_eic()`, `tie_line_eic()`, `location_eic()`, `resource_object_eic()`, `substation_eic()` and `all_approved_eic()` function.
-   The `financial_expenses_and_income_for_balancing()` renamed to `financial_expenses_and_income()`.
-   The `sharing_of_frr_capacity()` renamed to `sharing_of_rr_and_frr_capacity()`.
-   The `rr_actual_capacity()` removed since `rr_and_frr_actual_capacity()` covers its functionality.
-   The `business_type` argument has been removed from `shares_of_fcr_capacity()` since it did not changed the result table at all.
-   The `business_type` argument has been removed from `rr_and_frr_actual_capacity()` since it did not changed the result table at all.
-   The `gen_wind_solar_forecasts()` does not give back list of table by `process_type` from now on; instead `process_type` has been moved into the arguments of the function call.

## Miscellaneous

-   The code base simplified.
-   The "architecture" vignette has been updated according to the changes.
-   The package `DESCRIPTION` document slightly adjusted.

# entsoeapi v0.9.7 (2026-03-17)

## New functionality

-   None.

## Miscellaneous

-   The code base simplified.
-   The package level constants have been placed into a standalone file.
-   These dependency (Imports) packages removed from the code: `curl`, `data.table`, `fs`, `tidyselect`, `utf8`
-   The `sapply()` calls have been removed from some unit tests.
-   The global assignment operators have been removed from the code.
-   The .github/CONTRIBUTING.md file has been created and added.
-   The `DESCRIPTION` document has been updated.
-   A contributor guide has been added.

# entsoeapi v0.9.6 (2026-03-12),

## New functionality

-   Additional EIC assertion check has been added to each user facing functions.
-   The there_is_provider() has been turned to user-facing function.
-   The `architecture` vignette has been composed and added.

## Miscellaneous

-   Across all R files @examples blocks replaced by @examplesIf blocks — since they require an ENTSOE_PAT env var that isn't available in CI.
-   @return section has been added to each exported function.
-   The cran-comments.md has been created and has been added to .Rbuildignore.
-   The .urlchecker config file has been created and has been added to .Rbuildignore.
-   The DESCRIPTION and the LICENSE documents have been updated.

# entsoeapi v0.9.5.1 (2026-03-09)

## New functionality

-   None.

## Miscellaneous

-   There seemed to be a bug in the tidy output of some functions (unrelated dates occurred twice in the tidy output). This issue has been resolved.
-   The "master" git branch renamed to "main".

# entsoeapi v0.9.5.0 (2026-03-05)

## New functionality

-   new area_types were introduced — 2 codes: A01, A02
-   new sub_area_types were introduced — 7 codes: A01–A07
-   new connection_category_types were introduced — 10 codes: E03–E12
-   new customer_types were introduced — 4 codes: A01–A04

## Miscellaneous

-   business_types got new value — C96
-   reason_code_types got new values — B83, B84, B85

# entsoeapi v0.9.4.0 (2026-03-04)

## New functionality

-   The beta version of `sharing_of_frr_capacity()` query has been introduced.

## Miscellaneous

-   The progress bar and the console message formats standardized.
-   New hex sticker has been created and added.
-   A new da-price-spread-vignette.Rmd has been composed and added.
-   CODE_OF_CONDUCT.md has been composed and added.
-   The README.md has been updated.
-   A top-level documentation has been added to the package.
-   Minor under the hood improvements and fixes.

# entsoeapi v0.9.3.0 (2026-03-01)

## New functionality

-   The beta versions of `exchanged_volumes_per_border()`, `hvdc_link_constrains()`, `changes_to_bid_availability()`, `current_balancing_state()`, `balancing_energy_bids()`, `aggregated_balancing_energy_bids()`, `procured_balancing_capacity()`, `allocation_of_cross_zonal_balancing_cap()`, `contracted_reserves()`, `activated_balancing_prices()`, `imbalance_prices()`, `imbalance_volumes()`, `financial_expenses_and_income_for_balancing()`, `fcr_total_capacity()`, `shares_of_fcr_capacity()`, `rr_and_frr_actual_capacity()` and `rr_actual_capacity()` queries have been introduced.

## Miscellaneous

-   The warnings about not attached type names, eic names and definitions have been changed to simple console messages.
-   Minor under the hood improvements and fixes.

# entsoeapi v0.9.2.0 (2026-02-23)

## New functionality

-   The beta versions of `net_positions()`, `congestion_income()` and `allocated_transfer_capacities_3rd_countries()` queries have been introduced.

## Miscellaneous

-   Minor under the hood improvements and fixes.

# entsoeapi v0.9.1.1 (2026-02-21)

## New functionality

-   The beta versions of `implicit_offered_transfer_capacities()`, `explicit_offered_transfer_capacities()`, `continuous_offered_transfer_capacities()`, `flow_based_allocations()`, `auction_revenue()`, `expansion_and_dismantling_project()` and `intraday_cross_border_transfer_limits()` queries have been introduced.

## Miscellaneous

-   Minor under the hood improvements and fixes.

# entsoeapi v0.9.1.0 (2026-02-20)

## New functionality

-   The beta versions of `implicit_offered_transfer_capacities()`, `explicit_offered_transfer_capacities()`, `continuous_offered_transfer_capacities()`, `flow_based_allocations()`, `auction_revenue()`, `expansion_and_dismantling_project()` and `intraday_cross_border_transfer_limits()` queries have been introduced.

## Miscellaneous

-   The built-in type list tables have been updated according to the ENTSO-E code list version 93.
-   The 'ts_in_bidding_zone_domain_mrid' and 'ts_out_bidding_zone_domain_mrid' column related issue have been resolved in `gen_per_gen_unit()` query's tidy results.

# entsoeapi v0.9.0.1 (2026-02-14)

## New functionality

-   None.

## Miscellaneous

-   A minor table compositor engine issue has been resolved.

# entsoeapi v0.9.0.0 (2026-02-14)

## New functionality

-   Pagination functionality has been added to outage queries.
-   Day-ahead market notation has been added for `day_ahead_prices()` query result where more than one day-ahead auction are available; the column named as `ts_classification_sequence_position`.
-   The missing PSR information related issue resolved in `gen_per_gen_unit()` query results.

## Miscellaneous

-   some functions renamed according to the names of the transparency platform:
    -   `redispatching_x_border()` \>\> `redispatching_cross_border()`
    -   `transm_already_allocated_cap()` \>\> `already_allocated_total_capacity()`
    -   `transm_day_ahead_comm_sched()` \>\> `day_ahead_commercial_sched()`
    -   `transm_day_ahead_prices()` \>\> `day_ahead_prices()`
    -   `transm_day_ahead_transf_cap()` \>\> `forecasted_transfer_capacities()`
    -   `transm_total_comm_sched()` \>\> `total_commercial_sched()`
    -   `transm_total_nominated_cap()` \>\> `total_nominated_capacity()`
    -   `transm_x_border_phys_flow()` \>\> `cross_border_physical_flows()`
    -   `gen_day_ahead()` \>\> `gen_day_ahead_forecast()`
-   some functions phased out according to the transparency platform:
    -   `balancing_accepted_aggr_offers()`
    -   `balancing_activated_reserves()`
-   attribute(s) have been modified at some function(s):
    -   `elastic_demands()`
    -   `already_allocated_total_capacity()`

# entsoeapi v0.8.2.0 (2025-11-16)

## New functionality

-   None.

## Miscellaneous

-   The xml extractor engine made more resistant to oddities in XML response of the ENTSO-E API.
-   Under the hood the superseded 'httr' package functions replaced by 'httr2' package functions.
-   The `period_start_update` and `period_end_update` parameters removed from `outages_gen_units()`, `outages_prod_units`, `outages_both()` and `outages_offshore_grid` functions, since those blocked the responses.

# entsoeapi v0.8.1.0 (2025-06-08)

## New functionality

-   New code list documents introduced from ENTSO-E code lists version 92:
    -   `analog_types`
    -   `coordinate_system_types`
    -   `fuel_types`
    -   `market_product_types`
    -   `price_component_types`
    -   `timeframe_types`
    -   `unit_multiplier`

## Miscellaneous

-   The `document_types` code list table renamed to `message_types` to better reflect its content.
-   The ENTSO-E code lists updated from version 36 to version 92. Since it contains codes created recently, the result tables of queries may show new data.
-   If someone would like to use a not yet implemented endpoint/function, then notify us using the <https://github.com/krose/entsoeapi/issues> site.

# entsoeapi v0.8.0.0 (2025-01-12)

## New functionality

-   None.

## Miscellaneous

-   The timeseries data have not converted to table correctly in every case. The 'A03' type curves were processed in 'A01' way. (See further details [here](https://eepublicdownloads.entsoe.eu/clean-documents/EDI/Library/cim_based/Introduction_of_different_Timeseries_possibilities__curvetypes__with_ENTSO-E_electronic_document_v1.4.pdf).) This issue has been fixed.
-   If someone would like to use a not yet implemented endpoint/function, then notify us using the <https://github.com/krose/entsoeapi/issues> site.

# entsoeapi v0.7.3.2 (2025-01-06)

## New functionality

-   None.

## Miscellaneous

-   On some endpoints the `price` field name has changed to `price`.`amount` in the response XML. So we integrated the new field name into our `xml_to_table` engine.
-   If someone would like to use a not yet implemented endpoint/function, then notify us using the <https://github.com/krose/entsoeapi/issues> site.

# entsoeapi v0.7.3.1 (2025-01-02)

## New functionality

-   None.

## Miscellaneous

-   The ...\_eic() functions fixed. (There had been a data source URL related issue.)
-   If someone would like to use a not yet implemented endpoint/function, then notify us using the <https://github.com/krose/entsoeapi/issues> site.

# entsoeapi v0.7.3.0 (2024-12-16)

## New functionality

-   The `balancing_border_cap_limit()` query introduced.
-   The `exchanged_volumes()` query introduced.
-   The `netted_volumes()` query introduced.
-   The `elastic_demands()` query introduced.

## Miscellaneous

-   Further under the hood optimizations implemented.
-   If someone would like to use a not yet implemented one, then notify us using the <https://github.com/krose/entsoeapi/issues> site.

# entsoeapi v0.7.2.0 (2024-10-22)

## New functionality

-   The user's `security_token` value is not displayed on console and in logs any more.
-   The `redispatching_internal()` query introduced.
-   The `redispatching_x_border()` query introduced.
-   The `countertrading()` query introduced.
-   The `costs_of_congestion_management()` query introduced.

## Miscellaneous

-   Under the hood optimizations (e.g. caching) implemented.
-   If someone would like to use a not yet implemented one, then notify us using the <https://github.com/krose/entsoeapi/issues> site.

# entsoeapi v0.7.1.2

## Miscellaneous

-   Some under-the-hood improvements related to code syntax.

# entsoeapi v0.7.1.1

## Miscellaneous

-   The README has adjusted to correspond to new function names.
-   Some under-the-hood improvements related to error handling.

# entsoeapi v0.7.1.0

## New functionality

-   New endpoints/functions:
    -   GENERATION
        -   the gen_installed_capacity_per_pu (14.1.B) introduced
        -   the gen_storage_mean_filling_rate (16.1.D) introduced

## Miscellaneous

-   Renamed endpoint/function:
    -   GENERATION
        -   the gen_installed_capacity (14.1.A) renamed to gen_installed_capacity_per_pt (14.1.A)

# entsoeapi v0.7.0.0

Earlier versions contain experimental versions of API query functions, with very few standardisation.

## New functionality

-   First standardised submission.
-   Already available ENTSO-E API endpoints:
    -   BALANCING
        -   balancing_accepted_aggr_offers (17.1.D)
        -   balancing_activated_reserves (17.1.E)
    -   GENERATION
        -   gen_day_ahead (14.1.C)
        -   gen_installed_capacity (14.1.A)
        -   gen_per_gen_unit (16.1.A)
        -   gen_per_prod_type (16.1.B&C)
        -   gen_wind_solar_forecasts (14.1.D)
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
