# entsoeapi v0.8.0.0 (2025-01-12)

## New functionality

-   None.

## Miscellaneous

-   The timeseries data have not converted to table correctly in every case. The 'A03' type curves were processed in 'A01' way. (See further details [here](https://eepublicdownloads.entsoe.eu/clean-documents/EDI/Library/cim_based/Introduction_of_different_Timeseries_possibilities__curvetypes__with_ENTSO-E_electronic_document_v1.4.pdf).)
    This issue has been fixed.
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

-   The ..._eic() functions fixed. (There had been a data source URL related issue.)
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
