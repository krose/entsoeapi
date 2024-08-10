# entsoeapi (0.7.0.0 version)

* First standardized submission.
* Already available ENTSO-E API endpoints:
  - BALANCING
    - balancing_accepted_aggr_offers
    - balancing_activated_reserves
  - GENERATION
    - gen_day_ahead
    - gen_installed_capacity
    - gen_per_gen_unit
    - gen_per_prod_type
    - gen_wind_solar_forecasts
  - LOAD
    - load_actual_total
    - load_dayahead_total_forecast
    - load_monthahead_total_forecast
    - load_weekahead_total_forecast
    - load_yearahead_forecast_margin
    - load_yearahead_total_forecast
  - UNAVAILABILITY
    - outages_gen_units
    - outages_prod_units
    - outages_both
    - outages_cons_units
    - outages_fallbacks
    - outages_offshore_grid
    - outages_transmission_grid
  - TRANSMISSION
    - transm_already_allocated_cap
    - transm_day_ahead_comm_sched
    - transm_day_ahead_prices
    - transm_forecasted_cap
    - transm_total_comm_sched
    - transm_total_nominated_cap
    - transm_x_border_phys_flow

If someone would like to use a not yet implemented one, then notify us using the https://github.com/krose/entsoeapi/issues site.

---

# earlier versions
Those contain experimental versions of API query functions. with very few standardizations.
