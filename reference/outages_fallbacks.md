# Get Fall-Back Procedures. (IFs IN 7.2, mFRR 3.11, aFRR 3.10)

It publishes of application of fall back procedures by participants in
European platforms as a result of disconnection of DSO from the European
platform, unavailability of European platform itself (planned or
unplanned outage) or the situation where the algorithm used on the
platform fails or does not find solution.

## Usage

``` r
outages_fallbacks(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  process_type = "A63",
  event_nature = "A53",
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the bidding zone/ control area

- period_start:

  the starting date of the in-scope period in POSIXct or YYYY-MM-DD
  HH:MM:SS format One year range limit applies

- period_end:

  the ending date of the outage in-scope period in POSIXct or YYYY-MM-DD
  HH:MM:SS format One year range limit applies

- process_type:

  "A47" = mFRR "A51" = aFRR "A63" = imbalance netting defaults to "A63"

- event_nature:

  "C47" = Disconnection, "A53" = Planned maintenance, "A54": Unplanned
  outage, "A83" = Auction cancellation (used in case no solution found
  or algorithm failure); Defaults to "A53".

- tidy_output:

  Defaults to TRUE. flatten nested tables

- security_token:

  Security token for ENTSO-E transparency platform

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with the queried data.

## See also

Other outage endpoints:
[`outages_both()`](https://krose.github.io/entsoeapi/reference/outages_both.md),
[`outages_cons_units()`](https://krose.github.io/entsoeapi/reference/outages_cons_units.md),
[`outages_gen_units()`](https://krose.github.io/entsoeapi/reference/outages_gen_units.md),
[`outages_offshore_grid()`](https://krose.github.io/entsoeapi/reference/outages_offshore_grid.md),
[`outages_prod_units()`](https://krose.github.io/entsoeapi/reference/outages_prod_units.md),
[`outages_transmission_grid()`](https://krose.github.io/entsoeapi/reference/outages_transmission_grid.md)

## Examples

``` r
if (FALSE) { # there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
df <- entsoeapi::outages_fallbacks(
  eic = "10YBE----------2",
  period_start = lubridate::ymd(x = "2023-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2024-01-01", tz = "CET"),
  process_type = "A51",
  event_nature = "C47")

dplyr::glimpse(df)
}
```
