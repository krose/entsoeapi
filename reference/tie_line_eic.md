# Get Tie Line T Energy Identification Codes

This function downloads approved tie line T energy identification codes
from this site:
https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes
It covers a transmission line that connects different areas excluding
HVDC interconnectors.

## Usage

``` r
tie_line_eic()
```

## Value

A tibble of accordingly filtered EIC codes, which contains such columns
as `eic_code`, `eic_display_name`, `eic_long_name`, `eic_parent`,
`eic_responsible_party`, `eic_status`, `market_participant_postal_code`,
`market_participant_iso_country_code`, `market_participant_vat_code`,
`eic_type_function_list` and `type`.

## Examples

``` r
if (FALSE) { # there_is_provider()
eic_tie_line <- entsoeapi::tie_line_eic()

dplyr::glimpse(eic_tie_line)
}
```
