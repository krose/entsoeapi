# Get Area Y Energy Identification Codes

This function downloads approved area Y energy identification codes from
this site:
https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes

## Usage

``` r
area_eic()
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
eic_area <- entsoeapi::area_eic()

dplyr::glimpse(eic_area)
}
```
