# Get Resource Object W Energy Identification Codes

This function downloads approved resource object W energy identification
codes from this site:
https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes
A resource that can either produce or consume energy and that is
reported in a schedule.

## Usage

``` r
resource_object_eic()
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
eic_resource_object <- entsoeapi::resource_object_eic()

dplyr::glimpse(eic_resource_object)
}
```
