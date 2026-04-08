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
eic_tie_line <- entsoeapi::tie_line_eic()
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling T_eicCodes.csv file from cache

dplyr::glimpse(eic_tie_line)
#> Rows: 12,987
#> Columns: 11
#> $ eic_code                            <chr> "10T-1001-10010AS", "10T1001A1001A012", "10T1001A1001A020", "10T1001A1001A…
#> $ eic_display_name                    <chr> "L_KOM-KOSB", "L_UROSEV_SKOPJE", "L_PRIZRE_FIERZA", "L_PEC_RIBAREVIN", "L_…
#> $ eic_long_name                       <chr> "Tie Line Koman-KosovoB", "Urosevac_Skopje", "Prizren_Fierza", "Pec_Ribrev…
#> $ eic_parent                          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ eic_responsible_party               <chr> "10XAL-KESH-----J", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ eic_status                          <chr> "Active", "Active", "Active", "Active", "Active", "Active", "Active", "Act…
#> $ market_participant_postal_code      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ market_participant_iso_country_code <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ market_participant_vat_code         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ eic_type_function_list              <chr> "Tieline", "Tieline", "Tieline", "Tieline", "Tieline", "Tieline", "Tieline…
#> $ type                                <chr> "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T",…
```
