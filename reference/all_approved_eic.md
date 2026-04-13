# Get all Approved Energy Identification Codes

This function downloads all approved energy identification codes from
this site:
https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes
Further details are under:
https://www.entsoe.eu/data/energy-identification-codes-eic/#eic-documentation

## Usage

``` r
all_approved_eic()
```

## Value

A tibble of accordingly filtered EIC codes, which contains such columns
as `eic_code`, `eic_display_name`, `eic_long_name`, `eic_parent`,
`eic_responsible_party`, `eic_status`, `market_participant_postal_code`,
`market_participant_iso_country_code`, `market_participant_vat_code`,
`eic_type_function_list` and `type`.

## Examples

``` r
eic_all <- entsoeapi::all_approved_eic()
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading X_eicCodes.csv file ...
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling Y_eicCodes.csv file from cache
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling Z_eicCodes.csv file from cache
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading T_eicCodes.csv file ...
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading V_eicCodes.csv file ...
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading W_eicCodes.csv file ...
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading A_eicCodes.csv file ...

dplyr::glimpse(eic_all)
#> Rows: 70,782
#> Columns: 11
#> $ eic_code                            <chr> "26X00000001515-Y", "26X00000105734-O", "26X00000105740-W", "10X1001A1001A…
#> $ eic_display_name                    <chr> "LA_220", "IT-GEO____SPA", "IT-BETA_ENERGYS", "ELIA", "ENERGINET-DK", "FIN…
#> $ eic_long_name                       <chr> "La 220 S.p.A.", "GEO", "BETA ENERGY", "Elia Transmission Belgium", "Energ…
#> $ eic_parent                          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ eic_responsible_party               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ eic_status                          <chr> "Active", "Active", "Active", "Active", "Active", "Active", "Active", "Act…
#> $ market_participant_postal_code      <chr> NA, NA, NA, "1000", "7000", "FI-00101", "1031", "00138", "53123", "NL-6812…
#> $ market_participant_iso_country_code <chr> NA, NA, NA, "BE", NA, "FI", "HU", "IT", NA, "NL", NA, "EE", "SE", "BE", NA…
#> $ market_participant_vat_code         <chr> "IT02633180985", "IT03961820960", "IT07672150963", "BE0731852231", "DK2898…
#> $ eic_type_function_list              <chr> "Trade Responsible Party", "Trade Responsible Party", "Trade Responsible P…
#> $ type                                <chr> "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",…
```
