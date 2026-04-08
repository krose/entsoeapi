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
eic_area <- entsoeapi::area_eic()
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling Y_eicCodes.csv file from cache

dplyr::glimpse(eic_area)
#> Rows: 1,791
#> Columns: 11
#> $ eic_code                            <chr> "10Y1001A1001A008", "10Y1001A1001A016", "10Y1001A1001A39I", "10Y1001A1001A…
#> $ eic_display_name                    <chr> "BE-FR-NL-MARKET", "GB-NI", "EE", "CB-ES", "SE1", "SE2", "SE3", "SE4", "NO…
#> $ eic_long_name                       <chr> "Market coupling area Belgium - France - Netherlands", "Northern Ireland",…
#> $ eic_parent                          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ eic_responsible_party               <chr> NA, NA, NA, "10XES-REE------E", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "1…
#> $ eic_status                          <chr> "Active", "Active", "Active", "Active", "Active", "Active", "Active", "Act…
#> $ market_participant_postal_code      <chr> NA, "BT6 9RT", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ market_participant_iso_country_code <chr> NA, "GB", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ market_participant_vat_code         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ eic_type_function_list              <chr> "Scheduling Area", "Control Area,LFC Area,LFC Block,Scheduling Area", "Bid…
#> $ type                                <chr> "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y",…
```
