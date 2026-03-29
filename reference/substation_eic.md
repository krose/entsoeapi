# Get Substation A Energy Identification Codes

This function downloads all approved substation A energy identification
codes from this site:
https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes
Substation is a facility equipment that steps up or steps down the
voltage in utility power lines. Voltage is stepped up where power is
sent through long distance transmission lines, and stepped down where
the power is to enter the local distribution lines. They can be
classified as normal outside substation, armoured substation and
underground substation.

## Usage

``` r
substation_eic()
```

## Value

A tibble of accordingly filtered EIC codes, which contains such columns
as `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
`EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
`MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
`EicTypeFunctionList` and `type`.

## Examples

``` r
eic_substation <- entsoeapi::substation_eic()
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling A_eicCodes.csv file from cache

dplyr::glimpse(eic_substation)
#> Rows: 2,848
#> Columns: 11
#> $ eic_code                            <chr> "11A0-0000-0001-W", "11A0-0000-0002-T", "11A0-0000-0004-N", "11A0-0000-000…
#> $ eic_display_name                    <chr> "USWKSDON00000000", "USWKLAGE00000000", "USWKHAND00000000", "USWKELWE00000…
#> $ eic_long_name                       <chr> "USWKSDON00000000", "USWKLAGE00000000", "USWKHAND00000000", "USWKELWE00000…
#> $ eic_parent                          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ eic_responsible_party               <chr> "10XDE-EON-NETZ-C", "10XDE-EON-NETZ-C", "10XDE-EON-NETZ-C", "10XDE-EON-NET…
#> $ eic_status                          <chr> "Active", "Active", "Active", "Active", "Active", "Active", "Active", "Act…
#> $ market_participant_postal_code      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ market_participant_iso_country_code <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ market_participant_vat_code         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ eic_type_function_list              <chr> "Substation", "Substation", "Substation", "Substation", "Substation", "Sub…
#> $ type                                <chr> "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",…
```
