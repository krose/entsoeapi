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
#> ℹ pulling A_eicCodes.csv file from cache

dplyr::glimpse(eic_substation)
#> Rows: 2,847
#> Columns: 11
#> $ EicCode                         <chr> "11A0-0000-0001-W", "11A0-0000-0002-T", "11A0-0000-0004-N", "11A0-0000-0005-K"…
#> $ EicDisplayName                  <chr> "USWKSDON00000000", "USWKLAGE00000000", "USWKHAND00000000", "USWKELWE00000000"…
#> $ EicLongName                     <chr> "USWKSDON00000000", "USWKLAGE00000000", "USWKHAND00000000", "USWKELWE00000000"…
#> $ EicParent                       <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ EicResponsibleParty             <chr> "10XDE-EON-NETZ-C", "10XDE-EON-NETZ-C", "10XDE-EON-NETZ-C", "10XDE-EON-NETZ-C"…
#> $ EicStatus                       <chr> "Active", "Active", "Active", "Active", "Active", "Active", "Active", "Active"…
#> $ MarketParticipantPostalCode     <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ MarketParticipantIsoCountryCode <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ MarketParticipantVatCode        <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ EicTypeFunctionList             <chr> "Substation", "Substation", "Substation", "Substation", "Substation", "Substat…
#> $ type                            <chr> "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A"…
```
