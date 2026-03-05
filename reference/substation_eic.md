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

str(eic_substation)
#> tibble [2,847 × 11] (S3: tbl_df/tbl/data.frame)
#>  $ EicCode                        : chr [1:2847] "11A0-0000-0001-W" "11A0-0000-0002-T" "11A0-0000-0004-N" "11A0-0000-0005-K" ...
#>  $ EicDisplayName                 : chr [1:2847] "USWKSDON00000000" "USWKLAGE00000000" "USWKHAND00000000" "USWKELWE00000000" ...
#>  $ EicLongName                    : chr [1:2847] "USWKSDON00000000" "USWKLAGE00000000" "USWKHAND00000000" "USWKELWE00000000" ...
#>  $ EicParent                      : logi [1:2847] NA NA NA NA NA NA ...
#>  $ EicResponsibleParty            : chr [1:2847] "10XDE-EON-NETZ-C" "10XDE-EON-NETZ-C" "10XDE-EON-NETZ-C" "10XDE-EON-NETZ-C" ...
#>  $ EicStatus                      : chr [1:2847] "Active" "Active" "Active" "Active" ...
#>  $ MarketParticipantPostalCode    : logi [1:2847] NA NA NA NA NA NA ...
#>  $ MarketParticipantIsoCountryCode: logi [1:2847] NA NA NA NA NA NA ...
#>  $ MarketParticipantVatCode       : logi [1:2847] NA NA NA NA NA NA ...
#>  $ EicTypeFunctionList            : chr [1:2847] "Substation" "Substation" "Substation" "Substation" ...
#>  $ type                           : chr [1:2847] "A" "A" "A" "A" ...
```
