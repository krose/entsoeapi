# Get Party_X Energy Identification Codes

This function downloads approved party X energy identification codes
from this site:
https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes
It covers market participants.

## Usage

``` r
party_eic()
```

## Value

A tibble of accordingly filtered EIC codes, which contains such columns
as `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
`EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
`MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
`EicTypeFunctionList` and `type`.

## Examples

``` r
eic_party <- entsoeapi::party_eic()
#> ℹ pulling X_eicCodes.csv file from cache

str(eic_party)
#> tibble [14,797 × 11] (S3: tbl_df/tbl/data.frame)
#>  $ EicCode                        : chr [1:14797] "26X00000001515-Y" "26X00000105734-O" "26X00000105740-W" "10X1001A1001A094" ...
#>  $ EicDisplayName                 : chr [1:14797] "LA_220" "IT-GEO____SPA" "IT-BETA_ENERGYS" "ELIA" ...
#>  $ EicLongName                    : chr [1:14797] "La 220 S.p.A." "GEO" "BETA ENERGY" "Elia Transmission Belgium" ...
#>  $ EicParent                      : chr [1:14797] NA NA NA NA ...
#>  $ EicResponsibleParty            : logi [1:14797] NA NA NA NA NA NA ...
#>  $ EicStatus                      : chr [1:14797] "Active" "Active" "Active" "Active" ...
#>  $ MarketParticipantPostalCode    : chr [1:14797] NA NA NA "1000" ...
#>  $ MarketParticipantIsoCountryCode: chr [1:14797] NA NA NA "BE" ...
#>  $ MarketParticipantVatCode       : chr [1:14797] "IT02633180985" "IT03961820960" "IT07672150963" "BE0731852231" ...
#>  $ EicTypeFunctionList            : chr [1:14797] "Trade Responsible Party" "Trade Responsible Party" "Trade Responsible Party" "System Operator" ...
#>  $ type                           : chr [1:14797] "X" "X" "X" "X" ...
```
