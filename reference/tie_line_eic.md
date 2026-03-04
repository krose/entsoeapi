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
as `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
`EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
`MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
`EicTypeFunctionList` and `type`.

## Examples

``` r
eic_tie_line <- entsoeapi::tie_line_eic()
#> ℹ pulling T_eicCodes.csv file from cache

str(eic_tie_line)
#> tibble [12,936 × 11] (S3: tbl_df/tbl/data.frame)
#>  $ EicCode                        : chr [1:12936] "10T-1001-10010AS" "10T1001A1001A012" "10T1001A1001A020" "10T1001A1001A03Z" ...
#>  $ EicDisplayName                 : chr [1:12936] "L_KOM-KOSB" "L_UROSEV_SKOPJE" "L_PRIZRE_FIERZA" "L_PEC_RIBAREVIN" ...
#>  $ EicLongName                    : chr [1:12936] "Tie Line Koman-KosovoB" "Urosevac_Skopje" "Prizren_Fierza" "Pec_Ribrevina" ...
#>  $ EicParent                      : logi [1:12936] NA NA NA NA NA NA ...
#>  $ EicResponsibleParty            : chr [1:12936] "10XAL-KESH-----J" NA NA NA ...
#>  $ EicStatus                      : chr [1:12936] "Active" "Active" "Active" "Active" ...
#>  $ MarketParticipantPostalCode    : int [1:12936] NA NA NA NA NA NA NA NA NA NA ...
#>  $ MarketParticipantIsoCountryCode: chr [1:12936] NA NA NA NA ...
#>  $ MarketParticipantVatCode       : logi [1:12936] NA NA NA NA NA NA ...
#>  $ EicTypeFunctionList            : chr [1:12936] "Tieline" "Tieline" "Tieline" "Tieline" ...
#>  $ type                           : chr [1:12936] "T" "T" "T" "T" ...
```
