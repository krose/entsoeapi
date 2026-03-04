# Get Accounting Point Z Energy Identification Codes

This function downloads approved accounting point Z energy
identification codes from this site:
https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes
An entity under balance responsibility where balance supplier change can
take place and for which commercial business processes are defined.

## Usage

``` r
accounting_point_eic()
```

## Value

A tibble of accordingly filtered EIC codes, which contains such columns
as `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
`EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
`MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
`EicTypeFunctionList` and `type`.

## Examples

``` r
eic_accounting_point <- entsoeapi::accounting_point_eic()
#> ℹ downloading Z_eicCodes.csv file ...

str(eic_accounting_point)
#> tibble [2,508 × 11] (S3: tbl_df/tbl/data.frame)
#>  $ EicCode                        : chr [1:2508] "10Z-1001-10010AI" "10Z1001A1001A01T" "10Z1001A1001A02R" "10Z1001A1001A03P" ...
#>  $ EicDisplayName                 : chr [1:2508] "M_KOM-KOSB" "M_UROSEV_SKOPJE" "M_PRIZRE_FIERZA" "M_PEC_RIBAREVIN" ...
#>  $ EicLongName                    : chr [1:2508] "Tie Line Koman-KosovoB" "Urosevac_Skopje" "Prizren_Fierza" "Pec_Ribrevina" ...
#>  $ EicParent                      : logi [1:2508] NA NA NA NA NA NA ...
#>  $ EicResponsibleParty            : chr [1:2508] NA NA NA NA ...
#>  $ EicStatus                      : chr [1:2508] "Active" "Active" "Active" "Active" ...
#>  $ MarketParticipantPostalCode    : logi [1:2508] NA NA NA NA NA NA ...
#>  $ MarketParticipantIsoCountryCode: chr [1:2508] NA NA NA NA ...
#>  $ MarketParticipantVatCode       : logi [1:2508] NA NA NA NA NA NA ...
#>  $ EicTypeFunctionList            : chr [1:2508] "Accounting Point" "Accounting Point" "Accounting Point" "Accounting Point" ...
#>  $ type                           : chr [1:2508] "Z" "Z" "Z" "Z" ...
```
