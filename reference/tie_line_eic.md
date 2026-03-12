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

dplyr::glimpse(eic_tie_line)
#> Rows: 12,961
#> Columns: 11
#> $ EicCode                         <chr> "10T-1001-10010AS", "10T1001A1001A012", "10T1001A1001A020", "10T1001A1001A03Z"…
#> $ EicDisplayName                  <chr> "L_KOM-KOSB", "L_UROSEV_SKOPJE", "L_PRIZRE_FIERZA", "L_PEC_RIBAREVIN", "L_NIS_…
#> $ EicLongName                     <chr> "Tie Line Koman-KosovoB", "Urosevac_Skopje", "Prizren_Fierza", "Pec_Ribrevina"…
#> $ EicParent                       <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ EicResponsibleParty             <chr> "10XAL-KESH-----J", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ EicStatus                       <chr> "Active", "Active", "Active", "Active", "Active", "Active", "Active", "Active"…
#> $ MarketParticipantPostalCode     <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ MarketParticipantIsoCountryCode <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ MarketParticipantVatCode        <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ EicTypeFunctionList             <chr> "Tieline", "Tieline", "Tieline", "Tieline", "Tieline", "Tieline", "Tieline", "…
#> $ type                            <chr> "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T"…
```
