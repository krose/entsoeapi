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

dplyr::glimpse(eic_party)
#> Rows: 14,809
#> Columns: 11
#> $ EicCode                         <chr> "26X00000001515-Y", "26X00000105734-O", "26X00000105740-W", "10X1001A1001A094"…
#> $ EicDisplayName                  <chr> "LA_220", "IT-GEO____SPA", "IT-BETA_ENERGYS", "ELIA", "ENERGINET-DK", "FINGRID…
#> $ EicLongName                     <chr> "La 220 S.p.A.", "GEO", "BETA ENERGY", "Elia Transmission Belgium", "Energinet…
#> $ EicParent                       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ EicResponsibleParty             <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ EicStatus                       <chr> "Active", "Active", "Active", "Active", "Active", "Active", "Active", "Active"…
#> $ MarketParticipantPostalCode     <chr> NA, NA, NA, "1000", "7000", "FI-00101", "1031", "00138", "53123", "NL-6812 AR"…
#> $ MarketParticipantIsoCountryCode <chr> NA, NA, NA, "BE", NA, "FI", "HU", "IT", NA, "NL", NA, "EE", "SE", "BE", NA, "A…
#> $ MarketParticipantVatCode        <chr> "IT02633180985", "IT03961820960", "IT07672150963", "BE0731852231", "DK28980671…
#> $ EicTypeFunctionList             <chr> "Trade Responsible Party", "Trade Responsible Party", "Trade Responsible Party…
#> $ type                            <chr> "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X"…
```
