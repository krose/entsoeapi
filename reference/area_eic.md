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
as `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
`EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
`MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
`EicTypeFunctionList` and `type`.

## Examples

``` r
eic_area <- entsoeapi::area_eic()
#> ℹ pulling Y_eicCodes.csv file from cache

dplyr::glimpse(eic_area)
#> Rows: 1,786
#> Columns: 11
#> $ EicCode                         <chr> "10Y1001A1001A008", "10Y1001A1001A016", "10Y1001A1001A39I", "10Y1001A1001A42T"…
#> $ EicDisplayName                  <chr> "BE-FR-NL-MARKET", "GB-NI", "EE", "CB-ES", "SE1", "SE2", "SE3", "SE4", "NO5", …
#> $ EicLongName                     <chr> "Market coupling area Belgium - France - Netherlands", "Northern Ireland", "Es…
#> $ EicParent                       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ EicResponsibleParty             <chr> NA, NA, NA, "10XES-REE------E", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "10XPT…
#> $ EicStatus                       <chr> "Active", "Active", "Active", "Active", "Active", "Active", "Active", "Active"…
#> $ MarketParticipantPostalCode     <chr> NA, "BT6 9RT", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ MarketParticipantIsoCountryCode <chr> NA, "GB", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ MarketParticipantVatCode        <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ EicTypeFunctionList             <chr> "Scheduling Area", "Control Area,LFC Area,LFC Block,Scheduling Area", "Bidding…
#> $ type                            <chr> "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y"…
```
