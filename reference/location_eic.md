# Get Location V Energy Identification Codes

This function downloads approved location V energy identification codes
from this site:
https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes
It covers an endpoint, or an IT-system.

## Usage

``` r
location_eic()
```

## Value

A tibble of accordingly filtered EIC codes, which contains such columns
as `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
`EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
`MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
`EicTypeFunctionList` and `type`.

## Examples

``` r
eic_location <- entsoeapi::location_eic()
#> ℹ pulling V_eicCodes.csv file from cache

dplyr::glimpse(eic_location)
#> Rows: 1,040
#> Columns: 11
#> $ EicCode                         <chr> "10V1001C--002139", "10V1001C--00212B", "10V000000000001T", "10V000000000002R"…
#> $ EicDisplayName                  <chr> "TSCNET_3_FB", "TSCNET_4_OPC_STA", "ECP_TP_TEST_EP", "ECP_TP_PROD_EP", "ECP_TP…
#> $ EicLongName                     <chr> "TSCNET Services GmbH", "TSCNET Services GmbH", "ECP Transparency Platform Tes…
#> $ EicParent                       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ EicResponsibleParty             <chr> "10X1001C--00003T", "10X1001C--00003T", "10X1001A1001A450", "10X1001A1001A450"…
#> $ EicStatus                       <chr> "Active", "Active", "Active", "Active", "Active", "Active", "Active", "Active"…
#> $ MarketParticipantPostalCode     <chr> NA, NA, "1000", "1000", "1000", "1023", "1023", "1000", NA, NA, NA, NA, NA, NA…
#> $ MarketParticipantIsoCountryCode <chr> NA, NA, "BE", "BE", "BE", "AL", "AL", "BE", NA, NA, "DE", NA, NA, NA, NA, NA, …
#> $ MarketParticipantVatCode        <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ EicTypeFunctionList             <chr> "Endpoint", "Endpoint", "Location", "Location", "Location", "Location", "Locat…
#> $ type                            <chr> "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", "V"…
```
