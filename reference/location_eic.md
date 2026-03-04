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

str(eic_location)
#> tibble [1,036 × 11] (S3: tbl_df/tbl/data.frame)
#>  $ EicCode                        : chr [1:1036] "10V1001C--002139" "10V1001C--00212B" "10V000000000001T" "10V000000000002R" ...
#>  $ EicDisplayName                 : chr [1:1036] "TSCNET_3_FB" "TSCNET_4_OPC_STA" "ECP_TP_TEST_EP" "ECP_TP_PROD_EP" ...
#>  $ EicLongName                    : chr [1:1036] "TSCNET Services GmbH" "TSCNET Services GmbH" "ECP Transparency Platform Test EndPoint" "ECP Transparency Platform Production Endpoint" ...
#>  $ EicParent                      : chr [1:1036] NA NA NA NA ...
#>  $ EicResponsibleParty            : chr [1:1036] "10X1001C--00003T" "10X1001C--00003T" "10X1001A1001A450" "10X1001A1001A450" ...
#>  $ EicStatus                      : chr [1:1036] "Active" "Active" "Active" "Active" ...
#>  $ MarketParticipantPostalCode    : chr [1:1036] NA NA "1000" "1000" ...
#>  $ MarketParticipantIsoCountryCode: chr [1:1036] NA NA "BE" "BE" ...
#>  $ MarketParticipantVatCode       : logi [1:1036] NA NA NA NA NA NA ...
#>  $ EicTypeFunctionList            : chr [1:1036] "Endpoint" "Endpoint" "Location" "Location" ...
#>  $ type                           : chr [1:1036] "V" "V" "V" "V" ...
```
