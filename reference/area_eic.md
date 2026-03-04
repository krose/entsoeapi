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

str(eic_area)
#> tibble [1,784 × 11] (S3: tbl_df/tbl/data.frame)
#>  $ EicCode                        : chr [1:1784] "10Y1001A1001A008" "10Y1001A1001A016" "10Y1001A1001A39I" "10Y1001A1001A42T" ...
#>  $ EicDisplayName                 : chr [1:1784] "BE-FR-NL-MARKET" "GB-NI" "EE" "CB-ES" ...
#>  $ EicLongName                    : chr [1:1784] "Market coupling area Belgium - France - Netherlands" "Northern Ireland" "Estonia" "CB Spain" ...
#>  $ EicParent                      : chr [1:1784] NA NA NA NA ...
#>  $ EicResponsibleParty            : chr [1:1784] NA NA NA "10XES-REE------E" ...
#>  $ EicStatus                      : chr [1:1784] "Active" "Active" "Active" "Active" ...
#>  $ MarketParticipantPostalCode    : chr [1:1784] NA "BT6 9RT" NA NA ...
#>  $ MarketParticipantIsoCountryCode: chr [1:1784] NA "GB" NA NA ...
#>  $ MarketParticipantVatCode       : logi [1:1784] NA NA NA NA NA NA ...
#>  $ EicTypeFunctionList            : chr [1:1784] "Scheduling Area" "Control Area,LFC Area,LFC Block,Scheduling Area" "Bidding Zone,Control Area,LFC Area,Member State,Scheduling Area" "Control Block" ...
#>  $ type                           : chr [1:1784] "Y" "Y" "Y" "Y" ...
```
