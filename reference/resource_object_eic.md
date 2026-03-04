# Get Resource Object W Energy Identification Codes

This function downloads approved resource object W energy identification
codes from this site:
https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes
A resource that can either produce or consume energy and that is
reported in a schedule.

## Usage

``` r
resource_object_eic()
```

## Value

A tibble of accordingly filtered EIC codes, which contains such columns
as `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
`EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
`MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
`EicTypeFunctionList` and `type`.

## Examples

``` r
eic_resource_object <- entsoeapi::resource_object_eic()
#> ℹ pulling W_eicCodes.csv file from cache

str(eic_resource_object)
#> tibble [34,222 × 11] (S3: tbl_df/tbl/data.frame)
#>  $ EicCode                        : chr [1:34222] "26WAI-PONTE10003" "26WIMPI-00680951" "26WIMPI-0073522R" "26WIMPI-0077109F" ...
#>  $ EicDisplayName                 : chr [1:34222] "0_868" "IM-0068095" "IM-0073522" "IM-0077109" ...
#>  $ EicLongName                    : chr [1:34222] "IMAI_PONTE_1" "IM_0068095" "IM_0073522" "IM_0077109" ...
#>  $ EicParent                      : chr [1:34222] NA NA NA NA ...
#>  $ EicResponsibleParty            : chr [1:34222] NA NA NA NA ...
#>  $ EicStatus                      : chr [1:34222] "Active" "Active" "Active" "Active" ...
#>  $ MarketParticipantPostalCode    : logi [1:34222] NA NA NA NA NA NA ...
#>  $ MarketParticipantIsoCountryCode: logi [1:34222] NA NA NA NA NA NA ...
#>  $ MarketParticipantVatCode       : logi [1:34222] NA NA NA NA NA NA ...
#>  $ EicTypeFunctionList            : chr [1:34222] "Production Unit" "Production Unit" "Production Unit" "Production Unit" ...
#>  $ type                           : chr [1:34222] "W" "W" "W" "W" ...
```
