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

dplyr::glimpse(eic_resource_object)
#> Rows: 34,233
#> Columns: 11
#> $ EicCode                         <chr> "26WAI-PONTE10003", "26WIMPI-00680951", "26WIMPI-0073522R", "26WIMPI-0077109F"…
#> $ EicDisplayName                  <chr> "0_868", "IM-0068095", "IM-0073522", "IM-0077109", "IM-0080544", "IM-0083490",…
#> $ EicLongName                     <chr> "IMAI_PONTE_1", "IM_0068095", "IM_0073522", "IM_0077109", "IM_0080544", "IM_00…
#> $ EicParent                       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ EicResponsibleParty             <chr> NA, NA, NA, NA, NA, NA, "11XGKM-P-------6", "11XGKM-P-------6", "11XGKM-P-----…
#> $ EicStatus                       <chr> "Active", "Active", "Active", "Active", "Active", "Active", "Active", "Active"…
#> $ MarketParticipantPostalCode     <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ MarketParticipantIsoCountryCode <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ MarketParticipantVatCode        <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ EicTypeFunctionList             <chr> "Production Unit", "Production Unit", "Production Unit", "Production Unit", "P…
#> $ type                            <chr> "W", "W", "W", "W", "W", "W", "W", "W", "W", "W", "W", "W", "W", "W", "W", "W"…
```
