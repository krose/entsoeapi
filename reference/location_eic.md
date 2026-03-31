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
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling V_eicCodes.csv file from cache

dplyr::glimpse(eic_location)
#> Rows: 1,048
#> Columns: 11
#> $ eic_code                            <chr> "10V1001C--002139", "10V1001C--00212B", "10V000000000001T", "10V0000000000…
#> $ eic_display_name                    <chr> "TSCNET_3_FB", "TSCNET_4_OPC_STA", "ECP_TP_TEST_EP", "ECP_TP_PROD_EP", "EC…
#> $ eic_long_name                       <chr> "TSCNET Services GmbH", "TSCNET Services GmbH", "ECP Transparency Platform…
#> $ eic_parent                          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ eic_responsible_party               <chr> "10X1001C--00003T", "10X1001C--00003T", "10X1001A1001A450", "10X1001A1001A…
#> $ eic_status                          <chr> "Active", "Active", "Active", "Active", "Active", "Active", "Active", "Act…
#> $ market_participant_postal_code      <chr> NA, NA, "1000", "1000", "1000", "1023", "1023", "1000", NA, NA, NA, NA, NA…
#> $ market_participant_iso_country_code <chr> NA, NA, "BE", "BE", "BE", "AL", "AL", "BE", NA, NA, "DE", NA, NA, NA, NA, …
#> $ market_participant_vat_code         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ eic_type_function_list              <chr> "Endpoint", "Endpoint", "Location", "Location", "Location", "Location", "L…
#> $ type                                <chr> "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", "V",…
```
