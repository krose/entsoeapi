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
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading Z_eicCodes.csv file ...

dplyr::glimpse(eic_accounting_point)
#> Rows: 2,515
#> Columns: 11
#> $ eic_code                            <chr> "10Z-1001-10010AI", "10Z1001A1001A01T", "10Z1001A1001A02R", "10Z1001A1001A…
#> $ eic_display_name                    <chr> "M_KOM-KOSB", "M_UROSEV_SKOPJE", "M_PRIZRE_FIERZA", "M_PEC_RIBAREVIN", "M_…
#> $ eic_long_name                       <chr> "Tie Line Koman-KosovoB", "Urosevac_Skopje", "Prizren_Fierza", "Pec_Ribrev…
#> $ eic_parent                          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ eic_responsible_party               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ eic_status                          <chr> "Active", "Active", "Active", "Active", "Active", "Active", "Active", "Act…
#> $ market_participant_postal_code      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ market_participant_iso_country_code <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ market_participant_vat_code         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ eic_type_function_list              <chr> "Accounting Point", "Accounting Point", "Accounting Point", "Accounting Po…
#> $ type                                <chr> "Z", "Z", "Z", "Z", "Z", "Z", "Z", "Z", "Z", "Z", "Z", "Z", "Z", "Z", "Z",…
```
