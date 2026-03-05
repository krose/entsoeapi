# List unit symbol types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List unit symbol types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
unit_symbol_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 11
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::unit_symbol_types)
#> Rows: 11
#> Columns: 3
#> $ code        <chr> "AMP", "C62", "DD", "GKH", "HTZ", "KVT", "MAR", "MAW", "MVA", "OHM", "P1"
#> $ title       <chr> "Ampere", "One", "degree (unit of angle)", "grams per kilowatt hour", "Hertz", "kV", "MVAr", "MW",…
#> $ description <chr> "The unit of electrical current in the International system of Units (SI) equivalent to one Coulom…
```
