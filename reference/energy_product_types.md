# List energy product types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List energy product types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
energy_product_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 9 rows
and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::energy_product_types)
#> Rows: 9
#> Columns: 3
#> $ code        <chr> "8716867000016", "8716867000023", "8716867000030", "8716867000047", "8716867000115", "871686700012…
#> $ title       <chr> "Active power", "Reactive power", "Active energy", "Reactive energy", "Capacitive reactive power",…
#> $ description <chr> "The product of voltage and the in-phase component of alternating current measured in units of wat…
```
