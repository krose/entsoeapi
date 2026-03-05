# List fuel types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List fuel types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
fuel_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 55
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::fuel_types)
#> Rows: 55
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", "A12", "A13", "A14", …
#> $ title       <chr> "Unspecified", "Renewable solid unspecified", "Renewable solid municipal waste", "Renewable solid …
#> $ description <chr> "Fuel that cannot be associated with any of available fuel codes.", "Fuel produced in a solid form…
```
