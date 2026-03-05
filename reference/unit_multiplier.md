# List unit multiplier based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List unit multiplier based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
unit_multiplier
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 1 rows
and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::unit_multiplier)
#> Rows: 1
#> Columns: 3
#> $ code        <chr> "1"
#> $ title       <chr> "none"
#> $ description <chr> "No multiplier or equivalently multiply by 1."
```
