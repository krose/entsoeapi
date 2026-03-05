# List direction types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List direction types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
direction_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 4 rows
and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::direction_types)
#> Rows: 4
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04"
#> $ title       <chr> "UP", "DOWN", "UP and DOWN", "Stable"
#> $ description <chr> "Up signifies that the available power can be used by the Purchasing area to increase energy.", "D…
```
