# List curve types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List curve types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
curve_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 5 rows
and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::curve_types)
#> Rows: 5
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04", "A05"
#> $ title       <chr> "Sequential fixed size block", "Point", "Variable sized Block", "Overlapping breakpoint", "Non-ove…
#> $ description <chr> "The curve is made of successive Intervals of time (Blocks) of constant duration (size), where the…
```
