# List allocation mode types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List allocation mode types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
allocation_mode_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 4 rows
and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::allocation_mode_types)
#> Rows: 4
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04"
#> $ title       <chr> "Order by price with pro rata", "Order by price with first come - first served", "First come - Fir…
#> $ description <chr> "The allocation method is by price with eventual pro rata.", "The allocation method is by price wi…
```
