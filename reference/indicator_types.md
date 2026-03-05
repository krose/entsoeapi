# List indicator types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List indicator types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
indicator_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 2 rows
and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::indicator_types)
#> Rows: 2
#> Columns: 3
#> $ code        <chr> "A01", "A02"
#> $ title       <chr> "YES", "NO"
#> $ description <chr> "A positive indication.", "A negative indication."
```
