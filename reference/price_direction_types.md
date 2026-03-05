# List price direction types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List price direction types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
price_direction_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 2 rows
and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::price_direction_types)
#> Rows: 2
#> Columns: 3
#> $ code        <chr> "A01", "A02"
#> $ title       <chr> "Expenditure.", "Income."
#> $ description <chr> "Expenditure, i.e. the Impacted Area System Operator pays to the internal Market Parties.", "Incom…
```
