# List category types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List category types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
category_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 4 rows
and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::category_types)
#> Rows: 4
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04"
#> $ title       <chr> "Base", "Peak", "Off peak", "Hourly"
#> $ description <chr> "The auction is for a base period.", "The auction is for a peak period.", "The auction is for an o…
```
