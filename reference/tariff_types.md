# List tariff types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List tariff types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
tariff_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 7 rows
and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::tariff_types)
#> Rows: 7
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04", "A05", "A06", "A07"
#> $ title       <chr> "Winter HT", "Winter HHT", "Winter NT", "Summer HT", "Summer HHT1", "Summer HHT2", "Summer NT"
#> $ description <chr> "Winter HT tariff.", "Winter HHT tariff.", "Winter NT tariff.", "Summer HT tariff.", "Summer HHT1 …
```
