# List process types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List process types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
process_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 75
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::process_types)
#> Rows: 75
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", "A12", "A13", "A14", …
#> $ title       <chr> "Day ahead", "Intra day incremental", "Inter-area transit", "System operation closure", "Metered d…
#> $ description <chr> "The information provided concerns a day ahead process.", "The information provided concerns an in…
```
