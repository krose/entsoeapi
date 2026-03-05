# List auction types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List auction types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
auction_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 8 rows
and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::auction_types)
#> Rows: 8
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08"
#> $ title       <chr> "Implicit", "Explicit", "Rule Based", "Mixed", "Explicit/split", "Shadow auction", "Flow-based", "…
#> $ description <chr> "The auction is an implicit auction.", "The auction is an explicit auction.", "The auction is a ru…
```
