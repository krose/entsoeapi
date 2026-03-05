# List document types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List document types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
document_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 103
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::document_types)
#> Rows: 103
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", "A12", "A13", "A14", …
#> $ title       <chr> "Balance responsible schedule", "Allocated capacity schedule", "Balance area schedule", "System Op…
#> $ description <chr> "A schedule which has been prepared by a balance responsible party providing planned schedule info…
```
