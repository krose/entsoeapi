# List status types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List status types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
status_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 76
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::status_types)
#> Rows: 76
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", "A12", "A13", "A14", …
#> $ title       <chr> "Intermediate", "Final", "Deactivated", "Reactivated", "Active", "Available", "Activated", "In pro…
#> $ description <chr> "The document is in a non finalized state.", "The document is in a definitive state.", "The object…
```
