# List quality types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List quality types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
quality_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 7
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::quality_types)
#> Rows: 7
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04", "A05", "A06", "A07"
#> $ title       <chr> "Adjusted", "Not available", "Estimated", "As provided", "Incomplete", "Calculated", "Temporary"
#> $ description <chr> "The contents of the object have been adjusted.", "The contents of the object are not available.",…
```
