# List settlement method types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List settlement method types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
settlement_method_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 2
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::settlement_method_types)
#> Rows: 2
#> Columns: 3
#> $ code        <chr> "E01", "E02"
#> $ title       <chr> "Profiled", "Non-profiled"
#> $ description <chr> "The settlement method regards profiled metered accounting points.", "The settlement method regard…
```
