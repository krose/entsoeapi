# List classification types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List classification types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
classification_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 2
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::classification_types)
#> Rows: 2
#> Columns: 3
#> $ code        <chr> "A01", "A02"
#> $ title       <chr> "Detail type ", "Summary type "
#> $ description <chr> "The Time Series content provides detailed information.", "The Time Series content provides aggreg…
```
