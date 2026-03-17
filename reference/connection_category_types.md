# List connection category types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List connection category types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
connection_category_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 10
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::connection_category_types)
#> Rows: 10
#> Columns: 3
#> $ code        <chr> "E03", "E04", "E05", "E06", "E07", "E08", "E09", "E10", "E11", "E12"
#> $ title       <chr> "Maximum voltage", "High voltage", "Medium voltage", "Low voltage", "High voltage / transformation…
#> $ description <chr> "A maximum voltage connection category.", "A high voltage connection category.", "A medium voltage…
```
