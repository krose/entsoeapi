# List price category types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List price category types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
price_category_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 8
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::price_category_types)
#> Rows: 8
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08"
#> $ title       <chr> "Category 1", "Category 2", "Category 3", "Excess balance", "Insufficient balance", "Average bid p…
#> $ description <chr> "A category one price calculation is to be applied.", "A category two price calculation is to be a…
```
