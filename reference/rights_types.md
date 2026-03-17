# List rights types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List rights types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
rights_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 6
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::rights_types)
#> Rows: 6
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04", "A05", "A06"
#> $ title       <chr> "Use It Or Lose It", "Use It Or Sell It", "Allocation curtailment possible", "Nomination curtailme…
#> $ description <chr> "Any rights not nominated shall be lost.", "Any rights that are not nominated shall be sold.", "Ri…
```
