# List area types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List area types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
area_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 2
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::area_types)
#> Rows: 2
#> Columns: 3
#> $ code        <chr> "A01", "A02"
#> $ title       <chr> "Bidding Zone", "Metering Grid Area"
#> $ description <chr> "A Bidding Zone (BZ) is the largest geographical area within which market participants are able to…
```
