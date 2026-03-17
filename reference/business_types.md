# List business types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List business types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
business_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 293
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::business_types)
#> Rows: 293
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", "A12", "A13", "A14", …
#> $ title       <chr> "Production", "Internal trade", "External trade explicit capacity", "Consumption", "External trade…
#> $ description <chr> "The nature of the business being described is production details.", "The nature of the business b…
```
