# List currency types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List currency types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
currency_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 23
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::currency_types)
#> Rows: 23
#> Columns: 3
#> $ code        <chr> "BAM", "BGN", "CHF", "CZK", "DKK", "EUR", "GBP", "HRK", "HUF", "ISK", "LEK", "LTL", "MKD", "NOK", …
#> $ title       <chr> "Bosnian convertible marka", "Bulgarian lev", "Swiss Franc", "Czech Koruna", "Danish Kroner ", "EU…
#> $ description <chr> "The Legal tender of Bosnia and Herzegovina.", "The Legal tender of Bulgaria.", "The Legal tender …
```
