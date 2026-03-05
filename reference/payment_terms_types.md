# List payment terms types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List payment terms types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
payment_terms_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 3 rows
and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::payment_terms_types)
#> Rows: 3
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03"
#> $ title       <chr> "Pay as bid", "Pay as cleared", "No payment terms"
#> $ description <chr> "The amount to be paid shall correspond to the amount bid.", "The amount to be paid shall correspo…
```
