# List customer types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List customer types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
customer_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 4 rows
and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::customer_types)
#> Rows: 4
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04"
#> $ title       <chr> "Household customer", "Deceased household customer", "Company customer", "Estate company customer"
#> $ description <chr> "The customer is a household customer.", "The customer is a deceased household customer. May be us…
```
