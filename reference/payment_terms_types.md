# List Payment Terms Types

List Payment Terms Types

## Usage

``` r
payment_terms_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 3 rows
and 3 columns.

## Examples

``` r
str(entsoeapi::payment_terms_types)
#> Classes ‘data.table’ and 'data.frame':   3 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02" "A03"
#>  $ title      : chr  "Pay as bid" "Pay as cleared" "No payment terms"
#>  $ description: chr  "The amount to be paid shall correspond to the amount bid." "The amount to be paid shall correspond to the amount calculated for clearing." "There are no payment terms to be used."
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
