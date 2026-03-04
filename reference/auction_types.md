# List Auction Types

List Auction Types

## Usage

``` r
auction_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 8 rows
and 3 columns.

## Examples

``` r
str(entsoeapi::auction_types)
#> Classes ‘data.table’ and 'data.frame':   8 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02" "A03" "A04" ...
#>  $ title      : chr  "Implicit" "Explicit" "Rule Based" "Mixed" ...
#>  $ description: chr  "The auction is an implicit auction." "The auction is an explicit auction." "The auction is a rule based auction." "The auction is partially implicit and partially explicit." ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
