# List Contract Types

List Contract Types

## Usage

``` r
contract_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 16
rows and 3 columns.

## Examples

``` r
str(entsoeapi::contract_types)
#> Classes ‘data.table’ and 'data.frame':   16 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02" "A03" "A04" ...
#>  $ title      : chr  "Daily contract" "Weekly contract" "Monthly contract" "Yearly contract" ...
#>  $ description: chr  "The condition under which capacity is allocated and handled is by daily auction or a daily transmission allocation procedure." "The condition under which capacity is allocated and handled is by weekly auction or a weekly transmission alloc"| __truncated__ "The condition under which capacity is allocated and handled is by monthly auction or a monthly transmission all"| __truncated__ "The condition under which capacity is allocated and handled is by yearly auction or a yearly transmission alloc"| __truncated__ ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
