# List Quality Types

List Quality Types

## Usage

``` r
quality_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 7 rows
and 3 columns.

## Examples

``` r
str(entsoeapi::quality_types)
#> Classes ‘data.table’ and 'data.frame':   7 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02" "A03" "A04" ...
#>  $ title      : chr  "Adjusted" "Not available" "Estimated" "As provided" ...
#>  $ description: chr  "The contents of the object have been adjusted." "The contents of the object are not available." "The contents of the object are estimated. The code is typically used when measured values are missing and an es"| __truncated__ "The contents of the object are as provided." ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
