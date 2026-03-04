# List Indicator Types

List Indicator Types

## Usage

``` r
indicator_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 2 rows
and 3 columns.

## Examples

``` r
str(entsoeapi::indicator_types)
#> Classes ‘data.table’ and 'data.frame':   2 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02"
#>  $ title      : chr  "YES" "NO"
#>  $ description: chr  "A positive indication." "A negative indication."
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
