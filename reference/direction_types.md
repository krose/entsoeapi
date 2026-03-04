# List Direction Types

List Direction Types

## Usage

``` r
direction_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 4 rows
and 3 columns.

## Examples

``` r
str(entsoeapi::direction_types)
#> Classes ‘data.table’ and 'data.frame':   4 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02" "A03" "A04"
#>  $ title      : chr  "UP" "DOWN" "UP and DOWN" "Stable"
#>  $ description: chr  "Up signifies that the available power can be used by the Purchasing area to increase energy." "Down signifies that the available power can be used by the Purchasing area to decrease energy." "Up and Down signifies that the UP and Down values are equal." "The direction at a given instant in time is considered to be stable."
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
