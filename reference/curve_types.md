# List Curve Types

List Curve Types

## Usage

``` r
curve_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 5 rows
and 3 columns.

## Examples

``` r
str(entsoeapi::curve_types)
#> Classes ‘data.table’ and 'data.frame':   5 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02" "A03" "A04" ...
#>  $ title      : chr  "Sequential fixed size block" "Point" "Variable sized Block" "Overlapping breakpoint" ...
#>  $ description: chr  "The curve is made of successive Intervals of time (Blocks) of constant duration (size), where the size of the B"| __truncated__ "The curve is made of successive instants of time (Points)." "The curve is made of successive Intervals of time (Blocks) of variable duration (size), where the end date and "| __truncated__ "The curve is made of successive Intervals of time of variable duration (size), where the end date and end time "| __truncated__ ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
