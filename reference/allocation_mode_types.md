# List Allocation Mode Types

List Allocation Mode Types

## Usage

``` r
allocation_mode_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 4 rows
and 3 columns.

## Examples

``` r
str(entsoeapi::allocation_mode_types)
#> Classes ‘data.table’ and 'data.frame':   4 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02" "A03" "A04"
#>  $ title      : chr  "Order by price with pro rata" "Order by price with first come - first served" "First come - First served" "Pro rata"
#>  $ description: chr  "The allocation method is by price with eventual pro rata." "The allocation method is by price with eventual use of first come first served." "The allocation method is first come first served." "The allocation method is pro rata."
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
