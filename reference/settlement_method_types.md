# List Settlement Method Types

List Settlement Method Types

## Usage

``` r
settlement_method_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 2 rows
and 3 columns.

## Examples

``` r
str(entsoeapi::settlement_method_types)
#> Classes ‘data.table’ and 'data.frame':   2 obs. of  3 variables:
#>  $ code       : chr  "E01" "E02"
#>  $ title      : chr  "Profiled" "Non-profiled"
#>  $ description: chr  "The settlement method regards profiled metered accounting points." "The settlement method regards continuous metered (non-profiled) accounting points."
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
