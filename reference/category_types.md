# List Category Types

List Category Types

## Usage

``` r
category_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 4 rows
and 3 columns.

## Examples

``` r
str(entsoeapi::category_types)
#> Classes ‘data.table’ and 'data.frame':   4 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02" "A03" "A04"
#>  $ title      : chr  "Base" "Peak" "Off peak" "Hourly"
#>  $ description: chr  "The auction is for a base period." "The auction is for a peak period." "The auction is for an off peak period." "The auction is for an hourly period."
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
