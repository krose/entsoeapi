# List Status Types

List Status Types

## Usage

``` r
status_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 76
rows and 3 columns.

## Examples

``` r
str(entsoeapi::status_types)
#> Classes ‘data.table’ and 'data.frame':   76 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02" "A03" "A04" ...
#>  $ title      : chr  "Intermediate" "Final" "Deactivated" "Reactivated" ...
#>  $ description: chr  "The document is in a non finalized state." "The document is in a definitive state." "The object being reported has been deactivated." "The object being reported has been reactivated." ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
