# List Asset Types

List Asset Types

## Usage

``` r
asset_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 90
rows and 3 columns.

## Examples

``` r
str(entsoeapi::asset_types)
#> Classes ‘data.table’ and 'data.frame':   90 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02" "A03" "A04" ...
#>  $ title      : chr  "Tie line" "Line" "Resource Object" "Generation" ...
#>  $ description: chr  "A high voltage line used for cross border energy interconnections." "A specific electric line within a country." "A resource that can either produce or consume energy." "A resource that can produce energy." ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
