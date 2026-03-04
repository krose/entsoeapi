# List EIC Types

List EIC Types

## Usage

``` r
eic_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 7 rows
and 3 columns.

## Examples

``` r
str(entsoeapi::eic_types)
#> Classes ‘data.table’ and 'data.frame':   7 obs. of  3 variables:
#>  $ code       : chr  "A" "T" "V" "W" ...
#>  $ title      : chr  "Substation" "Tieline" "Location" "Resource Object" ...
#>  $ description: chr  "An EIC code to substations." "An EIC code to identify tielines." "An EIC code to identify locations." "An EIC code to identify resource objects." ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
