# List Role Types

List Role Types

## Usage

``` r
role_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 59
rows and 3 columns.

## Examples

``` r
str(entsoeapi::role_types)
#> Classes ‘data.table’ and 'data.frame':   59 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02" "A03" "A04" ...
#>  $ title      : chr  "Trade responsible party" "Consumption responsible party" "Combined power exchange (not to be used)" "System operator" ...
#>  $ description: chr  "Refer to role model definitions in the ENTSO-E Harmonised Role Model Document." "Refer to role model definitions in the ENTSO-E Harmonised Role Model Document." "This role is no longer in the ENTSO-E Harmonised Role Model Document." "Refer to role model definitions in the ENTSO-E Harmonised Role Model Document." ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
