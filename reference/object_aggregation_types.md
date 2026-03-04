# List Object Aggregation Types

List Object Aggregation Types

## Usage

``` r
object_aggregation_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 16
rows and 3 columns.

## Examples

``` r
str(entsoeapi::object_aggregation_types)
#> Classes ‘data.table’ and 'data.frame':   16 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02" "A03" "A04" ...
#>  $ title      : chr  "Area" "Metering point" "Party" "Agreement Identification" ...
#>  $ description: chr  "The object being described concerns an area." "The object being described concerns a metering point." "The object being described concerns a party." "The object being described concerns an agreement identification." ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
