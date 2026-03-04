# List Timeframe Types

List Timeframe Types

## Usage

``` r
timeframe_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 46
rows and 3 columns.

## Examples

``` r
str(entsoeapi::timeframe_types)
#> Classes ‘data.table’ and 'data.frame':   46 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02" "A03" "A04" ...
#>  $ title      : chr  "Real time" "Intraday" "Hour-1" "Hour-2" ...
#>  $ description: chr  "The information provided concerns real time timeframe." "The information provided concerns an intra day timeframe." "The information provided concerns 1 hour ahead for given delivery hour." "The information provided concerns 2 hours ahead for given delivery hour." ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
