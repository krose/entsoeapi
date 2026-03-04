# List Process Types

List Process Types

## Usage

``` r
process_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 75
rows and 3 columns.

## Examples

``` r
str(entsoeapi::process_types)
#> Classes ‘data.table’ and 'data.frame':   75 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02" "A03" "A04" ...
#>  $ title      : chr  "Day ahead" "Intra day incremental" "Inter-area transit" "System operation closure" ...
#>  $ description: chr  "The information provided concerns a day ahead process." "The information provided concerns an intra day schedule." "The information provided concerns an inter area transit schedule. The rules governing this process are market dependent" "The information provided concerns the closure of a given period of both scheduled and regulation information." ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
