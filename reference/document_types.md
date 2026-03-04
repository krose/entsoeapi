# List Document Types

List Document Types

## Usage

``` r
document_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 103
rows and 3 columns.

## Examples

``` r
str(entsoeapi::document_types)
#> Classes ‘data.table’ and 'data.frame':   103 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02" "A03" "A04" ...
#>  $ title      : chr  "Balance responsible schedule" "Allocated capacity schedule" "Balance area schedule" "System Operator area schedule" ...
#>  $ description: chr  "A schedule which has been prepared by a balance responsible party providing planned schedule information." "A schedule which has been prepared by a capacity allocator providing allocated capacity." "A schedule that provides the planned schedule information for a balance area." "A compilation of all external schedules concerning two System Operator areas of all balance responsible parties." ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
