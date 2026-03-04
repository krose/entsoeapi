# List Business Types

List Business Types

## Usage

``` r
business_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 292
rows and 3 columns.

## Examples

``` r
str(entsoeapi::business_types)
#> Classes ‘data.table’ and 'data.frame':   292 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02" "A03" "A04" ...
#>  $ title      : chr  "Production" "Internal trade" "External trade explicit capacity" "Consumption" ...
#>  $ description: chr  "The nature of the business being described is production details." "The nature of the business being described is internal trade details." "The nature of the business being described is external trade details between two areas with limited capacity re"| __truncated__ "The nature of the business being described is consumption details." ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
