# List Flow Commodity Types

List Flow Commodity Types

## Usage

``` r
flow_commodity_option_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 4 rows
and 3 columns.

## Examples

``` r
str(entsoeapi::flow_commodity_option_types)
#> Classes ‘data.table’ and 'data.frame':   4 obs. of  3 variables:
#>  $ code       : chr  "E17" "E18" "E19" "E20"
#>  $ title      : chr  "Consumption" "Production" "Combined" "Exchange"
#>  $ description: chr  "The coded identification of the type of entity, e.g. consumption metering point." "The coded identification of the type of entity, e.g. production metering point." "The coded identification of the type of entity, e.g. combined consumption and production metering point." "The coded identification of the type of entity, e.g. exchange metering point."
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
