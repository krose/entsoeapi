# List Energy Product Types

List Energy Product Types

## Usage

``` r
energy_product_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 9 rows
and 3 columns.

## Examples

``` r
str(entsoeapi::energy_product_types)
#> Classes ‘data.table’ and 'data.frame':   9 obs. of  3 variables:
#>  $ code       : chr  "8716867000016" "8716867000023" "8716867000030" "8716867000047" ...
#>  $ title      : chr  "Active power" "Reactive power" "Active energy" "Reactive energy" ...
#>  $ description: chr  "The product of voltage and the in-phase component of alternating current measured in units of watts and standar"| __truncated__ "The product of voltage and current and the sine of the phase angle between them, measured in units of voltamper"| __truncated__ "The electrical energy produced, flowing or supplied by an electrical circuit during a time interval, being the "| __truncated__ "The integral with respect to time of reactive power (not used for planned schedules)." ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
