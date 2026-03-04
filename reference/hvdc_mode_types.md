# List HVDC Mode Types

List HVDC Mode Types

## Usage

``` r
hvdc_mode_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 3 rows
and 3 columns.

## Examples

``` r
str(entsoeapi::hvdc_mode_types)
#> Classes ‘data.table’ and 'data.frame':   3 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02" "A03"
#>  $ title      : chr  "Setpoint schedule" "Proportional external signal" "AC emulation"
#>  $ description: chr  "The code for the \"power setpoint\" mode of operation of the HVDC link." "The code for the \"Proportional external signal\" mode of operation of the HVDC link." "The code for the \"AC emulation\" mode of operation of the HVDC link."
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
