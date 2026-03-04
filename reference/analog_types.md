# List Analog Types

List Analog Types

## Usage

``` r
analog_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 42
rows and 3 columns.

## Examples

``` r
str(entsoeapi::analog_types)
#> Classes ‘data.table’ and 'data.frame':   42 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02" "A03" "A04" ...
#>  $ title      : chr  "Flow" "Permanent admissible transmission limit (PATL)" "Flow reliability margin" "Spanning margin value" ...
#>  $ description: chr  "This is the computed flow for the monitored element in the constraint situation (\"N situation\", \"N-1 situati"| __truncated__ "The permanent load of transmission system elements which is allowed for an unlimited period and which does not "| __truncated__ "This is the flow reliability margin for a given critical network element. The amount of MW or A that is reserve"| __truncated__ "This is the margin that is taken into account when spanning (fall-back process) is applied. Spanning marginal v"| __truncated__ ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
