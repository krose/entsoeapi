# List Unit of Measure Types

List Unit of Measure Types

## Usage

``` r
unit_of_measure_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 33
rows and 3 columns.

## Examples

``` r
str(entsoeapi::unit_of_measure_types)
#> Classes ‘data.table’ and 'data.frame':   33 obs. of  3 variables:
#>  $ code       : chr  "A59" "A90" "A97" "AMP" ...
#>  $ title      : chr  "OKTA unit" "gigawatt" "hectopascal" "ampere" ...
#>  $ description: chr  "A unit of measurement of the cloudiness expressed in OKTA or OCTA, i.e. A unit of count defining the number of "| __truncated__ "GW unit as per UN/CEFACT recommendation 20." "A unit of measurement of the pressure expressed in hectopascal." "The unit of electrical current in the International system of Units (SI) equivalent to one Coulomb per second." ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
