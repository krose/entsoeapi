# List Coding Scheme Types

List Coding Scheme Types

## Usage

``` r
coding_scheme_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 51
rows and 3 columns.

## Examples

``` r
str(entsoeapi::coding_scheme_types)
#> Classes ‘data.table’ and 'data.frame':   51 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02" "A03" "A10" ...
#>  $ title      : chr  "EIC" "CGM" "ISO" "Coding scheme which are part of GS1 system" ...
#>  $ description: chr  "The coding scheme is the Energy Identification Coding Scheme (EIC), maintained by ENTSO-E." "The coding scheme used for Common Grid Model Exchange Standard (CGMES)." "The coding scheme for the preceding attribute is a code maintained by International Organization for Standardiz"| __truncated__ "The coding schemes maintained by GS1." ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
