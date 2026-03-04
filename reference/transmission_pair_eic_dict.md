# List Transmission Pair EIC Dictionary

List Transmission Pair EIC Dictionary

## Usage

``` r
transmission_pair_eic_dict
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 114
rows and 8 columns.

## Examples

``` r
str(entsoeapi::transmission_pair_eic_dict)
#> Classes ‘data.table’ and 'data.frame':   114 obs. of  8 variables:
#>  $ OutAreaCode    : chr  "10YLT-1001A0008Q" "10YNO-4--------9" "10Y1001A1001A39I" "10Y1001A1001A45N" ...
#>  $ OutAreaTypeCode: chr  "BZN" "BZN" "BZN" "BZN" ...
#>  $ OutAreaName    : chr  "Litgrid BZ" "NO4 BZ" "Elering BZ" "SE2 BZ" ...
#>  $ OutMapCode     : chr  "LT" "NO4" "EE" "SE2" ...
#>  $ InAreaCode     : chr  "10Y1001A1001A47J" "10Y1001A1001A44P" "10YFI-1--------U" "10Y1001A1001A44P" ...
#>  $ InAreaTypeCode : chr  "BZN" "BZN" "BZN" "BZN" ...
#>  $ InAreaName     : chr  "SE4 BZ" "SE1 BZ" "Fingrid BZ" "SE1 BZ" ...
#>  $ InMapCode      : chr  "SE4" "SE1" "FI" "SE1" ...
```
