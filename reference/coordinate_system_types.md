# List Coordinate System Types

List Coordinate System Types

## Usage

``` r
coordinate_system_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 4 rows
and 3 columns.

## Examples

``` r
str(entsoeapi::coordinate_system_types)
#> Classes ‘data.table’ and 'data.frame':   4 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02" "A03" "A04"
#>  $ title      : chr  "ED50" "OSGB36" "WGS84" "GTRF"
#>  $ description: chr  "ED 50 (European Datum 1950) is a geodetic datum which was defined after World War II for the international conn"| __truncated__ "Ordinance Survey Great Britain 1936. The Ordinance Survey (OS) devised the national grid reference system, and "| __truncated__ "The World Geodetic System version 1984. for use in cartography, geodesy, and navigation including by GPS. It co"| __truncated__ "Galileo Terrestrial Reference Frame"
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
