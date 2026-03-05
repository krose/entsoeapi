# List coordinate system types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List coordinate system types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
coordinate_system_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 4 rows
and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::coordinate_system_types)
#> Rows: 4
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04"
#> $ title       <chr> "ED50", "OSGB36", "WGS84", "GTRF"
#> $ description <chr> "ED 50 (European Datum 1950) is a geodetic datum which was defined after World War II for the inte…
```
