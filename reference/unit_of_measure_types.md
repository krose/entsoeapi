# List unit of measure types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List unit of measure types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
unit_of_measure_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 33
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::unit_of_measure_types)
#> Rows: 33
#> Columns: 3
#> $ code        <chr> "A59", "A90", "A97", "AMP", "C62", "CEL", "D54", "DD", "E08", "GWH", "HMQ", "HTZ", "KEL", "KMT", "…
#> $ title       <chr> "OKTA unit", "gigawatt", "hectopascal", "ampere", "One", "Celsius", "watt per square meter", "degr…
#> $ description <chr> "A unit of measurement of the cloudiness expressed in OKTA or OCTA, i.e. A unit of count defining …
```
