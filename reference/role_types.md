# List role types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List role types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
role_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 59
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::role_types)
#> Rows: 59
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", "A12", "A13", "A14", …
#> $ title       <chr> "Trade responsible party", "Consumption responsible party", "Combined power exchange (not to be us…
#> $ description <chr> "Refer to role model definitions in the ENTSO-E Harmonised Role Model Document.", "Refer to role m…
```
