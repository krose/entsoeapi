# List price component types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List price component types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
price_component_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 3 rows
and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::price_component_types)
#> Rows: 3
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03"
#> $ title       <chr> "Scarcity", "Incentive", "Financial neutrality"
#> $ description <chr> "A scarcity component to be used in nationally defined scarcity situations.", "An incentive compon…
```
