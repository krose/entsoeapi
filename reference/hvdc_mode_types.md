# List HVDC mode types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List HVDC mode types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
hvdc_mode_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 3 rows
and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::hvdc_mode_types)
#> Rows: 3
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03"
#> $ title       <chr> "Setpoint schedule", "Proportional external signal", "AC emulation"
#> $ description <chr> "The code for the \"power setpoint\" mode of operation of the HVDC link.", "The code for the \"Pro…
```
