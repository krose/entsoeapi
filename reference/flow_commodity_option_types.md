# List flow commodity types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List flow commodity types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
flow_commodity_option_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 4 rows
and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::flow_commodity_option_types)
#> Rows: 4
#> Columns: 3
#> $ code        <chr> "E17", "E18", "E19", "E20"
#> $ title       <chr> "Consumption", "Production", "Combined", "Exchange"
#> $ description <chr> "The coded identification of the type of entity, e.g. consumption metering point.", "The coded ide…
```
