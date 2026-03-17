# List grid agreement types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List grid agreement types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
grid_agreement_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 4
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::grid_agreement_types)
#> Rows: 4
#> Columns: 3
#> $ code        <chr> "E01", "E02", "E03", "E04"
#> $ title       <chr> "Grid usage contract directly between Grid Access Provider and Customer", "Grid usage contract  di…
#> $ description <chr> "The grid usage contract is a contract directly between the grid access provider and the customer.…
```
