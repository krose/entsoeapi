# List contract types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List contract types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
contract_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 16
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::contract_types)
#> Rows: 16
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", "A12", "A13", "A14", …
#> $ title       <chr> "Daily contract", "Weekly contract", "Monthly contract", "Yearly contract", "Total contract", "Lon…
#> $ description <chr> "The condition under which capacity is allocated and handled is by daily auction or a daily transm…
```
