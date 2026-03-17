# List market product types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List market product types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
market_product_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 14
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::market_product_types)
#> Rows: 14
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", "A12", "A13", "A14"
#> $ title       <chr> "Standard balancing product", "Specific balancing product", "Product from integrated scheduling pr…
#> $ description <chr> "A harmonised balancing product defined by all TSOs for the exchange of balancing services.", "A p…
```
