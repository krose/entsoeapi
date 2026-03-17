# List asset types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List asset types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
asset_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 90
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::asset_types)
#> Rows: 90
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", "A12", "A13", "A14", …
#> $ title       <chr> "Tie line", "Line", "Resource Object", "Generation", "Load", "Phase Shift Transformer", "Circuit B…
#> $ description <chr> "A high voltage line used for cross border energy interconnections.", "A specific electric line wi…
```
