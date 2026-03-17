# List analog types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List analog types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
analog_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 42
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::analog_types)
#> Rows: 42
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", "A12", "A13", "A14", …
#> $ title       <chr> "Flow", "Permanent admissible transmission limit (PATL)", "Flow reliability margin", "Spanning mar…
#> $ description <chr> "This is the computed flow for the monitored element in the constraint situation (\"N situation\",…
```
