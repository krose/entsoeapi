# List reason code types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List reason code types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
reason_code_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 166
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::reason_code_types)
#> Rows: 166
#> Columns: 3
#> $ code        <chr> "999", "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A20", "A21", "A22", …
#> $ title       <chr> "Errors not specifically identified ", "Message fully accepted", "Message fully rejected ", "Messa…
#> $ description <chr> "This code is used to identify errors that have not been specifically addressed in the Reason code…
```
