# List object aggregation types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List object aggregation types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
object_aggregation_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 16
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::object_aggregation_types)
#> Rows: 16
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", "A12", "A13", "A14", …
#> $ title       <chr> "Area", "Metering point", "Party", "Agreement Identification", "Accounting point", "Resource Objec…
#> $ description <chr> "The object being described concerns an area.", "The object being described concerns a metering po…
```
