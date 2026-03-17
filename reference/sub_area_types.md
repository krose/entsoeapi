# List sub-area types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List sub-area types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
sub_area_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 7
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::sub_area_types)
#> Rows: 7
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04", "A05", "A06", "A07"
#> $ title       <chr> "Regional", "Industrial", "Distribution", "Non-concessional", "Production", "Transmission (main/ce…
#> $ description <chr> "The domain represents a regional grid.", "The domain represents an industrial grid.", "The domain…
```
