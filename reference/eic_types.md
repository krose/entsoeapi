# List EIC types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List EIC types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
eic_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 7
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::eic_types)
#> Rows: 7
#> Columns: 3
#> $ code        <chr> "A", "T", "V", "W", "X", "Y", "Z"
#> $ title       <chr> "Substation", "Tieline", "Location", "Resource Object", "Party", "Area or Domain", "Measurement po…
#> $ description <chr> "An EIC code to substations.", "An EIC code to identify tielines.", "An EIC code to identify locat…
```
