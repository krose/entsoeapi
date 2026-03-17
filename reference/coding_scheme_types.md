# List coding scheme types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List coding scheme types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
coding_scheme_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 51
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::coding_scheme_types)
#> Rows: 51
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A10", "NAD", "NAL", "NAM", "NAT", "NAZ", "NBA", "NBE", "NBG", "NCH", "NCS", …
#> $ title       <chr> "EIC", "CGM", "ISO", "Coding scheme which are part of GS1 system", "Andorra National coding scheme…
#> $ description <chr> "The coding scheme is the Energy Identification Coding Scheme (EIC), maintained by ENTSO-E.", "The…
```
