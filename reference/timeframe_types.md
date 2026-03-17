# List timeframe types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List timeframe types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
timeframe_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 46
rows and 3 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::timeframe_types)
#> Rows: 46
#> Columns: 3
#> $ code        <chr> "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", "A12", "A13", "A14", …
#> $ title       <chr> "Real time", "Intraday", "Hour-1", "Hour-2", "Hour-3", "Hour-4", "Hour-5", "Hour-6", "Hour-7", "Ho…
#> $ description <chr> "The information provided concerns real time timeframe.", "The information provided concerns an in…
```
