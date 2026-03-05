# List transmission pair EIC Dictionary

List transmission pair EIC Dictionary

## Usage

``` r
transmission_pair_eic_dict
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 114
rows and 8 columns.

## Examples

``` r
dplyr::glimpse(entsoeapi::transmission_pair_eic_dict)
#> Rows: 114
#> Columns: 8
#> $ OutAreaCode     <chr> "10YLT-1001A0008Q", "10YNO-4--------9", "10Y1001A1001A39I", "10Y1001A1001A45N", "10YNO-4------…
#> $ OutAreaTypeCode <chr> "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "CTA", "CTA", "BZN", "CTA", "BZ…
#> $ OutAreaName     <chr> "Litgrid BZ", "NO4 BZ", "Elering BZ", "SE2 BZ", "NO4 BZ", "PSE SA BZ", "Fingrid BZ", "DK2 BZ",…
#> $ OutMapCode      <chr> "LT", "NO4", "EE", "SE2", "NO4", "PL", "FI", "DK2", "SE4", "AT", "SI", "SE4", "IT", "EE", "LV"…
#> $ InAreaCode      <chr> "10Y1001A1001A47J", "10Y1001A1001A44P", "10YFI-1--------U", "10Y1001A1001A44P", "10Y1001A1001A…
#> $ InAreaTypeCode  <chr> "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "CTA", "CTA", "BZN", "CTA", "BZ…
#> $ InAreaName      <chr> "SE4 BZ", "SE1 BZ", "Fingrid BZ", "SE1 BZ", "SE2 BZ", "SE4 BZ", "Elering BZ", "SE4 BZ", "PSE S…
#> $ InMapCode       <chr> "SE4", "SE1", "FI", "SE1", "SE2", "SE4", "EE", "SE4", "PL", "SI", "AT", "DE_LU", "AT", "LV", "…
```
