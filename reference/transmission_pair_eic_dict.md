# List transmission pair EIC Dictionary

List transmission pair EIC Dictionary

## Usage

``` r
transmission_pair_eic_dict
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 114
rows and 8 columns.

## Examples

``` r
head(entsoeapi::transmission_pair_eic_dict)
#> # A tibble: 6 × 8
#>   OutAreaCode      OutAreaTypeCode OutAreaName OutMapCode InAreaCode       InAreaTypeCode InAreaName InMapCode
#>   <chr>            <chr>           <chr>       <chr>      <chr>            <chr>          <chr>      <chr>    
#> 1 10YLT-1001A0008Q BZN             Litgrid BZ  LT         10Y1001A1001A47J BZN            SE4 BZ     SE4      
#> 2 10YNO-4--------9 BZN             NO4 BZ      NO4        10Y1001A1001A44P BZN            SE1 BZ     SE1      
#> 3 10Y1001A1001A39I BZN             Elering BZ  EE         10YFI-1--------U BZN            Fingrid BZ FI       
#> 4 10Y1001A1001A45N BZN             SE2 BZ      SE2        10Y1001A1001A44P BZN            SE1 BZ     SE1      
#> 5 10YNO-4--------9 BZN             NO4 BZ      NO4        10Y1001A1001A45N BZN            SE2 BZ     SE2      
#> 6 10YPL-AREA-----S BZN             PSE SA BZ   PL         10Y1001A1001A47J BZN            SE4 BZ     SE4      
```
