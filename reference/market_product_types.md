# List Market Product Types

List Market Product Types

## Usage

``` r
market_product_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 14
rows and 3 columns.

## Examples

``` r
str(entsoeapi::market_product_types)
#> Classes ‘data.table’ and 'data.frame':   14 obs. of  3 variables:
#>  $ code       : chr  "A01" "A02" "A03" "A04" ...
#>  $ title      : chr  "Standard balancing product" "Specific balancing product" "Product from integrated scheduling process" "Local balancing product" ...
#>  $ description: chr  "A harmonised balancing product defined by all TSOs for the exchange of balancing services." "A product different from a standard product." "From the EBGL Article 2 (19), means an iterative process that uses at least integrated scheduling process bids "| __truncated__ "A balancing product that is neither standard nor specific. This type may be applicable only in the interim peri"| __truncated__ ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
