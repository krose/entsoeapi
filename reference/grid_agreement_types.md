# List Grid Agreement Types

List Grid Agreement Types

## Usage

``` r
grid_agreement_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 4 rows
and 3 columns.

## Examples

``` r
str(entsoeapi::grid_agreement_types)
#> Classes ‘data.table’ and 'data.frame':   4 obs. of  3 variables:
#>  $ code       : chr  "E01" "E02" "E03" "E04"
#>  $ title      : chr  "Grid usage contract directly between Grid Access Provider and Customer" "Grid usage contract  directly between Energy Supplier and Grid Access Provider" "Grid usage contract  between Grid Access Provider and Customer through Energy Supplier" "No grid usage contract"
#>  $ description: chr  "The grid usage contract is a contract directly between the grid access provider and the customer." "The grid usage contract is a contract directly between the energy supplier and the grid access provider." "The grid usage contract is a contract between the grid access provider and customer through the energy supplier." "There is no grid usage contract that must be signed by the customer."
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
