# List Reason Code Types

List Reason Code Types

## Usage

``` r
reason_code_types
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 163
rows and 3 columns.

## Examples

``` r
str(entsoeapi::reason_code_types)
#> Classes ‘data.table’ and 'data.frame':   163 obs. of  3 variables:
#>  $ code       : chr  "999" "A01" "A02" "A03" ...
#>  $ title      : chr  "Errors not specifically identified " "Message fully accepted" "Message fully rejected " "Message contains errors at the time series level " ...
#>  $ description: chr  "This code is used to identify errors that have not been specifically addressed in the Reason code list. It can "| __truncated__ "The message has been fully accepted for application processing." "No part of the message has been accepted for application processing, e.g. Global position incomplete." "Part of the message contents, i.e. certain time series, has been accepted for application processing. It is nec"| __truncated__ ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
