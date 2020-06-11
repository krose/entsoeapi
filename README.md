
<!-- README.md is generated from README.Rmd. Please edit that file -->

# entsoeapi

<!-- badges: start -->

<!-- badges: end -->

The goal of entsoeapi is to create an easy wrapper for the ENTSO-E api.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("krose/entsoeapi")
```

## Security token

Read here how to get a security token.

<https://transparency.entsoe.eu/content/static_content/Static%20content/web%20api/Guide.html#_authentication_and_authorisation>

You should also create a `.Renviron` file in your working directory with
a security token and call it ENTSOE\_PAT.

    ENTSOE_PAT = "<your_security_token>"

## Example

You use the eic codes to get the data. Let’s try to find the eic code
for Germany.

``` r
library(tidyverse)
library(entsoeapi)

en_eic() %>% 
  filter(AreaTypeCode == "CTY", 
         AreaName == "Germany") %>% 
  glimpse()
#> Observations: 1
#> Variables: 4
#> $ AreaCode     <chr> "10Y1001A1001A83F"
#> $ AreaTypeCode <chr> "CTY"
#> $ AreaName     <chr> "Germany"
#> $ MapCode      <chr> "DE"
```

For some of the data you need to translate the generation codes.

``` r

en_generation_codes() %>%
  glimpse()
#> Observations: 27
#> Variables: 2
#> $ codes   <chr> "A03", "A04", "A05", "B01", "B02", "B03", "B04", "B05"...
#> $ meaning <chr> "Mixed", "Generation", "Load", "Biomass", "Fossil Brow...
```

Let’s get the demand in Germany.

``` r

en_load_actual_total_load(eic = "10Y1001A1001A83F", 
                          period_start = lubridate::ymd("2020-01-01", tz = "CET"),
                          period_end = lubridate::ymd_hm("2020-01-01 23:00", tz = "CET")) %>%
  glimpse()
#> Observations: 92
#> Variables: 4
#> $ dt               <dttm> 2019-12-31 23:00:00, 2019-12-31 23:15:00, 20...
#> $ quantity         <dbl> 42783, 42374, 42103, 42078, 41737, 41198, 410...
#> $ unit             <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MA...
#> $ out_bidding_zone <chr> "10Y1001A1001A83F", "10Y1001A1001A83F", "10Y1...
```

This is basically how all the functions work, so let’s try to get the
production data too.

``` r

en_generation_agg_gen_per_type(eic = "10Y1001A1001A83F", 
                               period_start = lubridate::ymd("2020-01-01", tz = "CET"),
                               period_end = lubridate::ymd_hm("2020-01-01 23:00", tz = "CET")) %>%
  glimpse()
#> Observations: 2,484
#> Variables: 6
#> $ inBiddingZone_Domain.mRID  <chr> "10Y1001A1001A83F", "10Y1001A1001A8...
#> $ quantity_Measure_Unit.name <chr> "MAW", "MAW", "MAW", "MAW", "MAW", ...
#> $ MktPSRType                 <chr> "B01", "B01", "B01", "B01", "B01", ...
#> $ quantity                   <int> 4865, 4870, 4859, 4852, 4851, 4833,...
#> $ dt                         <dttm> 2019-12-31 23:00:00, 2019-12-31 23...
#> $ outBiddingZone_Domain.mRID <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA,...
```
