
<!-- README.md is generated from README.Rmd. Please edit that file -->

# entsoeapi

<!-- badges: start -->

<!-- badges: end -->

The goal of entsoeapi is to create an easy wrapper for the ENTSO-E api.
The goal is to implement easy wrappers for most of the data available on
the ENTSO-E [transparency](https://transparency.entsoe.eu/) platform.

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

    ENTSOE_PAT = "your_security_token"

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
#> Variables: 4
#> $ codes      <chr> "A03", "A04", "A05", "B01", "B02", "B03", "B04", "B...
#> $ meaning    <chr> "Mixed", "Generation", "Load", "Biomass", "Fossil B...
#> $ co2_g_kwh  <dbl> NA, NA, NA, 390, 360, NA, 200, 340, 260, NA, 380, N...
#> $ efficiency <dbl> NA, NA, NA, NA, 0.35, NA, 0.50, 0.38, NA, NA, NA, N...
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
#> Rows: 1,564
#> Columns: 23
#> $ process_type               <chr> "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16", "A16",…
#> $ process_type_def           <chr> "Realised", "Realised", "Realised", "Realised", "Realised", "Realised", "Realised", "Realised", "Realised", "Re…
#> $ curve_type                 <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01",…
#> $ curve_type_def             <chr> "Sequential fixed size block", "Sequential fixed size block", "Sequential fixed size block", "Sequential fixed …
#> $ object_aggregation         <chr> "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08", "A08",…
#> $ object_aggregation_def     <chr> "Resource type", "Resource type", "Resource type", "Resource type", "Resource type", "Resource type", "Resource…
#> $ business_type              <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A94", "A01", "A93", "A93",…
#> $ business_type_def          <chr> "Production", "Production", "Production", "Production", "Production", "Production", "Production", "Production",…
#> $ document_mrid              <chr> "8fc5f3457c114ffd9023e6b902140292", "8fc5f3457c114ffd9023e6b902140292", "8fc5f3457c114ffd9023e6b902140292", "8f…
#> $ inbiddingzone_domain_mrid  <chr> "10Y1001A1001A83F", "10Y1001A1001A83F", "10Y1001A1001A83F", "10Y1001A1001A83F", "10Y1001A1001A83F", "10Y1001A10…
#> $ outbiddingzone_domain_mrid <chr> NA, NA, NA, NA, NA, NA, NA, "10Y1001A1001A83F", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ resource_psr_type          <chr> "B01", "B02", "B04", "B05", "B06", "B09", "B10", "B10", "B11", "B12", "B14", "B15", "B16", "B17", "B18", "B19",…
#> $ resource_psr_type_def      <chr> "Biomass", "Fossil Brown coal/Lignite", "Fossil Gas", "Fossil Hard coal", "Fossil Oil", "Geothermal", "Hydro Pu…
#> $ revision_number            <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "…
#> $ ts_mrid                    <chr> "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "13", "14", "15", "16", "17", "12", "1", "2", "3", "4"…
#> $ dt_created                 <dttm> 2024-07-05 20:21:00, 2024-07-05 20:21:00, 2024-07-05 20:21:00, 2024-07-05 20:21:00, 2024-07-05 20:21:00, 2024-…
#> $ dt_start                   <dttm> 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019-…
#> $ dt_end                     <dttm> 2020-01-01 22:00:00, 2020-01-01 22:00:00, 2020-01-01 22:00:00, 2020-01-01 22:00:00, 2020-01-01 22:00:00, 2020-…
#> $ resolution                 <chr> "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT…
#> $ position                   <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2", "2", "…
#> $ start                      <dttm> 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019-12-31 23:00:00, 2019-…
#> $ quantity                   <chr> "4918", "9280", "5077", "1985", "454", "30", "1065", "195", "1320", "54", "8096", "195", "0", "830", "501", "59…
#> $ quantity_measure_unit      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW",…
```
