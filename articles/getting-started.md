# Getting Started with entsoeapi

## Introduction

The `entsoeapi` package provides a standardized R interface to the
ENTSO-E (European Network of Transmission System Operators for
Electricity) Transparency Platform API. This platform publishes
electricity market, generation, load, transmission, outage, and
balancing data for European countries.

This vignette will guide you through:

- Setting up the package and obtaining API access
- Checking connectivity and platform news
- Finding Energy Identification Codes (EICs) for areas of interest
- Making your first API call
- Understanding the output structure

## Prerequisites

### Obtaining an API Token

To use the ENTSO-E Transparency Platform API, you need a security token:

1.  Register at <https://transparency.entsoe.eu/>
2.  After registration, your token should be available in your profile
3.  Set the `ENTSOE_PAT` environment variable in your .Renviron file:

``` r
usethis::edit_r_environ()
```

For persistent access, add this line to your `.Renviron` file:

``` bash
# In your ~/.Renviron file:
ENTSOE_PAT=your_token_here
```

## Installation

Install the development version from GitHub:

``` r
# Install from GitHub
remotes::install_github("krose/entsoeapi")

# Or install from CRAN (when available)
install.packages("entsoeapi")
```

Load the packages:

``` r
library(entsoeapi)
suppressPackageStartupMessages(library(dplyr))
library(cli)
library(lubridate)
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union
library(kableExtra)
#> 
#> Attaching package: 'kableExtra'
#> The following object is masked from 'package:dplyr':
#> 
#>     group_rows
```

### Testing Connectivity

The
[`there_is_provider()`](https://krose.github.io/entsoeapi/reference/there_is_provider.md)
function checks if the ENTSO-E API is reachable:

``` r
# Check if the API is accessible
there_is_provider()
#> [1] TRUE
```

### Checking Platform News

The
[`get_news()`](https://krose.github.io/entsoeapi/reference/get_news.md)
function fetches the latest announcements from the ENTSO-E Transparency
Platform RSS feed. This is useful for checking planned maintenance
windows or data publication delays before running a batch of queries:

``` r
# Show the latest 3 news items
get_news(n = 3L)
#> 
#> ── ENTSO-E Transparency Platform News ──────────────────────────────────────────────────────────────────────────────────
#> 
#> ── Issues with the Transparency Platform API, subscriptions and File Library ──
#> 
#> ℹ Mon, 30 Mar 2026 13:47:39 GMT
#> Dear Transparency Platform users,We are currently experiencing issues affecting the availability of the API service. In
#> addition, delays have been detected in Subscriptions and in the publication of new data in the File Library extracts.
#> Our IT provider is actively investigating these disruptions as a priority, and working on a fix for them. Thank you for
#> your understanding and apologies for the inconvenience caused.Kind regards,Transparency Platform team
#> 
#> ── TP PROD data publication delays ──
#> 
#> ℹ Thu, 26 Mar 2026 13:31:47 GMT
#> Dear Transparency Platform users,The Transparency Platform is experiencing performance issues since 24th March 2026.
#> These issues are affecting data processing, which is causing delays in data publication. Hence, please expect delays up
#> to 10 hours for receiving the updates via all the download channels (API, FMS, Subscriptions and Website downloads).Our
#> service provider is working on the issue with the highest priority.We sincerely apologize for the inconvenience and
#> thank you for your patience and understanding.Best regards,Transparency Platform team
#> 
#> ── Reminder: Transparency Platform Quarterly Newsletter subscription  ──
#> 
#> ℹ Tue, 24 Mar 2026 12:18:19 GMT
#> Dear Transparency Platform users,We are pleased to introduce the Transparency Platform Quarterly Newsletter. This
#> newsletter will cover topics such as feature releases, user group meeting announcements, planned events, and any
#> service interruptions.To subscribe, please use the following LINK.Kind regards,Transparency Platform team
```

The result is returned invisibly as a tibble, so you can also capture
and filter it:

``` r
news <- get_news(n = 10L)
news |>
  subset(subset = grepl(pattern = "maintenance", x = title, ignore.case = TRUE))
```

## Finding EIC Codes

Energy Identification Codes (EICs) are 16-character codes that uniquely
identify market participants, bidding zones, transmission lines, and
other entities on the ENTSO-E platform.

The
[`all_approved_eic()`](https://krose.github.io/entsoeapi/reference/all_approved_eic.md)
function returns a comprehensive list of approved EIC codes:

``` r
# Find Germany's bidding zone EIC
germany_eic <- all_approved_eic() |>
  filter(eic_long_name == "Germany_Luxemburg") |>
  pull(eic_code)
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading X_eicCodes.csv file ...
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading Y_eicCodes.csv file ...
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading Z_eicCodes.csv file ...
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading T_eicCodes.csv file ...
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading V_eicCodes.csv file ...
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading W_eicCodes.csv file ...
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading A_eicCodes.csv file ...

cli_h1("Germany Bidding Zone EIC")
#> 
#> ── Germany Bidding Zone EIC ────────────────────────────────────────────────────────────────────────────────────────────
cli_text(germany_eic)
#> 10Y1001A1001A82H
```

The
[`area_eic()`](https://krose.github.io/entsoeapi/reference/area_eic.md)
function provides a focused lookup for bidding zones:

``` r
# Get all bidding zones
bidding_zones <- area_eic()
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling Y_eicCodes.csv file from cache
cli_h1("Available Bidding Zones")
#> 
#> ── Available Bidding Zones ─────────────────────────────────────────────────────────────────────────────────────────────
cli_text("Total zones: {nrow(bidding_zones)}")
#> Total zones: 1791
bidding_zones |>
  select(eic_code, eic_long_name) |>
  head(10) |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> |eic_code         |eic_long_name                                       |
#> |:----------------|:---------------------------------------------------|
#> |10Y1001A1001A008 |Market coupling area Belgium - France - Netherlands |
#> |10Y1001A1001A016 |Northern Ireland                                    |
#> |10Y1001A1001A39I |Estonia                                             |
#> |10Y1001A1001A42T |CB Spain                                            |
#> |10Y1001A1001A44P |Swedish Elspot Area 1                               |
#> |10Y1001A1001A45N |Swedish Elspot Area 2                               |
#> |10Y1001A1001A46L |Swedish Elspot Area 3                               |
#> |10Y1001A1001A47J |Swedish Elspot Area 4                               |
#> |10Y1001A1001A48H |Norwegian Area Elspot Area 5                        |
#> |10Y1001A1001A49F |Russian area                                        |
```

## Your First API Call

Let’s fetch day-ahead energy prices for Germany. The
[`energy_prices()`](https://krose.github.io/entsoeapi/reference/energy_prices.md)
function queries the market data endpoint:

``` r
# Define time range (one week of data)
from_ts <- ymd("2026-01-01", tz = "CET")
till_ts <- from_ts + weeks(1L)

cli_h1("Querying Day-Ahead Energy Prices")
#> 
#> ── Querying Day-Ahead Energy Prices ────────────────────────────────────────────────────────────────────────────────────
cli_inform("Period: {from_ts} to {till_ts}")
#> Period: 2026-01-01 to 2026-01-08
cli_inform("Area: {germany_eic}")
#> Area: 10Y1001A1001A82H

# Fetch the data
da_prices <- energy_prices(
  eic = germany_eic,
  period_start = from_ts,
  period_end = till_ts,
  contract_type = "A01",  # Day-ahead
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A44&in_Domain=10Y1001A1001A82H&out_Domain=10Y1001A1001A82H&periodStart=202512312300&periodEnd=202601072300&contract_MarketAgreement.type=A01&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 31 Mar 2026 07:10:45 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Energy_Prices_202512312300-202601072300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <-
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling Y_eicCodes.csv file from cache
#> ✔ Additional eic names have been added!

cli_alert_success("Retrieved {nrow(da_prices)} rows")
#> ✔ Retrieved 1344 rows
```

### Understanding the Output

The `tidy_output = TRUE` format returns one row per data point:

``` r
# Examine the structure
glimpse(da_prices)
#> Rows: 1,344
#> Columns: 23
#> $ ts_in_domain_mrid                   <chr> "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A…
#> $ ts_in_domain_name                   <chr> "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Lu…
#> $ ts_out_domain_mrid                  <chr> "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A…
#> $ ts_out_domain_name                  <chr> "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Lu…
#> $ type                                <chr> "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44…
#> $ type_def                            <chr> "Price Document", "Price Document", "Price Document", "Price Document", "P…
#> $ market_agreement_type               <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01…
#> $ market_agreement_type_def           <chr> "Daily contract", "Daily contract", "Daily contract", "Daily contract", "D…
#> $ ts_auction_type                     <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01…
#> $ ts_auction_type_def                 <chr> "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", "I…
#> $ ts_business_type                    <chr> "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62…
#> $ ts_business_type_def                <chr> "Spot price", "Spot price", "Spot price", "Spot price", "Spot price", "Spo…
#> $ created_date_time                   <dttm> 2026-03-31 07:10:45, 2026-03-31 07:10:45, 2026-03-31 07:10:45, 2026-03-31…
#> $ revision_number                     <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_resolution                       <chr> "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "P…
#> $ ts_time_interval_start              <dttm> 2025-12-31 23:00:00, 2025-12-31 23:00:00, 2025-12-31 23:00:00, 2025-12-31…
#> $ ts_time_interval_end                <dttm> 2026-01-01 23:00:00, 2026-01-01 23:00:00, 2026-01-01 23:00:00, 2026-01-01…
#> $ ts_mrid                             <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ ts_point_dt_start                   <dttm> 2025-12-31 23:00:00, 2025-12-31 23:15:00, 2025-12-31 23:30:00, 2025-12-31…
#> $ ts_point_price_amount               <dbl> 51.28, 52.08, 63.00, 41.08, 37.07, 40.03, 36.01, 25.20, 34.03, 21.83, 13.0…
#> $ ts_currency_unit_name               <chr> "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR…
#> $ ts_price_measure_unit_name          <chr> "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH…
#> $ ts_classification_sequence_position <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,…
```

Key columns in the output:

| Column                     | Description                              |
|----------------------------|------------------------------------------|
| `ts_point_dt_start`        | Timestamp for each data point            |
| `ts_point_price_amount`    | Price value                              |
| `ts_resolution`            | Time resolution (e.g., PT60M for hourly) |
| `ts_business_type`         | Type of price (Spot price = A62)         |
| `ts_market_agreement_type` | Contract type (A01 = Daily contract)     |

### The Nested Alternative

With `tidy_output = FALSE`, the data is structured differently:

``` r
# Same query with nested output
da_prices_nested <- energy_prices(
  eic = germany_eic,
  period_start = from_ts,
  period_end = till_ts,
  contract_type = "A01",
  tidy_output = FALSE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A44&in_Domain=10Y1001A1001A82H&out_Domain=10Y1001A1001A82H&periodStart=202512312300&periodEnd=202601072300&contract_MarketAgreement.type=A01&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 31 Mar 2026 07:10:48 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Energy_Prices_202512312300-202601072300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <-
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!

glimpse(da_prices_nested)
#> Rows: 14
#> Columns: 22
#> $ ts_in_domain_mrid                   <chr> "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A…
#> $ ts_in_domain_name                   <chr> "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Lu…
#> $ ts_out_domain_mrid                  <chr> "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A…
#> $ ts_out_domain_name                  <chr> "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Lu…
#> $ type                                <chr> "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44", "A44…
#> $ type_def                            <chr> "Price Document", "Price Document", "Price Document", "Price Document", "P…
#> $ market_agreement_type               <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01…
#> $ market_agreement_type_def           <chr> "Daily contract", "Daily contract", "Daily contract", "Daily contract", "D…
#> $ ts_auction_type                     <chr> "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01", "A01…
#> $ ts_auction_type_def                 <chr> "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", "Implicit", "I…
#> $ ts_business_type                    <chr> "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62", "A62…
#> $ ts_business_type_def                <chr> "Spot price", "Spot price", "Spot price", "Spot price", "Spot price", "Spo…
#> $ created_date_time                   <dttm> 2026-03-31 07:10:48, 2026-03-31 07:10:48, 2026-03-31 07:10:48, 2026-03-31…
#> $ revision_number                     <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ ts_resolution                       <chr> "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "PT15M", "P…
#> $ ts_time_interval_start              <dttm> 2025-12-31 23:00:00, 2025-12-31 23:00:00, 2026-01-01 23:00:00, 2026-01-01 …
#> $ ts_time_interval_end                <dttm> 2026-01-01 23:00:00, 2026-01-01 23:00:00, 2026-01-02 23:00:00, 2026-01-02…
#> $ ts_mrid                             <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14
#> $ ts_point                            <list> [<tbl_df[96 x 2]>], [<tbl_df[96 x 2]>], [<tbl_df[96 x 2]>], [<tbl_df[96 x…
#> $ ts_currency_unit_name               <chr> "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR…
#> $ ts_price_measure_unit_name          <chr> "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MWH", "MW…
#> $ ts_classification_sequence_position <dbl> 2, 1, 1, 2, 2, 1, 2, 1, 2, 1, 1, 2, 1, 2
```

With nested output, each row represents a time period, and all data
points are stored in the `ts_point` list-column. This format preserves
the original API structure and can be more efficient for certain
operations.

## Common Patterns

### Setting Date Ranges

Use lubridate for robust date handling:

``` r
# Various date range examples
last_week <- ymd(Sys.Date()) - days(7L)
today <- ymd(Sys.Date())

# Month of data
month_start <- floor_date(Sys.Date(), "month") - months(1L)
month_end <- floor_date(Sys.Date(), "month") - days(1L)
```

### The One-Year Limit

Most ENTSO-E endpoints enforce a maximum query range of one year. The
package handles this transparently through automatic pagination:

``` r
# Query close to the one-year limit
year_start <- ymd("2024-01-01", tz = "CET")
year_end <- ymd("2024-12-31", tz = "CET")

# The package will handle pagination automatically if needed
# This may take longer for large datasets
cli_warn("Querying a full year - this may take a moment")

da_prices_year <- energy_prices(
  eic = germany_eic,
  period_start = year_start,
  period_end = year_end,
  contract_type = "A01",
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A44&in_Domain=10Y1001A1001A82H&out_Domain=10Y1001A1001A82H&periodStart=202312312300&periodEnd=202412302300&contract_MarketAgreement.type=A01&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 31 Mar 2026 07:10:52 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Energy_Prices_202312312300-202412302300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <-
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!

cli_alert_success("Retrieved {nrow(da_prices_year)} rows for the year")
#> ✔ Retrieved 43800 rows for the year
```

### Timezone Considerations

All timestamps are returned in UTC. Use lubridate to convert:

``` r
# Convert timestamps to CET/CEST
da_prices |>
  mutate(
    ts_point_dt_start_cet = with_tz(time = ts_point_dt_start, tzone = "CET")
  ) |>
  select(ts_point_dt_start, ts_point_dt_start_cet, ts_point_price_amount) |>
  head(n = 5L) |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> |ts_point_dt_start   |ts_point_dt_start_cet | ts_point_price_amount|
#> |:-------------------|:---------------------|---------------------:|
#> |2025-12-31 23:00:00 |2026-01-01 00:00:00   |                 51.28|
#> |2025-12-31 23:15:00 |2026-01-01 00:15:00   |                 52.08|
#> |2025-12-31 23:30:00 |2026-01-01 00:30:00   |                 63.00|
#> |2025-12-31 23:45:00 |2026-01-01 00:45:00   |                 41.08|
#> |2026-01-01 00:00:00 |2026-01-01 01:00:00   |                 37.07|
```

## What’s Next?

Now that you’ve made your first API call, explore these topics:

- **EIC Codes**: The
  [`area_eic()`](https://krose.github.io/entsoeapi/reference/area_eic.md),
  [`tie_line_eic()`](https://krose.github.io/entsoeapi/reference/tie_line_eic.md),
  and other EIC functions help you find codes for specific regions and
  entities
- **Time Series Restructuring**: Understanding the `tidy_output`
  parameter helps you work with the data effectively
- **Generation Mix**: Analyze generation by production type with
  [`gen_per_prod_type()`](https://krose.github.io/entsoeapi/reference/gen_per_prod_type.md)
- **Architecture**: See the package architecture vignette for details on
  how the package works internally
