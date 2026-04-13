# Time Series Restructuring Explained

``` r
library(entsoeapi)
suppressPackageStartupMessages(library(dplyr))
library(cli)
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(kableExtra))
library(tidyr)
```

## Introduction

The ENTSO-E API encodes time series data in a compact XML format that
differs from the typical “one row per observation” structure used in
most R analysis workflows. This vignette explains how the entsoeapi
package transforms this compact representation into analysis-ready
tibbles.

This vignette covers:

- The compact time series encoding problem
- The `tidy_output` parameter and its two formats
- Curve types A01 (fix sized blocks) and A03 (variable sized blocks)
- Supported time resolutions
- Practical examples for both output formats

## The Compact Encoding Problem

ENTSO-E XML responses encode time series efficiently:

``` xml
<Period>
  <resolution>PT60M</resolution>
  <timeInterval>
    <start>2024010100000</start>
    <end>2024010200000</end>
  </timeInterval>
  <Point>
    <position>1</position>
    <price.amount>150.25</price.amount>
  </Point>
  <Point>
    <position>2</position>
    <price.amount>152.10</price.amount>
  </Point>
  <!-- ... 23 more points for hourly data ... -->
</Period>
```

Instead of storing 24 timestamps, the API stores:

1.  A start time (`2024-01-01 00:00:00`)
2.  A resolution (`PT60M` = 60 minutes = 1 hour)
3.  Positions (1, 2, 3, …) mapped to actual times
4.  Values at each position

This compact format is efficient for data transmission but requires
reconstruction for analysis in R.

## The tidy_output Parameter

Most functions in entsoeapi accept a `tidy_output` parameter:

| Setting                        | Output Format                     | Best For                             |
|--------------------------------|-----------------------------------|--------------------------------------|
| `tidy_output = TRUE` (default) | One row per data point            | Analysis, plotting, aggregation      |
| `tidy_output = FALSE`          | One row per period, nested points | Large datasets, preserving structure |

### tidy_output = TRUE: One Row Per Point

With `tidy_output = TRUE`, each row represents a single data point:

``` r
# Define parameters
es_zone <- "10YES-REE------0"
from_ts <- ymd(x = "2024-01-01", tz = "CET")
till_ts <- from_ts + days(1L)

cli_h1("tidy_output = TRUE (Default)")
#> 
#> ── tidy_output = TRUE (Default) ────────────────────────────────────────────────────────────────────────────────────────

# Fetch with tidy output
da_prices_tidy <- energy_prices(
  eic = es_zone,
  period_start = from_ts,
  period_end = till_ts,
  contract_type = "A01",
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A44&in_Domain=10YES-REE------0&out_Domain=10YES-REE------0&periodStart=202312312300&periodEnd=202401012300&contract_MarketAgreement.type=A01&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:53:57 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Energy_Prices_202312312300-202401012300.xml"
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
#> ℹ downloading Y_eicCodes.csv file ...
#> ✔ Additional eic names have been added!

cli_text("Rows: {nrow(da_prices_tidy)}")
#> Rows: 24
cli_text("Columns: {ncol(da_prices_tidy)}")
#> Columns: 22

# Examine structure
da_prices_tidy |>
  mutate(
    ts_point_dt_start = with_tz(time = ts_point_dt_start, tzone = "CET")
  ) |>
  select(
    ts_point_dt_start,
    ts_point_price_amount,
    ts_resolution
  ) |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> |ts_point_dt_start   | ts_point_price_amount|ts_resolution |
#> |:-------------------|---------------------:|:-------------|
#> |2024-01-01 00:00:00 |                 63.33|PT60M         |
#> |2024-01-01 01:00:00 |                 50.09|PT60M         |
#> |2024-01-01 02:00:00 |                 47.50|PT60M         |
#> |2024-01-01 03:00:00 |                 43.50|PT60M         |
#> |2024-01-01 04:00:00 |                 42.50|PT60M         |
#> |2024-01-01 05:00:00 |                 42.09|PT60M         |
#> |2024-01-01 06:00:00 |                 42.50|PT60M         |
#> |2024-01-01 07:00:00 |                 42.59|PT60M         |
#> |2024-01-01 08:00:00 |                 43.37|PT60M         |
#> |2024-01-01 09:00:00 |                 42.29|PT60M         |
#> |2024-01-01 10:00:00 |                 25.00|PT60M         |
#> |2024-01-01 11:00:00 |                  3.90|PT60M         |
#> |2024-01-01 12:00:00 |                  3.20|PT60M         |
#> |2024-01-01 13:00:00 |                  2.06|PT60M         |
#> |2024-01-01 14:00:00 |                  1.73|PT60M         |
#> |2024-01-01 15:00:00 |                  5.72|PT60M         |
#> |2024-01-01 16:00:00 |                 18.49|PT60M         |
#> |2024-01-01 17:00:00 |                 37.00|PT60M         |
#> |2024-01-01 18:00:00 |                 47.50|PT60M         |
#> |2024-01-01 19:00:00 |                 54.97|PT60M         |
#> |2024-01-01 20:00:00 |                 60.90|PT60M         |
#> |2024-01-01 21:00:00 |                 60.00|PT60M         |
#> |2024-01-01 22:00:00 |                 47.50|PT60M         |
#> |2024-01-01 23:00:00 |                 42.09|PT60M         |
```

Key columns in tidy output:

| Column                  | Description                                |
|-------------------------|--------------------------------------------|
| `ts_point_dt_start`     | Reconstructed timestamp for each point     |
| `ts_point_price_amount` | The actual value                           |
| `ts_point_position`     | Original position in the series (internal) |

### tidy_output = FALSE: Nested Output

With `tidy_output = FALSE`, each row represents a time period with all
data points nested in a list-column:

``` r
cli_h1("tidy_output = FALSE (Nested)")
#> 
#> ── tidy_output = FALSE (Nested) ────────────────────────────────────────────────────────────────────────────────────────

# Fetch with nested output
da_prices_nested <- energy_prices(
  eic = es_zone,
  period_start = from_ts,
  period_end = till_ts,
  contract_type = "A01",
  tidy_output = FALSE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A44&in_Domain=10YES-REE------0&out_Domain=10YES-REE------0&periodStart=202312312300&periodEnd=202401012300&contract_MarketAgreement.type=A01&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:53:59 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Energy_Prices_202312312300-202401012300.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- vary: accept-encoding
#> <- content-encoding: gzip
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <-
#> ✔ response has arrived
#> ✔ Additional type names have been added!
#> ✔ Additional eic names have been added!

cli_text("Rows: {nrow(da_prices_nested)}")
#> Rows: 1
cli_text("Columns: {ncol(da_prices_nested)}")
#> Columns: 21

# Examine structure
da_prices_nested |>
  mutate(
    ts_time_interval_start = with_tz(
      time = ts_time_interval_start,
      tzone = "CET"
    )
  ) |>
  select(
    ts_time_interval_start,
    ts_resolution,
    ts_point
  ) |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> |ts_time_interval_start |ts_resolution |ts_point                                                                                                                                                                                                                                                                                                                         |
#> |:----------------------|:-------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
#> |2024-01-01             |PT60M         |1.00, 2.00, 3.00, 4.00, 5.00, 6.00, 7.00, 8.00, 9.00, 10.00, 11.00, 12.00, 13.00, 14.00, 15.00, 16.00, 17.00, 18.00, 19.00, 20.00, 21.00, 22.00, 23.00, 24.00, 63.33, 50.09, 47.50, 43.50, 42.50, 42.09, 42.50, 42.59, 43.37, 42.29, 25.00, 3.90, 3.20, 2.06, 1.73, 5.72, 18.49, 37.00, 47.50, 54.97, 60.90, 60.00, 47.50, 42.09 |
```

Each row contains a `ts_point` list-column with nested data:

``` r
# Extract first period's points
da_prices_nested$ts_point[[1]] |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> | ts_point_position| ts_point_price_amount|
#> |-----------------:|---------------------:|
#> |                 1|                 63.33|
#> |                 2|                 50.09|
#> |                 3|                 47.50|
#> |                 4|                 43.50|
#> |                 5|                 42.50|
#> |                 6|                 42.09|
#> |                 7|                 42.50|
#> |                 8|                 42.59|
#> |                 9|                 43.37|
#> |                10|                 42.29|
#> |                11|                 25.00|
#> |                12|                  3.90|
#> |                13|                  3.20|
#> |                14|                  2.06|
#> |                15|                  1.73|
#> |                16|                  5.72|
#> |                17|                 18.49|
#> |                18|                 37.00|
#> |                19|                 47.50|
#> |                20|                 54.97|
#> |                21|                 60.90|
#> |                22|                 60.00|
#> |                23|                 47.50|
#> |                24|                 42.09|
```

### When to Use Each Format

Use **tidy output** when you need to:

- Plot time series directly with ggplot2
- Aggregate to different time grains (hourly, daily, weekly)
- Filter by specific timestamps
- Join with other tidy data
- Perform standard dplyr operations

Use **nested output** when you need to:

- Work with large datasets efficiently
- Preserve the original API structure
- Perform operations on entire periods at once
- Work with hierarchical/nested data structures

## Curve Types

ENTSO-E distinguishes between five curve types that affect how data is
structured.

- A01 – SEQUENTIAL FIXED SIZE BLOCKS
- A02 – POINT
- A03 – VARIABLE SIZED BLOCK
- A04 – OVERLAPPING BREAKPOINT
- A05 – NON-OVERLAPPING BREAKPOINT

See details
[here](https://eepublicdownloads.entsoe.eu/clean-documents/EDI/Library/cim_based/Introduction_of_different_Timeseries_possibilities__curvetypes__with_ENTSO-E_electronic_document_v1.4.pdf).

We implemented the processing of A01 and A03 curve types so far. These 2
proved to be sufficient till this point.

### A01: Sequential Fixed Size Blocks

Curve type A01 represents regular, evenly-spaced data points:

``` xml
<mRID>1</mRID>
<businessType>A01</businessType>
<objectAggregation>A08</objectAggregation>
<outBiddingZone_Domain.mRID codingScheme="A01">10YFR-RTE------C</outBiddingZone_Domain.mRID>
<quantity_Measure_Unit.name>MAW</quantity_Measure_Unit.name>
<curveType>A01</curveType>
<MktPSRType> <psrType>B10</psrType> </MktPSRType>
<Period>
  <timeInterval> <start>2020-01-31T23:00Z</start> <end>2020-02-01T23:00Z</end> </timeInterval>
  <resolution>PT60M</resolution>
  <Point> <position>1</position> <quantity>1466</quantity> </Point>
  <Point> <position>2</position> <quantity>2023</quantity> </Point>
  <Point> <position>3</position> <quantity>2365</quantity> </Point>
  <Point> <position>4</position> <quantity>3027</quantity> </Point>
  <Point> <position>5</position> <quantity>3247</quantity> </Point>
  <Point> <position>6</position> <quantity>3179</quantity> </Point>
  <Point> <position>7</position> <quantity>2871</quantity> </Point>
  <Point> <position>8</position> <quantity>2179</quantity> </Point>
  <Point> <position>9</position> <quantity>1152</quantity> </Point>
  <Point> <position>10</position> <quantity>395</quantity> </Point>
  <Point> <position>11</position> <quantity>67</quantity> </Point>
  <Point> <position>12</position> <quantity>75</quantity> </Point>
  <Point> <position>13</position> <quantity>494</quantity> </Point>
  <Point> <position>14</position> <quantity>1297</quantity> </Point>
  <Point> <position>15</position> <quantity>2230</quantity> </Point>
  <Point> <position>16</position> <quantity>2798</quantity> </Point>
  <Point> <position>17</position> <quantity>2897</quantity> </Point>
  <Point> <position>18</position> <quantity>1572</quantity> </Point>
  <Point> <position>19</position> <quantity>1033</quantity> </Point>
  <Point> <position>20</position> <quantity>1070</quantity> </Point>
  <Point> <position>21</position> <quantity>1587</quantity> </Point>
  <Point> <position>22</position> <quantity>2058</quantity> </Point>
  <Point> <position>23</position> <quantity>1641</quantity> </Point>
  <Point> <position>24</position> <quantity>547</quantity> </Point>
</Period>
```

A01 data has consistent intervals between points (e.g., every 60
minutes).

### A03: Variable Sized Blocks

Curve type A03 represents variable sized block data where some positions
may be absent:

``` xml
<mRID>1</mRID>
<businessType>A01</businessType>
<objectAggregation>A08</objectAggregation>
<outBiddingZone_Domain.mRID codingScheme="A01">10YFR-RTE------C</outBiddingZone_Domain.mRID>
<quantity_Measure_Unit.name>MAW</quantity_Measure_Unit.name>
<curveType>A03</curveType>
<MktPSRType> <psrType>B10</psrType> </MktPSRType>
<Period>
  <timeInterval> <start>2020-01-31T23:00Z</start> <end>2020-02-01T23:00Z</end> </timeInterval>
  <resolution>PT60M</resolution>
  <Point> <position>1</position> <quantity>1466</quantity> </Point>
  <Point> <position>2</position> <quantity>2023</quantity> </Point>
  <Point> <position>3</position> <quantity>2365</quantity> </Point>
  <Point> <position>4</position> <quantity>3027</quantity> </Point>
  <Point> <position>7</position> <quantity>2871</quantity> </Point>
  <Point> <position>8</position> <quantity>2179</quantity> </Point>
  <Point> <position>9</position> <quantity>1152</quantity> </Point>
  <Point> <position>10</position> <quantity>395</quantity> </Point>
  <Point> <position>11</position> <quantity>67</quantity> </Point>
  <Point> <position>13</position> <quantity>494</quantity> </Point>
  <Point> <position>14</position> <quantity>1297</quantity> </Point>
  <Point> <position>15</position> <quantity>2230</quantity> </Point>
  <Point> <position>16</position> <quantity>2798</quantity> </Point>
  <Point> <position>17</position> <quantity>2897</quantity> </Point>
  <Point> <position>18</position> <quantity>1572</quantity> </Point>
  <Point> <position>19</position> <quantity>1033</quantity> </Point>
  <Point> <position>21</position> <quantity>1587</quantity> </Point>
  <Point> <position>22</position> <quantity>2058</quantity> </Point>
  <Point> <position>23</position> <quantity>1641</quantity> </Point>
  <Point> <position>24</position> <quantity>547</quantity> </Point>
</Period>
```

A03 data may have gaps at certain positions

The package automatically handles A03 data by:

1.  Building a complete positional frame
2.  Performing a full join to identify gaps
3.  Carrying forward the last observed value (LOCF) to fill gaps

## Supported Time Resolutions

The ENTSO-E API supports various time resolutions:

| Resolution Code | Duration   | Typical Use                  |
|-----------------|------------|------------------------------|
| `PT4S`          | 4 seconds  | Automatic generation control |
| `PT1M`          | 1 minute   | Fast frequency response      |
| `PT15M`         | 15 minutes | Intraday markets             |
| `PT30M`         | 30 minutes | Half-hourly markets          |
| `PT60M`         | 1 hour     | Hourly day-ahead             |
| `P1D`           | 1 day      | Daily data                   |
| `P7D`           | 1 week     | Weekly data                  |
| `P1M`           | 1 month    | Monthly data                 |
| `P1Y`           | 1 year     | Yearly data                  |

The package automatically calculates timestamps based on:

    timestamp = period_start + (position - 1) × resolution

### Resolution Examples

``` r
cli_h1("Time Resolution Examples")
#> 
#> ── Time Resolution Examples ────────────────────────────────────────────────────────────────────────────────────────────

# Show data grouped by resolution
da_prices_tidy |>
  summarize(
    points = n(),
    start = min(ts_point_dt_start),
    end = max(ts_point_dt_start),
    .by = ts_resolution
  ) |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> |ts_resolution | points|start               |end                 |
#> |:-------------|------:|:-------------------|:-------------------|
#> |PT60M         |     24|2023-12-31 23:00:00 |2024-01-01 22:00:00 |
```

## Practical Examples

### Aggregating Hourly to Daily

With tidy output, aggregation is straightforward:

``` r
cli_h1("Aggregating to Daily Values")
#> 
#> ── Aggregating to Daily Values ─────────────────────────────────────────────────────────────────────────────────────────

da_prices_tidy |>
  mutate(date = as.Date(x = ts_point_dt_start, tz = "CET")) |>
  summarize(
    min_price = min(ts_point_price_amount),
    max_price = max(ts_point_price_amount),
    mean_price = mean(ts_point_price_amount),
    n_points = n(),
    .by = date
  ) |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> |date       | min_price| max_price| mean_price| n_points|
#> |:----------|---------:|---------:|----------:|--------:|
#> |2024-01-01 |      1.73|     63.33|    36.2425|       24|
```

### Working with Nested Output

Extract and process nested points:

``` r
cli_h1("Processing Nested Points")
#> 
#> ── Processing Nested Points ────────────────────────────────────────────────────────────────────────────────────────────

# Get first period's points
first_period_points <- da_prices_nested |>
  mutate(n_points = lengths(ts_point)) |>
  select(ts_time_interval_start, n_points, ts_point) |>
  slice(1)

# Unnest the points
first_period_points |>
  unnest(ts_point) |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> |ts_time_interval_start | n_points| ts_point_position| ts_point_price_amount|
#> |:----------------------|--------:|-----------------:|---------------------:|
#> |2023-12-31 23:00:00    |        2|                 1|                 63.33|
#> |2023-12-31 23:00:00    |        2|                 2|                 50.09|
#> |2023-12-31 23:00:00    |        2|                 3|                 47.50|
#> |2023-12-31 23:00:00    |        2|                 4|                 43.50|
#> |2023-12-31 23:00:00    |        2|                 5|                 42.50|
#> |2023-12-31 23:00:00    |        2|                 6|                 42.09|
#> |2023-12-31 23:00:00    |        2|                 7|                 42.50|
#> |2023-12-31 23:00:00    |        2|                 8|                 42.59|
#> |2023-12-31 23:00:00    |        2|                 9|                 43.37|
#> |2023-12-31 23:00:00    |        2|                10|                 42.29|
#> |2023-12-31 23:00:00    |        2|                11|                 25.00|
#> |2023-12-31 23:00:00    |        2|                12|                  3.90|
#> |2023-12-31 23:00:00    |        2|                13|                  3.20|
#> |2023-12-31 23:00:00    |        2|                14|                  2.06|
#> |2023-12-31 23:00:00    |        2|                15|                  1.73|
#> |2023-12-31 23:00:00    |        2|                16|                  5.72|
#> |2023-12-31 23:00:00    |        2|                17|                 18.49|
#> |2023-12-31 23:00:00    |        2|                18|                 37.00|
#> |2023-12-31 23:00:00    |        2|                19|                 47.50|
#> |2023-12-31 23:00:00    |        2|                20|                 54.97|
#> |2023-12-31 23:00:00    |        2|                21|                 60.90|
#> |2023-12-31 23:00:00    |        2|                22|                 60.00|
#> |2023-12-31 23:00:00    |        2|                23|                 47.50|
#> |2023-12-31 23:00:00    |        2|                24|                 42.09|
```

### Timezone Conversions

All timestamps are returned in UTC. Convert to your timezone:

``` r
library(lubridate)

cli_h1("Timezone Conversions")
#> 
#> ── Timezone Conversions ────────────────────────────────────────────────────────────────────────────────────────────────

da_prices_tidy |>
  mutate(
    utc = ts_point_dt_start,
    cet = with_tz(time = ts_point_dt_start, tzone = "CET"),
    est = with_tz(time = ts_point_dt_start, tzone = "America/New_York")
  ) |>
  select(utc, cet, est, ts_point_price_amount) |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> |utc                 |cet                 |est                 | ts_point_price_amount|
#> |:-------------------|:-------------------|:-------------------|---------------------:|
#> |2023-12-31 23:00:00 |2024-01-01 00:00:00 |2023-12-31 18:00:00 |                 63.33|
#> |2024-01-01 00:00:00 |2024-01-01 01:00:00 |2023-12-31 19:00:00 |                 50.09|
#> |2024-01-01 01:00:00 |2024-01-01 02:00:00 |2023-12-31 20:00:00 |                 47.50|
#> |2024-01-01 02:00:00 |2024-01-01 03:00:00 |2023-12-31 21:00:00 |                 43.50|
#> |2024-01-01 03:00:00 |2024-01-01 04:00:00 |2023-12-31 22:00:00 |                 42.50|
#> |2024-01-01 04:00:00 |2024-01-01 05:00:00 |2023-12-31 23:00:00 |                 42.09|
#> |2024-01-01 05:00:00 |2024-01-01 06:00:00 |2024-01-01 00:00:00 |                 42.50|
#> |2024-01-01 06:00:00 |2024-01-01 07:00:00 |2024-01-01 01:00:00 |                 42.59|
#> |2024-01-01 07:00:00 |2024-01-01 08:00:00 |2024-01-01 02:00:00 |                 43.37|
#> |2024-01-01 08:00:00 |2024-01-01 09:00:00 |2024-01-01 03:00:00 |                 42.29|
#> |2024-01-01 09:00:00 |2024-01-01 10:00:00 |2024-01-01 04:00:00 |                 25.00|
#> |2024-01-01 10:00:00 |2024-01-01 11:00:00 |2024-01-01 05:00:00 |                  3.90|
#> |2024-01-01 11:00:00 |2024-01-01 12:00:00 |2024-01-01 06:00:00 |                  3.20|
#> |2024-01-01 12:00:00 |2024-01-01 13:00:00 |2024-01-01 07:00:00 |                  2.06|
#> |2024-01-01 13:00:00 |2024-01-01 14:00:00 |2024-01-01 08:00:00 |                  1.73|
#> |2024-01-01 14:00:00 |2024-01-01 15:00:00 |2024-01-01 09:00:00 |                  5.72|
#> |2024-01-01 15:00:00 |2024-01-01 16:00:00 |2024-01-01 10:00:00 |                 18.49|
#> |2024-01-01 16:00:00 |2024-01-01 17:00:00 |2024-01-01 11:00:00 |                 37.00|
#> |2024-01-01 17:00:00 |2024-01-01 18:00:00 |2024-01-01 12:00:00 |                 47.50|
#> |2024-01-01 18:00:00 |2024-01-01 19:00:00 |2024-01-01 13:00:00 |                 54.97|
#> |2024-01-01 19:00:00 |2024-01-01 20:00:00 |2024-01-01 14:00:00 |                 60.90|
#> |2024-01-01 20:00:00 |2024-01-01 21:00:00 |2024-01-01 15:00:00 |                 60.00|
#> |2024-01-01 21:00:00 |2024-01-01 22:00:00 |2024-01-01 16:00:00 |                 47.50|
#> |2024-01-01 22:00:00 |2024-01-01 23:00:00 |2024-01-01 17:00:00 |                 42.09|
```

### Handling Missing Points

With tidy output, missing points are already handled:

``` r
cli_h1("Checking for Missing Data")
#> 
#> ── Checking for Missing Data ───────────────────────────────────────────────────────────────────────────────────────────

# Check for NA values
na_count <- is.na(da_prices_tidy$ts_point_price_amount) |>
  sum()
cli_text("NA values in price column: {na_count}")
#> NA values in price column: 0

# For A03 data, the package fills gaps with LOCF
# Check if any positions had gaps filled
if ("ts_curve_type" %in% names(da_prices_tidy)) {
  a03_count <- sum(da_prices_tidy$ts_curve_type == "A03", na.rm = TRUE)
  if (a03_count > 0) {
    cli_inform("A03 data: gaps filled using last observation carried forward")
  }
}
```

## Summary

The `tidy_output` parameter controls how time series data is structured:

| Format  | Rows           | Best For                         |
|---------|----------------|----------------------------------|
| `TRUE`  | One per point  | Analysis, plotting, aggregation  |
| `FALSE` | One per period | Large data, preserving structure |

The package handles curve types (A01 fix sized blocks, A03 variable
sized blocks) and all common time resolutions automatically.
