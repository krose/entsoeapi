# Get Sharing of FRR Capacity (SO GL 190.1)

Sharing of Frequency Restoration Reserve capacity between areas. One
year range limit applies.

## Usage

``` r
sharing_of_frr_capacity(
  eic_acquiring = NULL,
  eic_connecting = NULL,
  process_type = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic_acquiring:

  Energy Identification Code of the acquiring domain

- eic_connecting:

  Energy Identification Code of the connecting domain

- process_type:

  type of reserve "A56" FRR "A46" RR

- period_start:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- period_end:

  POSIXct or YYYY-MM-DD HH:MM:SS format One year range limit applies

- tidy_output:

  Defaults to TRUE. flatten nested tables

- security_token:

  Security token for ENTSO-E transparency platform

## Examples

``` r
if (FALSE) { # \dontrun{
df <- entsoeapi::sharing_of_frr_capacity(
  eic_acquiring = "10YCB-GERMANY--8",
  eic_connecting = "10YAT-APG------L",
  process_type = "A56",
  period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
  tidy_output = TRUE
)

str(df)
} # }
```
