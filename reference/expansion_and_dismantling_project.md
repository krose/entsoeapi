# Get Expansion and Dismantling Projects (9.1)

Query the interconnector network evolution or dismantling

## Usage

``` r
expansion_and_dismantling_project(
  eic_in = NULL,
  eic_out = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  business_type = NULL,
  doc_status = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic_in:

  Energy Identification Code of in domain

- eic_out:

  Energy Identification Code of out domain

- period_start:

  the starting date of the in-scope period in POSIXct or YYYY-MM-DD
  HH:MM:SS format

- period_end:

  the ending date of the outage in-scope period in POSIXct or YYYY-MM-DD
  HH:MM:SS format

- business_type:

  "B01" = for interconnector network evolution "B02" = interconnector
  network dismantling

- doc_status:

  Notification document status. "A01" for intermediate "A02" for final
  "A05" for active, "A09" for cancelled "A13" for withdrawn "X01" for
  estimated

- tidy_output:

  Defaults to TRUE. If TRUE, then flatten nested tables.

- security_token:

  Security token for ENTSO-E transparency platform

## Value

A data frame with expansion or dismantling projects

## Examples

``` r
df <- entsoeapi::expansion_and_dismantling_project(
  eic_in = "10YSK-SEPS-----K",
  eic_out = "10YHU-MAVIR----U",
  period_start = lubridate::ymd(x = "2023-01-01", tz = "CET"),
  period_end = lubridate::ymd(x = "2023-01-02", tz = "CET"),
  business_type = "B01",
  doc_status = "A05",
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A90&in_Domain=10YSK-SEPS-----K&out_Domain=10YHU-MAVIR----U&periodStart=202212312300&periodEnd=202301012300&businessType=B01&DocStatus=A05&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 12 Mar 2026 09:22:37 GMT
#> <- content-type: text/xml
#> <- content-length: 984
#> <- content-disposition: inline; filename="acknowledgement.xml"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ℹ No additional type names added!
#> ℹ No additional eic names added!

dplyr::glimpse(df)
#> Rows: 1
#> Columns: 3
#> $ created_date_time <dttm> 2026-03-12 09:22:37
#> $ reason_code       <chr> "999"
#> $ reason_text       <chr> "No matching data found for Data item EXPANSION_AND_DISMANTLING_PROJECTS_R3 [9.1] (10YSK-SE…
```
