# Get all Allocated Energy Identification Codes

Beware, this is a REAL SLOW function, it runs for minutes, be patient!
This function downloads all allocated energy identification codes from
this link: https://eepublicdownloads.blob.core.windows.net/
cio-lio/xml/allocated-eic-codes.xml Further details are under:
https://www.entsoe.eu/data/energy-identification-codes-eic/ It is an
alternative of
[`all_approved_eic()`](https://krose.github.io/entsoeapi/reference/all_approved_eic.md)
function call providing more details.

## Usage

``` r
all_allocated_eic()
```

## Value

A tibble of all allocated EIC codes, which contains such columns as
`revision_number`, `created_date_time`, `eic_code`, `doc_status_value`,
`doc_status`, `instance_component_attribute`, `long_name`,
`display_name`, `last_request_date`,
`deactivation_requested_date_and_or_time_date`,
`eic_code_market_participant_street_address`,
`market_participant_vat_code_name`, `market_participant_acer_code_name`,
`description`, `responsible_market_participant_mrid`, `function_names`
and `parent_market_document_mrid`

## Examples

``` r
eic_all <- entsoeapi::all_allocated_eic()
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading all_allocated_eic table ...
#> <- HTTP/1.1 200 OK
#> <- Content-Length: 70338163
#> <- Content-Type: application/octet-stream
#> <- Content-MD5: mzK0y06ZJ+pj3hmgOxl5gQ==
#> <- Last-Modified: Fri, 16 Oct 2026 01:15:13 GMT
#> <- ETag: 0x8DF2B22EE4C4D72
#> <- Vary: Origin
#> <- Server: Windows-Azure-Blob/1.0 Microsoft-HTTPAPI/2.0
#> <- x-ms-request-id: 1cb54ac4-501e-00f6-523f-5d1ea4000000
#> <- x-ms-version: 2009-09-19
#> <- x-ms-lease-status: unlocked
#> <- x-ms-blob-type: BlockBlob
#> <- Date: Fri, 16 Oct 2026 07:21:57 GMT
#> <- 
#> ✔ response has arrived
#> converting ■■■■■                             13% | ETA:  7s
#> converting ■■■■■■■■■■                        29% | ETA:  6s
#> converting ■■■■■■■■■■■■■■■■■■■■■■            69% | ETA:  2s
#> converting ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s

dplyr::glimpse(eic_all)
#> Rows: 77,520
#> Columns: 17
#> $ revision_number                              <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", …
#> $ created_date_time                            <chr> "2026-10-16T01:15:12Z", "2026-10-16T01:15:12Z", "2026-10-16T01:15…
#> $ eic_code                                     <chr> "10T-1001-10010AS", "10T1001A1001A012", "10T1001A1001A020", "10T1…
#> $ doc_status_value                             <chr> "A05", "A05", "A05", "A05", "A05", "A05", "A05", "A05", "A05", "A…
#> $ doc_status                                   <chr> "Control block area schedule", "Control block area schedule", "Co…
#> $ instance_component_attribute                 <chr> "International", "International", "International", "International…
#> $ long_name                                    <chr> "Tie Line Koman-KosovoB", "Urosevac_Skopje", "Prizren_Fierza", "P…
#> $ display_name                                 <chr> "L_KOM-KOSB", "L_UROSEV_SKOPJE", "L_PRIZRE_FIERZA", "L_PEC_RIBARE…
#> $ last_request_date                            <chr> "2018-10-31", "2014-12-29", "2014-12-29", "2014-12-29", "2014-12-…
#> $ deactivation_requested_date_and_or_time_date <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ eic_code_market_participant_street_address   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ market_participant_vat_code_name             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ market_participant_acer_code_name            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ description                                  <chr> "Tieline", "Tieline", "Tieline", "Tieline", "Tieline", "Tieline",…
#> $ responsible_market_participant_mrid          <chr> "10XAL-KESH-----J", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ function_names                               <chr> "Tieline", "Tieline", "Tieline", "Tieline", "Tieline", "Tieline",…
#> $ parent_market_document_mrid                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
```
