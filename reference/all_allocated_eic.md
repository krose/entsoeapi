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
#> <- Content-Length: 67280758
#> <- Content-Type: application/octet-stream
#> <- Content-MD5: +pkG6/+kJwvu08Ww4zeXmw==
#> <- Last-Modified: Wed, 08 Apr 2026 01:15:11 GMT
#> <- Accept-Ranges: bytes
#> <- ETag: "0x8DE950C4828B1DB"
#> <- Vary: Origin
#> <- Server: Windows-Azure-Blob/1.0 Microsoft-HTTPAPI/2.0
#> <- x-ms-request-id: eb80444f-c01e-00f4-1c59-c7a01c000000
#> <- x-ms-version: 2014-02-14
#> <- x-ms-lease-status: unlocked
#> <- x-ms-lease-state: available
#> <- x-ms-blob-type: BlockBlob
#> <- Date: Wed, 08 Apr 2026 13:13:23 GMT
#> <- 
#> ✔ response has arrived
#> converting ■■■■■                             13% | ETA:  7s
#> converting ■■■■■■■■■■■■■■                    43% | ETA:  5s
#> converting ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  2s
#> converting ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s

dplyr::glimpse(eic_all)
#> Rows: 74,169
#> Columns: 17
#> $ revision_number                              <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", …
#> $ created_date_time                            <chr> "2026-04-08T01:15:10Z", "2026-04-08T01:15:10Z", "2026-04-08T01:15…
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
