# Working with EIC Codes

``` r
library(entsoeapi)
suppressPackageStartupMessages(library(dplyr))
library(cli)
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(kableExtra))
```

## Introduction

Energy Identification Codes (EICs) are 16-character alphanumeric codes
that uniquely identify market participants, bidding zones, transmission
lines, and other entities on the ENTSO-E platform. Understanding EICs is
essential for querying data through the entsoeapi package.

This vignette covers:

- EIC code structure and validation
- The eight EIC lookup functions
- Choosing between
  [`all_approved_eic()`](https://krose.github.io/entsoeapi/reference/all_approved_eic.md)
  and
  [`all_allocated_eic()`](https://krose.github.io/entsoeapi/reference/all_allocated_eic.md)
- Practical examples for finding and using EIC codes

## EIC Code Structure

### The 16-Character Format

EIC codes follow a specific format:

    10YDE-VE-------2
    ^^^^ ^^ ^^^^^^^
     ||  ||     ||
     ||  ||     ++-- Checksum (weighted-modulo-37)
     ||  ++-------- Country/area code
     ++------------- Type code

Example: `10YDE-VE-------2`

- `10Y` - Type code for bidding zone (Y = area)
- `DE` - Country code (Germany)
- `VE` - Area code
- `-------` - Filler characters
- `2` - Checksum character

### EIC Type Codes

The first three characters indicate the EIC type:

| Type Code | Description                   | Example                 |
|-----------|-------------------------------|-------------------------|
| `10Y`     | Bidding zones / control areas | `10YDE-VE-------2`      |
| `10X`     | Market participants           | `10X1001A1001A42F`      |
| `10Z`     | Interconnectors / tie lines   | `10Z-DE-AT---------W`   |
| `10W`     | Power system resources        | `10WGRAR-10ZDE-ENBW--Q` |
| `10V`     | Locations                     | `10V1001A1001A48H`      |
| `10T`     | Substations                   | `10T-DE-VE-------Q`     |
| `11Y`     | Accounting points             | `11YDE-1001A0001K`      |

### Checksum Validation

The package automatically validates EIC codes using the ENTSO-E
weighted-modulo-37 algorithm. Invalid codes will produce an error:

``` r
# EIC with not valid checksum - will fail
try(
  expr = energy_prices(
    eic = "ABCDEF1234567890",
    period_start = ymd("2026-01-01", tz = "CET"),
    period_end = ymd("2026-01-02", tz = "CET"),
    contract_type = "A07",
    tidy_output = TRUE
  )
)
#> Error in assert_eic(eic = eic) : 
#>   Invalid EIC checksum character in `eic`
#> ✖ Expected "G", got "0"
```

## The Eight EIC Lookup Functions

The entsoeapi package provides eight functions to look up EIC codes,
organized by entity type:

### area_eic() - Bidding Zones

Returns all bidding zones and control areas:

``` r
# Get all bidding zones
zones <- area_eic()
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading Y_eicCodes.csv file ...

cli_h1("Bidding Zones")
#> 
#> ── Bidding Zones ───────────────────────────────────────────────────────────────────────────────────────────────────────
cli_text("Total zones: {nrow(zones)}")
#> Total zones: 1791

# Find specific countries
zones |>
  filter(
    grepl(
      pattern = "Switzerland|Schweiz|Swiss",
      x = eic_long_name,
      ignore.case = TRUE
    )
  ) |>
  select(eic_code, eic_long_name, type) |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> |eic_code         |eic_long_name                                                   |type |
#> |:----------------|:---------------------------------------------------------------|:----|
#> |10Y1001A1001A68B |Italy North_Switzerland                                         |Y    |
#> |10Y1001C--000069 |Border Area DE-TransnetBW-Switzerland                           |Y    |
#> |10Y1001C--000085 |Border Area DE-Amprion-Switzerland                              |Y    |
#> |10Y1001C--00037Z |European Single Market Area, EEA plus Switzerland               |Y    |
#> |10Y1001C--00089G |Border Domain Swissgrid-TERRE Western Europe                    |Y    |
#> |10YCB-SWITZERL-D |Control Block  Switzerland                                      |Y    |
#> |10YCH-SWISSGRIDZ |Switzerland                                                     |Y    |
#> |10YDOM-1001A0060 |Border area Switzerland Italy                                   |Y    |
#> |10YDOM-1001A061T |Border between Italy and France, Austria, Slovenia, Switzerland |Y    |
#> |10YDOM-1010A009U |Border Area Switzerland Austria                                 |Y    |
#> |10YDOM-1010A0124 |Border area Germany-Switzerland                                 |Y    |
#> |10YDOM--CH-FR--W |Border area Switzerland  France                                 |Y    |
#> |21Y000000000142C |Aggregated exit to Switzerland                                  |Y    |
```

### party_eic() - Market Participants

Returns market participants (generators, traders, etc.):

``` r
# Get market participants
parties <- party_eic()
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading X_eicCodes.csv file ...

cli_h1("Market Participants")
#> 
#> ── Market Participants ─────────────────────────────────────────────────────────────────────────────────────────────────
cli_text("Total parties: {nrow(parties)}")
#> Total parties: 14917

# Find German TSOs
parties |>
  filter(
    grepl(
      pattern = "Switzerland|Schweiz|Swiss",
      x = eic_long_name,
      ignore.case = TRUE
    )
  ) |>
  select(eic_code, eic_long_name, type) |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> |eic_code         |eic_long_name                              |type |
#> |:----------------|:------------------------------------------|:----|
#> |10XCH-SWISSGRIDC |Swissgrid AG                               |X    |
#> |11XSBB---------H |Schweizerische Bundesbahnen SBB            |X    |
#> |12X-0000001852-R |KTG Swiss SA                               |X    |
#> |12X-0000001861-Q |Swissgrid AG                               |X    |
#> |12X-0000001909-L |ENGELHART CTP (SWITZERLAND) SA             |X    |
#> |12X-0000001959-1 |AOT Energy Switzerland AG                  |X    |
#> |12X-0000001966-6 |Illumia Swiss SA                           |X    |
#> |12X-0000001998-P |Swisselectricity.com SA                    |X    |
#> |12X-0000002005-Z |HYPER ENERGY SWITZERLAND AG                |X    |
#> |12X-0000002017-P |SEFE Marketing & Trading Switzerland AG    |X    |
#> |12X-0000002026-O |Somos (Switzerland) Sàrl                   |X    |
#> |12X-0000002032-W |Swissgas AG                                |X    |
#> |12X-0000002060-Q |enSwiss Energy Trading AG                  |X    |
#> |12X-0000002065-B |Energie Genossenschaft Schweiz             |X    |
#> |12X-000000208Z-O |BEAUFORT ENERGY TRADING SWITZERLAND LP     |X    |
#> |12X-000000209T-1 |STG Switzerland GmbH                       |X    |
#> |12X-000000209W-T |COUNT Energy Trading Switzerland GmbH      |X    |
#> |12X-00000020A3-1 |Weststream Switzerland AG                  |X    |
#> |12X-00000020A5-W |Swisspower Green Gas AG                    |X    |
#> |12XALSTOM-TURBOJ |ALSTOM (Schweiz) AG                        |X    |
#> |12XCKW-HANDEL--K |Centralschweizerische Kraftwerke AG        |X    |
#> |12XEFT-SWITZERLR |Energy Financing Team (Switzerland) AG     |X    |
#> |12XKKW-BEZNAU--0 |Nordostschweizerische Kraftwerke AG        |X    |
#> |12XNOK-HANDEL--Z |Nordostschweizerische Kraftwerke AG Handel |X    |
#> |12XSBBKRAFTWERKU |Schweizerische Bundesbahnen SBB            |X    |
#> |19XENERGYPOLSKAQ |Ergo Swiss Sp. z o. o.                     |X    |
#> |21X0000000012582 |GAZPROM SCHWEIZ AG                         |X    |
#> |21X-CH-A-A0A0A-Q |FluxSwiss Sagl                             |X    |
#> |21X-CH-B-A0A0A-H |Swissgas                                   |X    |
#> |23X--140203SSW-H |Shell Switzerland                          |X    |
#> |23X--160205-S--7 |SET Swiss Energy Trading AG                |X    |
#> |23X--160219-EZ-Z |Erdgas Zentralschweiz AG                   |X    |
#> |27XSWISS-ENERGOB |SWISS ENERGO s.r.o.                        |X    |
#> |34X-0000000014-L |Swiss KTG d.o.o.                           |X    |
#> |59X000000000003K |SWISS GAS & LIGHT GBMH                     |X    |
#> |59X0000000000179 |KVK SWISS ITALIA S.R.L.                    |X    |
#> |59X0000000000187 |KVK SWISS AG                               |X    |
#> |54X-SW-101B-P01Y |Swiss Balancing Pool sh.pk                 |X    |
#> |12X-00000020B2-0 |Onyx Commodities Switzerland AG            |X    |
#> |12X-00000020BV-O |Swiss Statistical Design&Innovation Sàrl   |X    |
#> |23X--260107--S-L |Swiss Solar Park L.L.C                     |X    |
```

### accounting_point_eic() - Accounting Points

Returns accounting point EICs:

``` r
acc_points <- accounting_point_eic()
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading Z_eicCodes.csv file ...

cli_h1("Accounting Points")
#> 
#> ── Accounting Points ───────────────────────────────────────────────────────────────────────────────────────────────────
cli_text("Total accounting points: {nrow(acc_points)}")
#> Total accounting points: 2519

# Sample entries
acc_points |>
  filter(
    grepl(
      pattern = "Switzerland|Schweiz|Swiss",
      x = eic_long_name,
      ignore.case = TRUE
    )
  ) |>
  select(eic_code, eic_long_name, type) |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> |eic_code         |eic_long_name                                                      |type |
#> |:----------------|:------------------------------------------------------------------|:----|
#> |21Z0000000004960 |VIP Germany-Switzerland                                            |Z    |
#> |21Z987265839283C |RC Ostschweiz, temporary EIC to be used in the Incremental process |Z    |
#> |10ZAT-CH-001A24Z |Virtual Gemeinschaftskraftwerk Inn IWA Anteil Schweiz              |Z    |
```

### tie_line_eic() - Interconnectors

Returns transmission lines between bidding zones:

``` r
tie_lines <- tie_line_eic()
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading T_eicCodes.csv file ...

cli_h1("Tie Lines (Interconnectors)")
#> 
#> ── Tie Lines (Interconnectors) ─────────────────────────────────────────────────────────────────────────────────────────
cli_text("Total interconnectors: {nrow(tie_lines)}")
#> Total interconnectors: 12985

# Find German interconnectors
tie_lines |>
  filter(
    grepl(
      pattern = "Switzerland|Schweiz|Swiss",
      x = eic_long_name,
      ignore.case = TRUE
    )
  ) |>
  select(eic_code, eic_long_name, type) |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> |eic_code         |eic_long_name                                         |type |
#> |:----------------|:-----------------------------------------------------|:----|
#> |14TAPG-RD-SWISS3 |Virtual Tieline APG-SWISSGRID                         |T    |
#> |10TAT-CH-001A248 |Virtual Gemeinschaftskraftwerk Inn IWA Anteil Schweiz |T    |
```

### location_eic() - Locations

Returns location EICs (V codes):

``` r
locations <- location_eic()
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading V_eicCodes.csv file ...

cli_h1("Locations")
#> 
#> ── Locations ───────────────────────────────────────────────────────────────────────────────────────────────────────────
cli_text("Total locations: {nrow(locations)}")
#> Total locations: 1051

locations |>
  filter(
    grepl(
      pattern = "Switzerland|Schweiz|Swiss",
      x = eic_long_name,
      ignore.case = TRUE
    )
  ) |>
  select(eic_code, eic_long_name, type) |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> |eic_code         |eic_long_name |type |
#> |:----------------|:-------------|:----|
#> |12V-ECP-00009V-C |Swissgrid AG  |V    |
```

### resource_object_eic() - Power Resources

Returns power system resources (generating units, loads):

``` r
resources <- resource_object_eic()
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading W_eicCodes.csv file ...

cli_h1("Power Resources")
#> 
#> ── Power Resources ─────────────────────────────────────────────────────────────────────────────────────────────────────
cli_text("Total resources: {nrow(resources)}")
#> Total resources: 34671

# Find German power plants
resources |>
  filter(
    grepl(
      pattern = "KW-DE|DE-TU",
      x = eic_long_name,
      ignore.case = TRUE
    )
  ) |>
  select(eic_code, eic_long_name, type) |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> |eic_code         |eic_long_name               |type |
#> |:----------------|:---------------------------|:----|
#> |11WD2GBRD0002415 |Braunau-Simbach-G1-DE-TU    |W    |
#> |11WD2GBRK000231Q |Braunau-Simbach-KW-DE       |W    |
#> |11WD2GEOD000253W |Egglfing-Obernberg-G1-DE-TU |W    |
#> |11WD2GEOK000237B |Egglfing-Obernberg-KW-DE    |W    |
#> |11WD2GERD0002519 |Ering-Frauenstein-G1-DE-TU  |W    |
#> |11WD2GERK000236N |Ering-Frauenstein-KW-DE     |W    |
#> |11WD2GJSD000249B |Jochenstein-G1-DE-TU        |W    |
#> |11WD2GJSK0002353 |Jochenstein-KW-DE           |W    |
#> |11WD2GNUD000238W |Nussdorf-G1-DE-TU           |W    |
#> |11WD2GNUK000230T |Nussdorf-KW-DE              |W    |
#> |11WD2GOED000245L |Oberaudorf-Ebbs-G1-DE-TU    |W    |
#> |11WD2GOEK0002339 |Oberaudorf-Ebbs-KW-DE       |W    |
#> |11WD2GPSD000247T |Passau-Ingling-G1-DE-TU     |W    |
#> |11WD2GPSK000234J |Passau-Ingling-KW-DE        |W    |
#> |11WD2GSDD000243V |Schaerding-Neuhaus-G1-DE-TU |W    |
#> |11WD2GSDK000232H |Schaerding-Neuhaus-KW-DE    |W    |
```

### substation_eic() - Substations

Returns substation EICs (A codes):

``` r
substations <- substation_eic()
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading A_eicCodes.csv file ...

cli_h1("Substations")
#> 
#> ── Substations ─────────────────────────────────────────────────────────────────────────────────────────────────────────
cli_text("Total substations: {nrow(substations)}")
#> Total substations: 2848

# Sample entries
substations |>
  head(12L) |>
  select(eic_code, eic_long_name, type) |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> |eic_code         |eic_long_name          |type |
#> |:----------------|:----------------------|:----|
#> |11A0-0000-0001-W |USWKSDON00000000       |A    |
#> |11A0-0000-0002-T |USWKLAGE00000000       |A    |
#> |11A0-0000-0004-N |USWKHAND00000000       |A    |
#> |11A0-0000-0005-K |USWKELWE00000000       |A    |
#> |11A0-0000-0007-E |USWKSHUW00000000       |A    |
#> |11A0-0000-0008-B |USWKWILW00000000       |A    |
#> |11A0-0000-0017-A |USWKFEDD00000000       |A    |
#> |11A0-0000-0018-7 |USWKMEHN00000000       |A    |
#> |11A0-0000-0019-4 |UW-KLIXBUELL/S         |A    |
#> |11A0-0000-0020-R |UW-CAPPELN/W           |A    |
#> |11A0-0000-0021-O |UW-GARREL/O            |A    |
#> |11A0-0000-0022-L |UW-HOLLANDSE KUST ZUID |A    |
```

### all_approved_eic() - All Approved EICs

Combines all approved EICs into a single tibble:

``` r
all_eic <- all_approved_eic()
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling X_eicCodes.csv file from cache
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling Y_eicCodes.csv file from cache
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling Z_eicCodes.csv file from cache
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling T_eicCodes.csv file from cache
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling V_eicCodes.csv file from cache
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling W_eicCodes.csv file from cache
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling A_eicCodes.csv file from cache

cli_h1("All Approved EICs")
#> 
#> ── All Approved EICs ───────────────────────────────────────────────────────────────────────────────────────────────────
cli_text("Total EICs: {nrow(all_eic)}")
#> Total EICs: 70782

# Count by type
all_eic |>
  count(type, sort = TRUE) |>
  mutate(pct = round(x = n / sum(n) * 100, digits = 2L)) |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> |type |     n|   pct|
#> |:----|-----:|-----:|
#> |W    | 34671| 48.98|
#> |X    | 14917| 21.07|
#> |T    | 12985| 18.35|
#> |A    |  2848|  4.02|
#> |Z    |  2519|  3.56|
#> |Y    |  1791|  2.53|
#> |V    |  1051|  1.48|
```

## all_approved_eic() vs all_allocated_eic()

The package provides two comprehensive EIC functions with different
characteristics:

### Comparison Table

| Feature          | all_approved_eic() | all_allocated_eic() |
|------------------|--------------------|---------------------|
| Data source      | CSV downloads      | XML download        |
| Speed            | Fast (seconds)     | Slow (minutes)      |
| Update frequency | Less frequent      | More current        |
| Columns          | Standardized       | Extended details    |
| Columns returned | 11 columns         | 18 columns          |

### all_approved_eic()

Fast and efficient for most use cases:

``` r
cli_h1("all_approved_eic() - Column Details")
#> 
#> ── all_approved_eic() - Column Details ─────────────────────────────────────────────────────────────────────────────────
approved_eic <- all_approved_eic()
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling X_eicCodes.csv file from cache
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling Y_eicCodes.csv file from cache
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling Z_eicCodes.csv file from cache
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling T_eicCodes.csv file from cache
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling V_eicCodes.csv file from cache
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling W_eicCodes.csv file from cache
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling A_eicCodes.csv file from cache
glimpse(approved_eic)
#> Rows: 70,782
#> Columns: 11
#> $ eic_code                            <chr> "26X00000001515-Y", "26X00000105734-O", "26X00000105740-W", "10X1001A1001A…
#> $ eic_display_name                    <chr> "LA_220", "IT-GEO____SPA", "IT-BETA_ENERGYS", "ELIA", "ENERGINET-DK", "FIN…
#> $ eic_long_name                       <chr> "La 220 S.p.A.", "GEO", "BETA ENERGY", "Elia Transmission Belgium", "Energ…
#> $ eic_parent                          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ eic_responsible_party               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ eic_status                          <chr> "Active", "Active", "Active", "Active", "Active", "Active", "Active", "Act…
#> $ market_participant_postal_code      <chr> NA, NA, NA, "1000", "7000", "FI-00101", "1031", "00138", "53123", "NL-6812…
#> $ market_participant_iso_country_code <chr> NA, NA, NA, "BE", NA, "FI", "HU", "IT", NA, "NL", NA, "EE", "SE", "BE", NA…
#> $ market_participant_vat_code         <chr> "IT02633180985", "IT03961820960", "IT07672150963", "BE0731852231", "DK2898…
#> $ eic_type_function_list              <chr> "Trade Responsible Party", "Trade Responsible Party", "Trade Responsible P…
#> $ type                                <chr> "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",…
```

Use this function when you need:

- Quick lookups
- Basic EIC information
- Standard column structure

### all_allocated_eic()

Provides more detailed information but is slower:

``` r
cli_h1("all_allocated_eic() - Column Details")
#> 
#> ── all_allocated_eic() - Column Details ────────────────────────────────────────────────────────────────────────────────
cli_inform("Note: This function is slow (downloads ~70MB XML)")
#> Note: This function is slow (downloads ~70MB XML)

allocated_eic <- all_allocated_eic()
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ downloading all_allocated_eic table ...
#> <- HTTP/1.1 200 OK
#> <- Content-Length: 67295664
#> <- Content-Type: application/octet-stream
#> <- Content-MD5: JsBSS8Yq6SiYAT1SZT5MOA==
#> <- Last-Modified: Mon, 13 Apr 2026 01:15:12 GMT
#> <- Accept-Ranges: bytes
#> <- ETag: "0x8DE98FA1CEB6EF9"
#> <- Vary: Origin
#> <- Server: Windows-Azure-Blob/1.0 Microsoft-HTTPAPI/2.0
#> <- x-ms-request-id: a66c332d-301e-0092-341d-cbef3c000000
#> <- x-ms-version: 2014-02-14
#> <- x-ms-lease-status: unlocked
#> <- x-ms-lease-state: available
#> <- x-ms-blob-type: BlockBlob
#> <- Date: Mon, 13 Apr 2026 08:14:47 GMT
#> <-
#> ✔ response has arrived
#> converting ■■■■■■                            15% | ETA:  6s
#> converting ■■■■■■■■■■■■■■■                   46% | ETA:  4s
#> converting ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     93% | ETA:  0s
#> converting ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
glimpse(allocated_eic)
#> Rows: 74,187
#> Columns: 17
#> $ revision_number                              <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", …
#> $ created_date_time                            <chr> "2026-04-13T01:15:10Z", "2026-04-13T01:15:10Z", "2026-04-13T01:15…
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

Column comparison between
[`all_allocated_eic()`](https://krose.github.io/entsoeapi/reference/all_allocated_eic.md)
and `all_approced_eic`:

``` r
cli_h1("all_allocated_eic()")
#> 
#> ── all_allocated_eic() ─────────────────────────────────────────────────────────────────────────────────────────────────
allocated_eic |>
  filter(eic_code == "50WG00000001997X") |>
  t() |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> |                                             |                            |
#> |:--------------------------------------------|:---------------------------|
#> |revision_number                              |1                           |
#> |created_date_time                            |2026-04-13T01:15:10Z        |
#> |eic_code                                     |50WG00000001997X            |
#> |doc_status_value                             |A05                         |
#> |doc_status                                   |Control block area schedule |
#> |instance_component_attribute                 |International               |
#> |long_name                                    |USTA    G2      HYDRO       |
#> |display_name                                 |NO-USTAG2                   |
#> |last_request_date                            |2025-02-21                  |
#> |deactivation_requested_date_and_or_time_date |NA                          |
#> |eic_code_market_participant_street_address   |NA                          |
#> |market_participant_vat_code_name             |NA                          |
#> |market_participant_acer_code_name            |NA                          |
#> |description                                  |NA                          |
#> |responsible_market_participant_mrid          |10X1001A1001A38Y            |
#> |function_names                               |Generation Unit             |
#> |parent_market_document_mrid                  |50WP000000016883            |

cli_h1("all_approved_eic()")
#> 
#> ── all_approved_eic() ──────────────────────────────────────────────────────────────────────────────────────────────────

approved_eic |>
  filter(eic_code == "50WG00000001997X") |>
  t() |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> |                                    |                      |
#> |:-----------------------------------|:---------------------|
#> |eic_code                            |50WG00000001997X      |
#> |eic_display_name                    |NO-USTAG2             |
#> |eic_long_name                       |USTA    G2      HYDRO |
#> |eic_parent                          |50WP000000016883      |
#> |eic_responsible_party               |10X1001A1001A38Y      |
#> |eic_status                          |Active                |
#> |market_participant_postal_code      |NA                    |
#> |market_participant_iso_country_code |NA                    |
#> |market_participant_vat_code         |NA                    |
#> |eic_type_function_list              |Generation Unit       |
#> |type                                |W                     |
```

Use
[`all_allocated_eic()`](https://krose.github.io/entsoeapi/reference/all_allocated_eic.md)
function when you need:

- More detailed participant information
- Current allocation status
- Extended metadata

## Practical Examples

### Finding Germany’s EIC Codes

``` r
cli_h1("German bidding zones")
#> 
#> ── German bidding zones ────────────────────────────────────────────────────────────────────────────────────────────────
all_eic |>
  filter(
    grepl(
      pattern = "Germany.*bidding|area.*germany",
      x = eic_long_name,
      ignore.case = TRUE
    )
  ) |>
  select(eic_code, eic_long_name, type) |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> |eic_code         |eic_long_name                                    |type |
#> |:----------------|:------------------------------------------------|:----|
#> |10Y1001C--00029Y |Border Area Germany - Czech Republic             |Y    |
#> |10Y1001C--000360 |Border Area Germany - Norway                     |Y    |
#> |10YDOM-1001A092I |Border area Germany - Netherlands                |Y    |
#> |10YDOM-1010A0124 |Border area Germany-Switzerland                  |Y    |
#> |10YDOM-1010A016X |Border Area Austria–Germany                      |Y    |
#> |10YDOM-1010A018T |Border Area APG–TenneT (Germany)                 |Y    |
#> |10YDOM-CZ-DE-SKK |Border area Czech Repulbic Germany and Slovakia  |Y    |
#> |10YDOM--DE-FR--8 |Border area Germany France                       |Y    |
#> |10YDOM-EON-NL--2 |Border area Germany (TSO: TenneT DE) Netherlands |Y    |
#> |22Y201903141---O |Border Area Germany-Belgium                      |Y    |

cli_h1("German TSOs")
#> 
#> ── German TSOs ─────────────────────────────────────────────────────────────────────────────────────────────────────────
party_eic() |>
  filter(
    grepl(
      pattern = "50Hertz|Amprion|TenneT|TransnetBW",
      x = eic_long_name,
      ignore.case = TRUE
    )
  ) |>
  filter(
    grepl(
      pattern = "System Operator",
      x = eic_type_function_list,
      ignore.case = TRUE
    )
  ) |>
  filter(
    grepl(
      pattern = "GmbH",
      x = eic_long_name,
      ignore.case = TRUE
    )
  ) |>
  select(eic_code, eic_long_name, type) |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling X_eicCodes.csv file from cache
#> |eic_code         |eic_long_name             |type |
#> |:----------------|:-------------------------|:----|
#> |10XDE-ENBW--TNGX |TransnetBW GmbH           |X    |
#> |10XDE-EON-NETZ-C |TenneT TSO GmbH           |X    |
#> |10XDE-RWENET---W |Amprion GmbH              |X    |
#> |10XDE-VE-TRANSMK |50Hertz Transmission GmbH |X    |
```

### Finding Nordic Bidding Zones

``` r
cli_h1("Nordic Bidding Zones")
#> 
#> ── Nordic Bidding Zones ────────────────────────────────────────────────────────────────────────────────────────────────

nordic_countries <- c("Sweden", "Norway", "Denmark", "Finland")
zones |>
  filter(eic_long_name %in% nordic_countries) |>
  select(eic_code, eic_long_name, type) |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> |eic_code         |eic_long_name |type |
#> |:----------------|:-------------|:----|
#> |10Y1001A1001A65H |Denmark       |Y    |
#> |10YFI-1--------U |Finland       |Y    |
#> |10YNO-0--------C |Norway        |Y    |
#> |10YSE-1--------K |Sweden        |Y    |
```

### Finding Transmission Lines Between Countries

``` r
cli_h1("Cross-Border Transmission Lines")
#> 
#> ── Cross-Border Transmission Lines ─────────────────────────────────────────────────────────────────────────────────────

# Find Germany-France interconnector
tie_line_eic() |>
  filter(
    grepl(
      pattern = "DE.*FR|FR.*DE|Germany.*France",
      x = eic_long_name,
      ignore.case = TRUE
    )
  ) |>
  select(eic_code, eic_long_name, type) |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> 
#> ── public download ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> ℹ pulling T_eicCodes.csv file from cache
#> |eic_code         |eic_long_name                                       |type |
#> |:----------------|:---------------------------------------------------|:----|
#> |10T-DK-DK-00001Q |FRAUGDE-HERSLEV                                     |T    |
#> |10T-ES-PT-000120 |Rosal de la Frontera-Vilaverde                      |T    |
#> |11TD8L592------K |380-kV-line Dresden/Süd - Freiberg/Nord 592         |T    |
#> |16TLCDRA1FRD---D |Canicada - Riba De Ave 1 - Frades 150               |T    |
#> |45T000000000076E |400 Fraugde Kingstrup                               |T    |
#> |45T000000000077C |400 Fraugde Landerupgård                            |T    |
#> |45T000000000090K |400 kV Fraugde Busbar S1                            |T    |
#> |45T000000000136M |400 kV Fraugde Busbar S2                            |T    |
#> |49T000000000560N |Zwolle Frankhuis - Zwolle Weteringkade wit 110 kV   |T    |
#> |49T000000000561L |Zwolle Frankhuis - Zwolle Weteringkade zwart 110 kV |T    |
#> |50TL00000001149G |N 33 Frydenlund-Nygård                              |T    |
#> |50TL00000001155L |N 33Narvik-Frydenlund1                              |T    |
#> |50TL00000001156J |N 33Narvik-Frydenlund2                              |T    |
#> |50TL00000001157H |N 33Narvik-Frydenlund3                              |T    |
#> |50TL00000001216R |N 420 Viklandet-Fræna                               |T    |
#> |50TL000000017604 |N 66 Førde-Tefre                                    |T    |
#> |50TL00000003124M |N 33Narvik-Frydenlund4                              |T    |
#> |49T000000000730O |IJsselmuiden-Zwolle Frankhuis 110kV Zwart           |T    |
#> |45T000000000199Z |400kV Fraugde Vest Fynsværket                       |T    |
```

### Combining EIC Data with API Queries

Use EIC lookups to construct queries:

``` r
cli_h1("Using EICs in Queries")
#> 
#> ── Using EICs in Queries ───────────────────────────────────────────────────────────────────────────────────────────────
# Query intraday prices for Spain
from_ts <- ymd("2026-01-01", tz = "CET")
till_ts <- from_ts + days(3L)

es_prices <- energy_prices(
  eic = "10YES-REE------0",
  period_start = from_ts,
  period_end = till_ts,
  contract_type = "A07",
  tidy_output = TRUE
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A44&in_Domain=10YES-REE------0&out_Domain=10YES-REE------0&periodStart=202512312300&periodEnd=202601032300&contract_MarketAgreement.type=A07&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Mon, 13 Apr 2026 08:15:09 GMT
#> <- content-type: text/xml
#> <- content-disposition: inline; filename="Energy_Prices_202512312300-202601032300.xml"
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

cli_alert_success("Retrieved {nrow(es_prices)} intraday price points for Spain")
#> ✔ Retrieved 720 intraday price points for Spain

# Show sample data
es_prices |>
  filter(ts_classification_sequence_position == 1L) |>
  select(
    market_agreement_type_def,
    ts_point_dt_start,
    ts_point_price_amount,
    ts_currency_unit_name
  ) |>
  head(12) |>
  kbl(format = "pipe") |>
  cat(sep = "\n")
#> |market_agreement_type_def |ts_point_dt_start   | ts_point_price_amount|ts_currency_unit_name |
#> |:-------------------------|:-------------------|---------------------:|:---------------------|
#> |Intraday contract         |2025-12-31 23:00:00 |                 93.22|EUR                   |
#> |Intraday contract         |2025-12-31 23:15:00 |                 92.83|EUR                   |
#> |Intraday contract         |2025-12-31 23:30:00 |                 91.71|EUR                   |
#> |Intraday contract         |2025-12-31 23:45:00 |                 91.25|EUR                   |
#> |Intraday contract         |2026-01-01 00:00:00 |                 89.37|EUR                   |
#> |Intraday contract         |2026-01-01 00:15:00 |                 90.64|EUR                   |
#> |Intraday contract         |2026-01-01 00:30:00 |                 88.99|EUR                   |
#> |Intraday contract         |2026-01-01 00:45:00 |                 87.08|EUR                   |
#> |Intraday contract         |2026-01-01 01:00:00 |                 85.64|EUR                   |
#> |Intraday contract         |2026-01-01 01:15:00 |                 86.00|EUR                   |
#> |Intraday contract         |2026-01-01 01:30:00 |                 84.97|EUR                   |
#> |Intraday contract         |2026-01-01 01:45:00 |                 84.12|EUR                   |
```

## Summary

The entsoeapi package provides eight EIC lookup functions to help you
find the codes needed for your queries:

| Function                                                                                        | Returns             | EIC Type  |
|-------------------------------------------------------------------------------------------------|---------------------|-----------|
| [`area_eic()`](https://krose.github.io/entsoeapi/reference/area_eic.md)                         | Bidding zones       | Y codes   |
| [`party_eic()`](https://krose.github.io/entsoeapi/reference/party_eic.md)                       | Market participants | X codes   |
| [`accounting_point_eic()`](https://krose.github.io/entsoeapi/reference/accounting_point_eic.md) | Accounting points   | Y codes   |
| [`tie_line_eic()`](https://krose.github.io/entsoeapi/reference/tie_line_eic.md)                 | Interconnectors     | Z codes   |
| [`location_eic()`](https://krose.github.io/entsoeapi/reference/location_eic.md)                 | Locations           | V codes   |
| [`resource_object_eic()`](https://krose.github.io/entsoeapi/reference/resource_object_eic.md)   | Power resources     | W codes   |
| [`substation_eic()`](https://krose.github.io/entsoeapi/reference/substation_eic.md)             | Substations         | A codes   |
| [`all_approved_eic()`](https://krose.github.io/entsoeapi/reference/all_approved_eic.md)         | All approved EICs   | All types |

For comprehensive lookups, use
[`all_approved_eic()`](https://krose.github.io/entsoeapi/reference/all_approved_eic.md)
for speed or
[`all_allocated_eic()`](https://krose.github.io/entsoeapi/reference/all_allocated_eic.md)
for extended details.
