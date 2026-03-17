# Get Unavailability of Offshore Grid Infrastructure. (10.1.C)

Unavailability of the off-shore grid that reduce wind power feed-in
during at least one market time unit. Wind power fed in at the time of
the change in the availability is provided.

## Usage

``` r
outages_offshore_grid(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() + lubridate::days(x = 1L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date() + lubridate::days(x = 2L), tz = "CET"),
  doc_status = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
```

## Arguments

- eic:

  Energy Identification Code of the bidding zone/ control area (To
  extract outages of bidding zone DE-AT-LU area, it is recommended to
  send queries per control area i.e. CTA\|DE(50Hertz), CTA\|DE(Amprion),
  CTA\|DE(TeneTGer), CTA\|DE(TransnetBW),CTA\|AT,CTA\|LU but not per
  bidding zone.)

- period_start:

  the starting date of the in-scope period in POSIXct or YYYY-MM-DD
  HH:MM:SS format One year range limit applies

- period_end:

  the ending date of the outage in-scope period in POSIXct or YYYY-MM-DD
  HH:MM:SS format One year range limit applies

- doc_status:

  Notification document status. NULL or "A05" for active and "A13" for
  withdrawn. Defaults to NULL.

- tidy_output:

  Defaults to TRUE. flatten nested tables

- security_token:

  Security token for ENTSO-E transparency platform

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with the queried data, or `NULL` if no data is available for the given
parameters.

## Examples

``` r
df <- entsoeapi::outages_offshore_grid(
  eic = "10Y1001A1001A82H",
  period_start = lubridate::ymd(
    x = Sys.Date() - lubridate::days(x = 365L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(x = Sys.Date(), tz = "CET")
)
#> 
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A79&biddingZone_Domain=10Y1001A1001A82H&periodStart=202503162300&periodEnd=202603162300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Tue, 17 Mar 2026 22:11:42 GMT
#> <- content-type: application/zip
#> <- content-length: 174069
#> <- content-disposition: attachment; filename="Unavailability_of_offshore_grid_202503261549-202606012159.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpOgumiW/001-UNAVAILABILITY_OF_OFFSHORE_GRID_202503261549-202503270600.xml has been read in
#> ✔ /tmp/RtmpOgumiW/002-UNAVAILABILITY_OF_OFFSHORE_GRID_202503280609-202503292259.xml has been read in
#> ✔ /tmp/RtmpOgumiW/003-UNAVAILABILITY_OF_OFFSHORE_GRID_202503280609-202503292259.xml has been read in
#> ✔ /tmp/RtmpOgumiW/004-UNAVAILABILITY_OF_OFFSHORE_GRID_202503280609-202503310640.xml has been read in
#> ✔ /tmp/RtmpOgumiW/005-UNAVAILABILITY_OF_OFFSHORE_GRID_202503280609-202503310640.xml has been read in
#> ✔ /tmp/RtmpOgumiW/006-UNAVAILABILITY_OF_OFFSHORE_GRID_202503280609-202503310640.xml has been read in
#> ✔ /tmp/RtmpOgumiW/007-UNAVAILABILITY_OF_OFFSHORE_GRID_202503280609-202503310640.xml has been read in
#> ✔ /tmp/RtmpOgumiW/008-UNAVAILABILITY_OF_OFFSHORE_GRID_202504111123-202504111755.xml has been read in
#> ✔ /tmp/RtmpOgumiW/009-UNAVAILABILITY_OF_OFFSHORE_GRID_202504151800-202504152015.xml has been read in
#> ✔ /tmp/RtmpOgumiW/010-UNAVAILABILITY_OF_OFFSHORE_GRID_202504200200-202504200446.xml has been read in
#> ✔ /tmp/RtmpOgumiW/011-UNAVAILABILITY_OF_OFFSHORE_GRID_202504200200-202504200446.xml has been read in
#> ✔ /tmp/RtmpOgumiW/012-UNAVAILABILITY_OF_OFFSHORE_GRID_202504200200-202504211518.xml has been read in
#> ✔ /tmp/RtmpOgumiW/013-UNAVAILABILITY_OF_OFFSHORE_GRID_202504211425-202504211508.xml has been read in
#> ✔ /tmp/RtmpOgumiW/014-UNAVAILABILITY_OF_OFFSHORE_GRID_202504211427-202504211508.xml has been read in
#> ✔ /tmp/RtmpOgumiW/015-UNAVAILABILITY_OF_OFFSHORE_GRID_202505161521-202505171256.xml has been read in
#> ✔ /tmp/RtmpOgumiW/016-UNAVAILABILITY_OF_OFFSHORE_GRID_202506211346-202506221045.xml has been read in
#> ✔ /tmp/RtmpOgumiW/017-UNAVAILABILITY_OF_OFFSHORE_GRID_202507100554-202507100611.xml has been read in
#> ✔ /tmp/RtmpOgumiW/018-UNAVAILABILITY_OF_OFFSHORE_GRID_202507100554-202507100611.xml has been read in
#> ✔ /tmp/RtmpOgumiW/019-UNAVAILABILITY_OF_OFFSHORE_GRID_202507100554-202507100611.xml has been read in
#> ✔ /tmp/RtmpOgumiW/020-UNAVAILABILITY_OF_OFFSHORE_GRID_202507100554-202507100611.xml has been read in
#> ✔ /tmp/RtmpOgumiW/021-UNAVAILABILITY_OF_OFFSHORE_GRID_202507100554-202507100611.xml has been read in
#> ✔ /tmp/RtmpOgumiW/022-UNAVAILABILITY_OF_OFFSHORE_GRID_202508080647-202508081412.xml has been read in
#> ✔ /tmp/RtmpOgumiW/023-UNAVAILABILITY_OF_OFFSHORE_GRID_202508080647-202508081412.xml has been read in
#> ✔ /tmp/RtmpOgumiW/024-UNAVAILABILITY_OF_OFFSHORE_GRID_202508080647-202508081412.xml has been read in
#> ✔ /tmp/RtmpOgumiW/025-UNAVAILABILITY_OF_OFFSHORE_GRID_202508080647-202508081429.xml has been read in
#> ✔ /tmp/RtmpOgumiW/026-UNAVAILABILITY_OF_OFFSHORE_GRID_202508080647-202508081429.xml has been read in
#> ✔ /tmp/RtmpOgumiW/027-UNAVAILABILITY_OF_OFFSHORE_GRID_202508080647-202508081431.xml has been read in
#> ✔ /tmp/RtmpOgumiW/028-UNAVAILABILITY_OF_OFFSHORE_GRID_202508080647-202508081431.xml has been read in
#> ✔ /tmp/RtmpOgumiW/029-UNAVAILABILITY_OF_OFFSHORE_GRID_202508101438-202508101609.xml has been read in
#> ✔ /tmp/RtmpOgumiW/030-UNAVAILABILITY_OF_OFFSHORE_GRID_202508101438-202508101609.xml has been read in
#> ✔ /tmp/RtmpOgumiW/031-UNAVAILABILITY_OF_OFFSHORE_GRID_202508101438-202508101618.xml has been read in
#> ✔ /tmp/RtmpOgumiW/032-UNAVAILABILITY_OF_OFFSHORE_GRID_202508101438-202508101618.xml has been read in
#> ✔ /tmp/RtmpOgumiW/033-UNAVAILABILITY_OF_OFFSHORE_GRID_202508101438-202508101620.xml has been read in
#> ✔ /tmp/RtmpOgumiW/034-UNAVAILABILITY_OF_OFFSHORE_GRID_202508101438-202508101620.xml has been read in
#> ✔ /tmp/RtmpOgumiW/035-UNAVAILABILITY_OF_OFFSHORE_GRID_202508101438-202508101701.xml has been read in
#> ✔ /tmp/RtmpOgumiW/036-UNAVAILABILITY_OF_OFFSHORE_GRID_202508111150-202508111726.xml has been read in
#> ✔ /tmp/RtmpOgumiW/037-UNAVAILABILITY_OF_OFFSHORE_GRID_202508111150-202508111821.xml has been read in
#> ✔ /tmp/RtmpOgumiW/038-UNAVAILABILITY_OF_OFFSHORE_GRID_202508111150-202508111821.xml has been read in
#> ✔ /tmp/RtmpOgumiW/039-UNAVAILABILITY_OF_OFFSHORE_GRID_202508111150-202508111846.xml has been read in
#> ✔ /tmp/RtmpOgumiW/040-UNAVAILABILITY_OF_OFFSHORE_GRID_202508111150-202508111846.xml has been read in
#> ✔ /tmp/RtmpOgumiW/041-UNAVAILABILITY_OF_OFFSHORE_GRID_202508310804-202509021031.xml has been read in
#> ✔ /tmp/RtmpOgumiW/042-UNAVAILABILITY_OF_OFFSHORE_GRID_202509020628-202509020937.xml has been read in
#> ✔ /tmp/RtmpOgumiW/043-UNAVAILABILITY_OF_OFFSHORE_GRID_202509020628-202509020937.xml has been read in
#> ✔ /tmp/RtmpOgumiW/044-UNAVAILABILITY_OF_OFFSHORE_GRID_202509181617-202509191407.xml has been read in
#> ✔ /tmp/RtmpOgumiW/045-UNAVAILABILITY_OF_OFFSHORE_GRID_202509181617-202509191407.xml has been read in
#> ✔ /tmp/RtmpOgumiW/046-UNAVAILABILITY_OF_OFFSHORE_GRID_202509181617-202509191407.xml has been read in
#> ✔ /tmp/RtmpOgumiW/047-UNAVAILABILITY_OF_OFFSHORE_GRID_202509181617-202509191413.xml has been read in
#> ✔ /tmp/RtmpOgumiW/048-UNAVAILABILITY_OF_OFFSHORE_GRID_202509181617-202509191413.xml has been read in
#> ✔ /tmp/RtmpOgumiW/049-UNAVAILABILITY_OF_OFFSHORE_GRID_202509181617-202509191413.xml has been read in
#> ✔ /tmp/RtmpOgumiW/050-UNAVAILABILITY_OF_OFFSHORE_GRID_202509181617-202509191413.xml has been read in
#> ✔ /tmp/RtmpOgumiW/051-UNAVAILABILITY_OF_OFFSHORE_GRID_202510012254-202510071943.xml has been read in
#> ✔ /tmp/RtmpOgumiW/052-UNAVAILABILITY_OF_OFFSHORE_GRID_202510012254-202510071943.xml has been read in
#> ✔ /tmp/RtmpOgumiW/053-UNAVAILABILITY_OF_OFFSHORE_GRID_202510012254-202510071943.xml has been read in
#> ✔ /tmp/RtmpOgumiW/054-UNAVAILABILITY_OF_OFFSHORE_GRID_202510012254-202510071943.xml has been read in
#> ✔ /tmp/RtmpOgumiW/055-UNAVAILABILITY_OF_OFFSHORE_GRID_202510012254-202510071950.xml has been read in
#> ✔ /tmp/RtmpOgumiW/056-UNAVAILABILITY_OF_OFFSHORE_GRID_202510012254-202510071950.xml has been read in
#> ✔ /tmp/RtmpOgumiW/057-UNAVAILABILITY_OF_OFFSHORE_GRID_202510301432-202510301751.xml has been read in
#> ✔ /tmp/RtmpOgumiW/058-UNAVAILABILITY_OF_OFFSHORE_GRID_202510301432-202510301751.xml has been read in
#> ✔ /tmp/RtmpOgumiW/059-UNAVAILABILITY_OF_OFFSHORE_GRID_202510301432-202510301758.xml has been read in
#> ✔ /tmp/RtmpOgumiW/060-UNAVAILABILITY_OF_OFFSHORE_GRID_202510301432-202510301758.xml has been read in
#> ✔ /tmp/RtmpOgumiW/061-UNAVAILABILITY_OF_OFFSHORE_GRID_202510301432-202510301823.xml has been read in
#> ✔ /tmp/RtmpOgumiW/062-UNAVAILABILITY_OF_OFFSHORE_GRID_202510301432-202510301826.xml has been read in
#> ✔ /tmp/RtmpOgumiW/063-UNAVAILABILITY_OF_OFFSHORE_GRID_202511041034-202511041223.xml has been read in
#> ✔ /tmp/RtmpOgumiW/064-UNAVAILABILITY_OF_OFFSHORE_GRID_202511041034-202511041223.xml has been read in
#> ✔ /tmp/RtmpOgumiW/065-UNAVAILABILITY_OF_OFFSHORE_GRID_202511041034-202511062114.xml has been read in
#> ✔ /tmp/RtmpOgumiW/066-UNAVAILABILITY_OF_OFFSHORE_GRID_202511041034-202511062114.xml has been read in
#> ✔ /tmp/RtmpOgumiW/067-UNAVAILABILITY_OF_OFFSHORE_GRID_202511051433-202511051733.xml has been read in
#> ✔ /tmp/RtmpOgumiW/068-UNAVAILABILITY_OF_OFFSHORE_GRID_202511051433-202511051735.xml has been read in
#> ✔ /tmp/RtmpOgumiW/069-UNAVAILABILITY_OF_OFFSHORE_GRID_202511051433-202511051738.xml has been read in
#> ✔ /tmp/RtmpOgumiW/070-UNAVAILABILITY_OF_OFFSHORE_GRID_202511051433-202511051738.xml has been read in
#> ✔ /tmp/RtmpOgumiW/071-UNAVAILABILITY_OF_OFFSHORE_GRID_202511051433-202511061250.xml has been read in
#> ✔ /tmp/RtmpOgumiW/072-UNAVAILABILITY_OF_OFFSHORE_GRID_202511180945-202511181350.xml has been read in
#> ✔ /tmp/RtmpOgumiW/073-UNAVAILABILITY_OF_OFFSHORE_GRID_202511180945-202511181410.xml has been read in
#> ✔ /tmp/RtmpOgumiW/074-UNAVAILABILITY_OF_OFFSHORE_GRID_202511180945-202511181410.xml has been read in
#> ✔ /tmp/RtmpOgumiW/075-UNAVAILABILITY_OF_OFFSHORE_GRID_202511180945-202511181416.xml has been read in
#> ✔ /tmp/RtmpOgumiW/076-UNAVAILABILITY_OF_OFFSHORE_GRID_202511180945-202511191100.xml has been read in
#> ✔ /tmp/RtmpOgumiW/077-UNAVAILABILITY_OF_OFFSHORE_GRID_202511211342-202511221712.xml has been read in
#> ✔ /tmp/RtmpOgumiW/078-UNAVAILABILITY_OF_OFFSHORE_GRID_202511221600-202511221843.xml has been read in
#> ✔ /tmp/RtmpOgumiW/079-UNAVAILABILITY_OF_OFFSHORE_GRID_202511221712-202511221843.xml has been read in
#> ✔ /tmp/RtmpOgumiW/080-UNAVAILABILITY_OF_OFFSHORE_GRID_202511280802-202511281942.xml has been read in
#> ✔ /tmp/RtmpOgumiW/081-UNAVAILABILITY_OF_OFFSHORE_GRID_202511280830-202511281726.xml has been read in
#> ✔ /tmp/RtmpOgumiW/082-UNAVAILABILITY_OF_OFFSHORE_GRID_202511280830-202511281726.xml has been read in
#> ✔ /tmp/RtmpOgumiW/083-UNAVAILABILITY_OF_OFFSHORE_GRID_202511281419-202511281638.xml has been read in
#> ✔ /tmp/RtmpOgumiW/084-UNAVAILABILITY_OF_OFFSHORE_GRID_202511281419-202511281645.xml has been read in
#> ✔ /tmp/RtmpOgumiW/085-UNAVAILABILITY_OF_OFFSHORE_GRID_202511281419-202511281645.xml has been read in
#> ✔ /tmp/RtmpOgumiW/086-UNAVAILABILITY_OF_OFFSHORE_GRID_202511281419-202512011215.xml has been read in
#> ✔ /tmp/RtmpOgumiW/087-UNAVAILABILITY_OF_OFFSHORE_GRID_202511290920-202511291030.xml has been read in
#> ✔ /tmp/RtmpOgumiW/088-UNAVAILABILITY_OF_OFFSHORE_GRID_202511290920-202511291115.xml has been read in
#> ✔ /tmp/RtmpOgumiW/089-UNAVAILABILITY_OF_OFFSHORE_GRID_202511290920-202512051425.xml has been read in
#> ✔ /tmp/RtmpOgumiW/090-UNAVAILABILITY_OF_OFFSHORE_GRID_202511290920-202512051425.xml has been read in
#> ✔ /tmp/RtmpOgumiW/091-UNAVAILABILITY_OF_OFFSHORE_GRID_202511301021-202511301807.xml has been read in
#> ✔ /tmp/RtmpOgumiW/092-UNAVAILABILITY_OF_OFFSHORE_GRID_202512010600-202512011844.xml has been read in
#> ✔ /tmp/RtmpOgumiW/093-UNAVAILABILITY_OF_OFFSHORE_GRID_202512011201-202512011303.xml has been read in
#> ✔ /tmp/RtmpOgumiW/094-UNAVAILABILITY_OF_OFFSHORE_GRID_202512011202-202512011303.xml has been read in
#> ✔ /tmp/RtmpOgumiW/095-UNAVAILABILITY_OF_OFFSHORE_GRID_202512011203-202512011310.xml has been read in
#> ✔ /tmp/RtmpOgumiW/096-UNAVAILABILITY_OF_OFFSHORE_GRID_202512021500-202512021947.xml has been read in
#> ✔ /tmp/RtmpOgumiW/097-UNAVAILABILITY_OF_OFFSHORE_GRID_202512031300-202512031935.xml has been read in
#> ✔ /tmp/RtmpOgumiW/098-UNAVAILABILITY_OF_OFFSHORE_GRID_202512041000-202512041953.xml has been read in
#> ✔ /tmp/RtmpOgumiW/099-UNAVAILABILITY_OF_OFFSHORE_GRID_202512051600-202512051612.xml has been read in
#> ✔ /tmp/RtmpOgumiW/100-UNAVAILABILITY_OF_OFFSHORE_GRID_202512051600-202512051612.xml has been read in
#> ✔ /tmp/RtmpOgumiW/101-UNAVAILABILITY_OF_OFFSHORE_GRID_202512160711-202512160911.xml has been read in
#> ✔ /tmp/RtmpOgumiW/102-UNAVAILABILITY_OF_OFFSHORE_GRID_202512160711-202512160911.xml has been read in
#> ✔ /tmp/RtmpOgumiW/103-UNAVAILABILITY_OF_OFFSHORE_GRID_202512160711-202512160912.xml has been read in
#> ✔ /tmp/RtmpOgumiW/104-UNAVAILABILITY_OF_OFFSHORE_GRID_202512160711-202512160912.xml has been read in
#> ✔ /tmp/RtmpOgumiW/105-UNAVAILABILITY_OF_OFFSHORE_GRID_202512160711-202512161518.xml has been read in
#> ✔ /tmp/RtmpOgumiW/106-UNAVAILABILITY_OF_OFFSHORE_GRID_202512211618-202512211807.xml has been read in
#> ✔ /tmp/RtmpOgumiW/107-UNAVAILABILITY_OF_OFFSHORE_GRID_202512211618-202512211807.xml has been read in
#> ✔ /tmp/RtmpOgumiW/108-UNAVAILABILITY_OF_OFFSHORE_GRID_202512211618-202512241825.xml has been read in
#> ✔ /tmp/RtmpOgumiW/109-UNAVAILABILITY_OF_OFFSHORE_GRID_202512211618-202512241825.xml has been read in
#> ✔ /tmp/RtmpOgumiW/110-UNAVAILABILITY_OF_OFFSHORE_GRID_202601101024-202601101621.xml has been read in
#> ✔ /tmp/RtmpOgumiW/111-UNAVAILABILITY_OF_OFFSHORE_GRID_202601101024-202601101621.xml has been read in
#> ✔ /tmp/RtmpOgumiW/112-UNAVAILABILITY_OF_OFFSHORE_GRID_202601101024-202601101621.xml has been read in
#> ✔ /tmp/RtmpOgumiW/113-UNAVAILABILITY_OF_OFFSHORE_GRID_202601101024-202601101625.xml has been read in
#> ✔ /tmp/RtmpOgumiW/114-UNAVAILABILITY_OF_OFFSHORE_GRID_202601101024-202601101625.xml has been read in
#> ✔ /tmp/RtmpOgumiW/115-UNAVAILABILITY_OF_OFFSHORE_GRID_202601141254-202601141548.xml has been read in
#> ✔ /tmp/RtmpOgumiW/116-UNAVAILABILITY_OF_OFFSHORE_GRID_202601141254-202601141548.xml has been read in
#> ✔ /tmp/RtmpOgumiW/117-UNAVAILABILITY_OF_OFFSHORE_GRID_202601141254-202601141548.xml has been read in
#> ✔ /tmp/RtmpOgumiW/118-UNAVAILABILITY_OF_OFFSHORE_GRID_202601141254-202601141548.xml has been read in
#> ✔ /tmp/RtmpOgumiW/119-UNAVAILABILITY_OF_OFFSHORE_GRID_202601141254-202601141548.xml has been read in
#> ✔ /tmp/RtmpOgumiW/120-UNAVAILABILITY_OF_OFFSHORE_GRID_202601141254-202601141552.xml has been read in
#> ✔ /tmp/RtmpOgumiW/121-UNAVAILABILITY_OF_OFFSHORE_GRID_202601141254-202601141552.xml has been read in
#> ✔ /tmp/RtmpOgumiW/122-UNAVAILABILITY_OF_OFFSHORE_GRID_202601161828-202601181842.xml has been read in
#> ✔ /tmp/RtmpOgumiW/123-UNAVAILABILITY_OF_OFFSHORE_GRID_202601161828-202601181842.xml has been read in
#> ✔ /tmp/RtmpOgumiW/124-UNAVAILABILITY_OF_OFFSHORE_GRID_202601161828-202601181845.xml has been read in
#> ✔ /tmp/RtmpOgumiW/125-UNAVAILABILITY_OF_OFFSHORE_GRID_202601161828-202601181847.xml has been read in
#> ✔ /tmp/RtmpOgumiW/126-UNAVAILABILITY_OF_OFFSHORE_GRID_202601161828-202601181900.xml has been read in
#> ✔ /tmp/RtmpOgumiW/127-UNAVAILABILITY_OF_OFFSHORE_GRID_202601161828-202601181902.xml has been read in
#> ✔ /tmp/RtmpOgumiW/128-UNAVAILABILITY_OF_OFFSHORE_GRID_202601231412-202601231604.xml has been read in
#> ✔ /tmp/RtmpOgumiW/129-UNAVAILABILITY_OF_OFFSHORE_GRID_202601231412-202601231604.xml has been read in
#> ✔ /tmp/RtmpOgumiW/130-UNAVAILABILITY_OF_OFFSHORE_GRID_202601251259-202601251300.xml has been read in
#> ✔ /tmp/RtmpOgumiW/131-UNAVAILABILITY_OF_OFFSHORE_GRID_202601271143-202601271446.xml has been read in
#> ✔ /tmp/RtmpOgumiW/132-UNAVAILABILITY_OF_OFFSHORE_GRID_202601290809-202601291115.xml has been read in
#> ✔ /tmp/RtmpOgumiW/133-UNAVAILABILITY_OF_OFFSHORE_GRID_202602151520-202602151838.xml has been read in
#> ✔ /tmp/RtmpOgumiW/134-UNAVAILABILITY_OF_OFFSHORE_GRID_202602151520-202602151838.xml has been read in
#> ✔ /tmp/RtmpOgumiW/135-UNAVAILABILITY_OF_OFFSHORE_GRID_202602151520-202602151838.xml has been read in
#> ✔ /tmp/RtmpOgumiW/136-UNAVAILABILITY_OF_OFFSHORE_GRID_202602151520-202602151906.xml has been read in
#> ✔ /tmp/RtmpOgumiW/137-UNAVAILABILITY_OF_OFFSHORE_GRID_202602151520-202602151906.xml has been read in
#> ✔ /tmp/RtmpOgumiW/138-UNAVAILABILITY_OF_OFFSHORE_GRID_202602151520-202602151908.xml has been read in
#> ✔ /tmp/RtmpOgumiW/139-UNAVAILABILITY_OF_OFFSHORE_GRID_202602151520-202602151908.xml has been read in
#> ✔ /tmp/RtmpOgumiW/140-UNAVAILABILITY_OF_OFFSHORE_GRID_202602170855-202602171138.xml has been read in
#> ✔ /tmp/RtmpOgumiW/141-UNAVAILABILITY_OF_OFFSHORE_GRID_202602170855-202602171138.xml has been read in
#> ✔ /tmp/RtmpOgumiW/142-UNAVAILABILITY_OF_OFFSHORE_GRID_202602170855-202602171138.xml has been read in
#> ✔ /tmp/RtmpOgumiW/143-UNAVAILABILITY_OF_OFFSHORE_GRID_202602170855-202602171144.xml has been read in
#> ✔ /tmp/RtmpOgumiW/144-UNAVAILABILITY_OF_OFFSHORE_GRID_202602170855-202602171144.xml has been read in
#> ✔ /tmp/RtmpOgumiW/145-UNAVAILABILITY_OF_OFFSHORE_GRID_202602170855-202602171145.xml has been read in
#> ✔ /tmp/RtmpOgumiW/146-UNAVAILABILITY_OF_OFFSHORE_GRID_202602170855-202602171145.xml has been read in
#> ✔ /tmp/RtmpOgumiW/147-UNAVAILABILITY_OF_OFFSHORE_GRID_202602220913-202602221116.xml has been read in
#> ✔ /tmp/RtmpOgumiW/148-UNAVAILABILITY_OF_OFFSHORE_GRID_202602220913-202602221116.xml has been read in
#> ✔ /tmp/RtmpOgumiW/149-UNAVAILABILITY_OF_OFFSHORE_GRID_202602220913-202602221116.xml has been read in
#> ✔ /tmp/RtmpOgumiW/150-UNAVAILABILITY_OF_OFFSHORE_GRID_202602220913-202602221122.xml has been read in
#> ✔ /tmp/RtmpOgumiW/151-UNAVAILABILITY_OF_OFFSHORE_GRID_202602220913-202602221122.xml has been read in
#> ✔ /tmp/RtmpOgumiW/152-UNAVAILABILITY_OF_OFFSHORE_GRID_202602220913-202602221126.xml has been read in
#> ✔ /tmp/RtmpOgumiW/153-UNAVAILABILITY_OF_OFFSHORE_GRID_202602220913-202602221126.xml has been read in
#> ✔ /tmp/RtmpOgumiW/154-UNAVAILABILITY_OF_OFFSHORE_GRID_202602221637-202602221855.xml has been read in
#> ✔ /tmp/RtmpOgumiW/155-UNAVAILABILITY_OF_OFFSHORE_GRID_202602221637-202602221856.xml has been read in
#> ✔ /tmp/RtmpOgumiW/156-UNAVAILABILITY_OF_OFFSHORE_GRID_202602221637-202603101912.xml has been read in
#> ✔ /tmp/RtmpOgumiW/157-UNAVAILABILITY_OF_OFFSHORE_GRID_202602221637-202603101912.xml has been read in
#> ✔ /tmp/RtmpOgumiW/158-UNAVAILABILITY_OF_OFFSHORE_GRID_202603031122-202606012159.xml has been read in
#> ✔ /tmp/RtmpOgumiW/159-UNAVAILABILITY_OF_OFFSHORE_GRID_202603121559-202603121828.xml has been read in
#> ✔ /tmp/RtmpOgumiW/160-UNAVAILABILITY_OF_OFFSHORE_GRID_202603121559-202603121828.xml has been read in
#> ✔ /tmp/RtmpOgumiW/161-UNAVAILABILITY_OF_OFFSHORE_GRID_202603121559-202603121828.xml has been read in
#> ✔ /tmp/RtmpOgumiW/162-UNAVAILABILITY_OF_OFFSHORE_GRID_202603121559-202603121840.xml has been read in
#> ✔ /tmp/RtmpOgumiW/163-UNAVAILABILITY_OF_OFFSHORE_GRID_202603121559-202603121840.xml has been read in
#> ✔ /tmp/RtmpOgumiW/164-UNAVAILABILITY_OF_OFFSHORE_GRID_202603121559-202603121840.xml has been read in
#> ✔ /tmp/RtmpOgumiW/165-UNAVAILABILITY_OF_OFFSHORE_GRID_202603121559-202603121840.xml has been read in
#> ✔ /tmp/RtmpOgumiW/166-UNAVAILABILITY_OF_OFFSHORE_GRID_202603151834-202603152208.xml has been read in
#> ✔ /tmp/RtmpOgumiW/167-UNAVAILABILITY_OF_OFFSHORE_GRID_202603151834-202603152208.xml has been read in
#> ✔ /tmp/RtmpOgumiW/168-UNAVAILABILITY_OF_OFFSHORE_GRID_202603151834-202603202100.xml has been read in
#> ✔ /tmp/RtmpOgumiW/169-UNAVAILABILITY_OF_OFFSHORE_GRID_202603151834-202603202100.xml has been read in
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■                                7% | ETA: 14s
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■                           21% | ETA: 12s
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■                      38% | ETA: 10s
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■                56% | ETA:  7s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  4s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     94% | ETA:  1s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     94% | ETA:  1s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     94% | ETA:  1s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     94% | ETA:  1s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     94% | ETA:  1s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     94% | ETA:  1s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     94% | ETA:  1s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     94% | ETA:  1s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     94% | ETA:  1s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     94% | ETA:  1s
#> ℹ pulling area_eic_name table from cache
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     94% | ETA:  1s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ℹ pulling area_eic_name table from cache

dplyr::glimpse(df)
#> Rows: 368,189
#> Columns: 27
#> $ ts_bidding_zone_domain_mrid        <chr> "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A82H", "10Y1001A1001A8…
#> $ ts_bidding_zone_domain_name        <chr> "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Luxemburg", "Germany_Lux…
#> $ ts_asset_location_name             <chr> "BORKUM RIFFGRUND 1-DOLWIN ALPHA", "BORKUM RIFFGRUND 1-DOLWIN ALPHA", "BORK…
#> $ ts_asset_mrid                      <chr> "11TD2L000000281U", "11TD2L000000281U", "11TD2L000000281U", "11TD2L00000028…
#> $ ts_asset_name                      <chr> "TTG/155/BORKUM RIFFGRUND 1-DOLWIN ALPHA/AC130", "TTG/155/BORKUM RIFFGRUND …
#> $ type                               <chr> "A79", "A79", "A79", "A79", "A79", "A79", "A79", "A79", "A79", "A79", "A79"…
#> $ type_def                           <chr> "Offshore grid infrastructure unavailability", "Offshore grid infrastructur…
#> $ process_type                       <chr> "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26", "A26"…
#> $ process_type_def                   <chr> "Outage information", "Outage information", "Outage information", "Outage i…
#> $ ts_business_type                   <chr> "A54", "A54", "A54", "A54", "A54", "A54", "A54", "A54", "A54", "A54", "A54"…
#> $ ts_business_type_def               <chr> "Unplanned outage", "Unplanned outage", "Unplanned outage", "Unplanned outa…
#> $ ts_asset_psr_type                  <chr> "B21", "B21", "B21", "B21", "B21", "B21", "B21", "B21", "B21", "B21", "B21"…
#> $ ts_asset_psr_type_def              <chr> "AC Link", "AC Link", "AC Link", "AC Link", "AC Link", "AC Link", "AC Link"…
#> $ created_date_time                  <dttm> 2025-09-16 10:15:05, 2025-09-16 10:15:05, 2025-09-16 10:15:05, 2025-09-16 …
#> $ reason_code                        <chr> "B18", "B18", "B18", "B18", "B18", "B18", "B18", "B18", "B18", "B18", "B18"…
#> $ reason_text                        <chr> "Failure", "Failure", "Failure", "Failure", "Failure", "Failure", "Failure"…
#> $ revision_number                    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ unavailability_time_interval_start <dttm> 2025-03-26 15:49:00, 2025-03-26 15:49:00, 2025-03-26 15:49:00, 2025-03-26 …
#> $ unavailability_time_interval_end   <dttm> 2025-03-27 06:00:00, 2025-03-27 06:00:00, 2025-03-27 06:00:00, 2025-03-27 …
#> $ ts_resolution                      <chr> "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT1M", "PT…
#> $ ts_time_interval_start             <dttm> 2025-03-26 15:49:00, 2025-03-26 15:49:00, 2025-03-26 15:49:00, 2025-03-26 …
#> $ ts_time_interval_end               <dttm> 2025-03-27 06:00:00, 2025-03-27 06:00:00, 2025-03-27 06:00:00, 2025-03-27 …
#> $ ts_mrid                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ts_point_dt_start                  <dttm> 2025-03-26 15:49:00, 2025-03-26 15:50:00, 2025-03-26 15:51:00, 2025-03-26 …
#> $ ts_production_psr_nominal_p        <dbl> 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, …
#> $ ts_point_quantity                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ ts_quantity_measure_unit_name      <chr> "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW", "MAW"…
```
