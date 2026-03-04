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
#> ── API call ────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A79&biddingZone_Domain=10Y1001A1001A82H&periodStart=202503032300&periodEnd=202603032300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Wed, 04 Mar 2026 22:14:02 GMT
#> <- content-type: application/zip
#> <- content-length: 161729
#> <- content-disposition: attachment; filename="Unavailability_of_offshore_grid_202503261549-202603072100.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpSo3AKG/001-UNAVAILABILITY_OF_OFFSHORE_GRID_202503261549-202503270600.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/002-UNAVAILABILITY_OF_OFFSHORE_GRID_202503280609-202503292259.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/003-UNAVAILABILITY_OF_OFFSHORE_GRID_202503280609-202503292259.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/004-UNAVAILABILITY_OF_OFFSHORE_GRID_202503280609-202503310640.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/005-UNAVAILABILITY_OF_OFFSHORE_GRID_202503280609-202503310640.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/006-UNAVAILABILITY_OF_OFFSHORE_GRID_202503280609-202503310640.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/007-UNAVAILABILITY_OF_OFFSHORE_GRID_202503280609-202503310640.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/008-UNAVAILABILITY_OF_OFFSHORE_GRID_202504111123-202504111755.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/009-UNAVAILABILITY_OF_OFFSHORE_GRID_202504151800-202504152015.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/010-UNAVAILABILITY_OF_OFFSHORE_GRID_202504200200-202504200446.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/011-UNAVAILABILITY_OF_OFFSHORE_GRID_202504200200-202504200446.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/012-UNAVAILABILITY_OF_OFFSHORE_GRID_202504200200-202504211518.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/013-UNAVAILABILITY_OF_OFFSHORE_GRID_202504211425-202504211508.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/014-UNAVAILABILITY_OF_OFFSHORE_GRID_202504211427-202504211508.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/015-UNAVAILABILITY_OF_OFFSHORE_GRID_202505161521-202505171256.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/016-UNAVAILABILITY_OF_OFFSHORE_GRID_202506211346-202506221045.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/017-UNAVAILABILITY_OF_OFFSHORE_GRID_202507100554-202507100611.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/018-UNAVAILABILITY_OF_OFFSHORE_GRID_202507100554-202507100611.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/019-UNAVAILABILITY_OF_OFFSHORE_GRID_202507100554-202507100611.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/020-UNAVAILABILITY_OF_OFFSHORE_GRID_202507100554-202507100611.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/021-UNAVAILABILITY_OF_OFFSHORE_GRID_202507100554-202507100611.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/022-UNAVAILABILITY_OF_OFFSHORE_GRID_202508080647-202508081412.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/023-UNAVAILABILITY_OF_OFFSHORE_GRID_202508080647-202508081412.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/024-UNAVAILABILITY_OF_OFFSHORE_GRID_202508080647-202508081412.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/025-UNAVAILABILITY_OF_OFFSHORE_GRID_202508080647-202508081429.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/026-UNAVAILABILITY_OF_OFFSHORE_GRID_202508080647-202508081429.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/027-UNAVAILABILITY_OF_OFFSHORE_GRID_202508080647-202508081431.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/028-UNAVAILABILITY_OF_OFFSHORE_GRID_202508080647-202508081431.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/029-UNAVAILABILITY_OF_OFFSHORE_GRID_202508101438-202508101609.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/030-UNAVAILABILITY_OF_OFFSHORE_GRID_202508101438-202508101609.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/031-UNAVAILABILITY_OF_OFFSHORE_GRID_202508101438-202508101618.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/032-UNAVAILABILITY_OF_OFFSHORE_GRID_202508101438-202508101618.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/033-UNAVAILABILITY_OF_OFFSHORE_GRID_202508101438-202508101620.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/034-UNAVAILABILITY_OF_OFFSHORE_GRID_202508101438-202508101620.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/035-UNAVAILABILITY_OF_OFFSHORE_GRID_202508101438-202508101701.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/036-UNAVAILABILITY_OF_OFFSHORE_GRID_202508111150-202508111726.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/037-UNAVAILABILITY_OF_OFFSHORE_GRID_202508111150-202508111821.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/038-UNAVAILABILITY_OF_OFFSHORE_GRID_202508111150-202508111821.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/039-UNAVAILABILITY_OF_OFFSHORE_GRID_202508111150-202508111846.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/040-UNAVAILABILITY_OF_OFFSHORE_GRID_202508111150-202508111846.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/041-UNAVAILABILITY_OF_OFFSHORE_GRID_202508310804-202509021031.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/042-UNAVAILABILITY_OF_OFFSHORE_GRID_202509020628-202509020937.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/043-UNAVAILABILITY_OF_OFFSHORE_GRID_202509020628-202509020937.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/044-UNAVAILABILITY_OF_OFFSHORE_GRID_202509181617-202509191407.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/045-UNAVAILABILITY_OF_OFFSHORE_GRID_202509181617-202509191407.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/046-UNAVAILABILITY_OF_OFFSHORE_GRID_202509181617-202509191407.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/047-UNAVAILABILITY_OF_OFFSHORE_GRID_202509181617-202509191413.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/048-UNAVAILABILITY_OF_OFFSHORE_GRID_202509181617-202509191413.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/049-UNAVAILABILITY_OF_OFFSHORE_GRID_202509181617-202509191413.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/050-UNAVAILABILITY_OF_OFFSHORE_GRID_202509181617-202509191413.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/051-UNAVAILABILITY_OF_OFFSHORE_GRID_202510012254-202510071943.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/052-UNAVAILABILITY_OF_OFFSHORE_GRID_202510012254-202510071943.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/053-UNAVAILABILITY_OF_OFFSHORE_GRID_202510012254-202510071943.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/054-UNAVAILABILITY_OF_OFFSHORE_GRID_202510012254-202510071943.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/055-UNAVAILABILITY_OF_OFFSHORE_GRID_202510012254-202510071950.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/056-UNAVAILABILITY_OF_OFFSHORE_GRID_202510012254-202510071950.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/057-UNAVAILABILITY_OF_OFFSHORE_GRID_202510301432-202510301751.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/058-UNAVAILABILITY_OF_OFFSHORE_GRID_202510301432-202510301751.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/059-UNAVAILABILITY_OF_OFFSHORE_GRID_202510301432-202510301758.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/060-UNAVAILABILITY_OF_OFFSHORE_GRID_202510301432-202510301758.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/061-UNAVAILABILITY_OF_OFFSHORE_GRID_202510301432-202510301823.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/062-UNAVAILABILITY_OF_OFFSHORE_GRID_202510301432-202510301826.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/063-UNAVAILABILITY_OF_OFFSHORE_GRID_202511041034-202511041223.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/064-UNAVAILABILITY_OF_OFFSHORE_GRID_202511041034-202511041223.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/065-UNAVAILABILITY_OF_OFFSHORE_GRID_202511041034-202511062114.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/066-UNAVAILABILITY_OF_OFFSHORE_GRID_202511041034-202511062114.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/067-UNAVAILABILITY_OF_OFFSHORE_GRID_202511051433-202511051733.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/068-UNAVAILABILITY_OF_OFFSHORE_GRID_202511051433-202511051735.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/069-UNAVAILABILITY_OF_OFFSHORE_GRID_202511051433-202511051738.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/070-UNAVAILABILITY_OF_OFFSHORE_GRID_202511051433-202511051738.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/071-UNAVAILABILITY_OF_OFFSHORE_GRID_202511051433-202511061250.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/072-UNAVAILABILITY_OF_OFFSHORE_GRID_202511180945-202511181350.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/073-UNAVAILABILITY_OF_OFFSHORE_GRID_202511180945-202511181410.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/074-UNAVAILABILITY_OF_OFFSHORE_GRID_202511180945-202511181410.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/075-UNAVAILABILITY_OF_OFFSHORE_GRID_202511180945-202511181416.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/076-UNAVAILABILITY_OF_OFFSHORE_GRID_202511180945-202511191100.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/077-UNAVAILABILITY_OF_OFFSHORE_GRID_202511211342-202511221712.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/078-UNAVAILABILITY_OF_OFFSHORE_GRID_202511221600-202511221843.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/079-UNAVAILABILITY_OF_OFFSHORE_GRID_202511221712-202511221843.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/080-UNAVAILABILITY_OF_OFFSHORE_GRID_202511280802-202511281942.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/081-UNAVAILABILITY_OF_OFFSHORE_GRID_202511280830-202511281726.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/082-UNAVAILABILITY_OF_OFFSHORE_GRID_202511280830-202511281726.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/083-UNAVAILABILITY_OF_OFFSHORE_GRID_202511281419-202511281638.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/084-UNAVAILABILITY_OF_OFFSHORE_GRID_202511281419-202511281645.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/085-UNAVAILABILITY_OF_OFFSHORE_GRID_202511281419-202511281645.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/086-UNAVAILABILITY_OF_OFFSHORE_GRID_202511281419-202512011215.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/087-UNAVAILABILITY_OF_OFFSHORE_GRID_202511290920-202511291030.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/088-UNAVAILABILITY_OF_OFFSHORE_GRID_202511290920-202511291115.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/089-UNAVAILABILITY_OF_OFFSHORE_GRID_202511290920-202512051425.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/090-UNAVAILABILITY_OF_OFFSHORE_GRID_202511290920-202512051425.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/091-UNAVAILABILITY_OF_OFFSHORE_GRID_202511301021-202511301807.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/092-UNAVAILABILITY_OF_OFFSHORE_GRID_202512010600-202512011844.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/093-UNAVAILABILITY_OF_OFFSHORE_GRID_202512011201-202512011303.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/094-UNAVAILABILITY_OF_OFFSHORE_GRID_202512011202-202512011303.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/095-UNAVAILABILITY_OF_OFFSHORE_GRID_202512011203-202512011310.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/096-UNAVAILABILITY_OF_OFFSHORE_GRID_202512021500-202512021947.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/097-UNAVAILABILITY_OF_OFFSHORE_GRID_202512031300-202512031935.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/098-UNAVAILABILITY_OF_OFFSHORE_GRID_202512041000-202512041953.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/099-UNAVAILABILITY_OF_OFFSHORE_GRID_202512051600-202512051612.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/100-UNAVAILABILITY_OF_OFFSHORE_GRID_202512051600-202512051612.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/101-UNAVAILABILITY_OF_OFFSHORE_GRID_202512160711-202512160911.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/102-UNAVAILABILITY_OF_OFFSHORE_GRID_202512160711-202512160911.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/103-UNAVAILABILITY_OF_OFFSHORE_GRID_202512160711-202512160912.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/104-UNAVAILABILITY_OF_OFFSHORE_GRID_202512160711-202512160912.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/105-UNAVAILABILITY_OF_OFFSHORE_GRID_202512160711-202512161518.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/106-UNAVAILABILITY_OF_OFFSHORE_GRID_202512211618-202512211807.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/107-UNAVAILABILITY_OF_OFFSHORE_GRID_202512211618-202512211807.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/108-UNAVAILABILITY_OF_OFFSHORE_GRID_202512211618-202512241825.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/109-UNAVAILABILITY_OF_OFFSHORE_GRID_202512211618-202512241825.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/110-UNAVAILABILITY_OF_OFFSHORE_GRID_202601101024-202601101621.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/111-UNAVAILABILITY_OF_OFFSHORE_GRID_202601101024-202601101621.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/112-UNAVAILABILITY_OF_OFFSHORE_GRID_202601101024-202601101621.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/113-UNAVAILABILITY_OF_OFFSHORE_GRID_202601101024-202601101625.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/114-UNAVAILABILITY_OF_OFFSHORE_GRID_202601101024-202601101625.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/115-UNAVAILABILITY_OF_OFFSHORE_GRID_202601141254-202601141548.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/116-UNAVAILABILITY_OF_OFFSHORE_GRID_202601141254-202601141548.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/117-UNAVAILABILITY_OF_OFFSHORE_GRID_202601141254-202601141548.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/118-UNAVAILABILITY_OF_OFFSHORE_GRID_202601141254-202601141548.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/119-UNAVAILABILITY_OF_OFFSHORE_GRID_202601141254-202601141548.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/120-UNAVAILABILITY_OF_OFFSHORE_GRID_202601141254-202601141552.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/121-UNAVAILABILITY_OF_OFFSHORE_GRID_202601141254-202601141552.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/122-UNAVAILABILITY_OF_OFFSHORE_GRID_202601161828-202601181842.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/123-UNAVAILABILITY_OF_OFFSHORE_GRID_202601161828-202601181842.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/124-UNAVAILABILITY_OF_OFFSHORE_GRID_202601161828-202601181845.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/125-UNAVAILABILITY_OF_OFFSHORE_GRID_202601161828-202601181847.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/126-UNAVAILABILITY_OF_OFFSHORE_GRID_202601161828-202601181900.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/127-UNAVAILABILITY_OF_OFFSHORE_GRID_202601161828-202601181902.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/128-UNAVAILABILITY_OF_OFFSHORE_GRID_202601231412-202601231604.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/129-UNAVAILABILITY_OF_OFFSHORE_GRID_202601231412-202601231604.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/130-UNAVAILABILITY_OF_OFFSHORE_GRID_202601251259-202601251300.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/131-UNAVAILABILITY_OF_OFFSHORE_GRID_202601271143-202601271446.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/132-UNAVAILABILITY_OF_OFFSHORE_GRID_202601290809-202601291115.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/133-UNAVAILABILITY_OF_OFFSHORE_GRID_202602151520-202602151838.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/134-UNAVAILABILITY_OF_OFFSHORE_GRID_202602151520-202602151838.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/135-UNAVAILABILITY_OF_OFFSHORE_GRID_202602151520-202602151838.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/136-UNAVAILABILITY_OF_OFFSHORE_GRID_202602151520-202602151906.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/137-UNAVAILABILITY_OF_OFFSHORE_GRID_202602151520-202602151906.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/138-UNAVAILABILITY_OF_OFFSHORE_GRID_202602151520-202602151908.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/139-UNAVAILABILITY_OF_OFFSHORE_GRID_202602151520-202602151908.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/140-UNAVAILABILITY_OF_OFFSHORE_GRID_202602170855-202602171138.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/141-UNAVAILABILITY_OF_OFFSHORE_GRID_202602170855-202602171138.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/142-UNAVAILABILITY_OF_OFFSHORE_GRID_202602170855-202602171138.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/143-UNAVAILABILITY_OF_OFFSHORE_GRID_202602170855-202602171144.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/144-UNAVAILABILITY_OF_OFFSHORE_GRID_202602170855-202602171144.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/145-UNAVAILABILITY_OF_OFFSHORE_GRID_202602170855-202602171145.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/146-UNAVAILABILITY_OF_OFFSHORE_GRID_202602170855-202602171145.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/147-UNAVAILABILITY_OF_OFFSHORE_GRID_202602220913-202602221116.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/148-UNAVAILABILITY_OF_OFFSHORE_GRID_202602220913-202602221116.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/149-UNAVAILABILITY_OF_OFFSHORE_GRID_202602220913-202602221116.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/150-UNAVAILABILITY_OF_OFFSHORE_GRID_202602220913-202602221122.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/151-UNAVAILABILITY_OF_OFFSHORE_GRID_202602220913-202602221122.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/152-UNAVAILABILITY_OF_OFFSHORE_GRID_202602220913-202602221126.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/153-UNAVAILABILITY_OF_OFFSHORE_GRID_202602220913-202602221126.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/154-UNAVAILABILITY_OF_OFFSHORE_GRID_202602221637-202602221855.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/155-UNAVAILABILITY_OF_OFFSHORE_GRID_202602221637-202602221856.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/156-UNAVAILABILITY_OF_OFFSHORE_GRID_202602221637-202603072100.xml has been read in
#> ✔ /tmp/RtmpSo3AKG/157-UNAVAILABILITY_OF_OFFSHORE_GRID_202603031122-202603052259.xml has been read in
#> processing xml list ■■■                                8% | ETA: 12s
#> processing xml list ■■■■■■                            18% | ETA: 11s
#> processing xml list ■■■■■■■■■■■■                      38% | ETA:  9s
#> processing xml list ■■■■■■■■■■■■■■■■■■■               59% | ETA:  6s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■         80% | ETA:  3s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s

str(df)
#> tibble [197,911 × 27] (S3: tbl_df/tbl/data.frame)
#>  $ ts_bidding_zone_domain_mrid       : chr [1:197911] "10Y1001A1001A82H" "10Y1001A1001A82H" "10Y1001A1001A82H" "10Y1001A1001A82H" ...
#>  $ ts_bidding_zone_domain_name       : chr [1:197911] "Germany_Luxemburg" "Germany_Luxemburg" "Germany_Luxemburg" "Germany_Luxemburg" ...
#>  $ ts_asset_location_name            : chr [1:197911] "BORKUM RIFFGRUND 1-DOLWIN ALPHA" "BORKUM RIFFGRUND 1-DOLWIN ALPHA" "BORKUM RIFFGRUND 1-DOLWIN ALPHA" "BORKUM RIFFGRUND 1-DOLWIN ALPHA" ...
#>  $ ts_asset_mrid                     : chr [1:197911] "11TD2L000000281U" "11TD2L000000281U" "11TD2L000000281U" "11TD2L000000281U" ...
#>  $ ts_asset_name                     : chr [1:197911] "TTG/155/BORKUM RIFFGRUND 1-DOLWIN ALPHA/AC130" "TTG/155/BORKUM RIFFGRUND 1-DOLWIN ALPHA/AC130" "TTG/155/BORKUM RIFFGRUND 1-DOLWIN ALPHA/AC130" "TTG/155/BORKUM RIFFGRUND 1-DOLWIN ALPHA/AC130" ...
#>  $ type                              : chr [1:197911] "A79" "A79" "A79" "A79" ...
#>  $ type_def                          : chr [1:197911] "Offshore grid infrastructure unavailability" "Offshore grid infrastructure unavailability" "Offshore grid infrastructure unavailability" "Offshore grid infrastructure unavailability" ...
#>  $ process_type                      : chr [1:197911] "A26" "A26" "A26" "A26" ...
#>  $ process_type_def                  : chr [1:197911] "Outage information" "Outage information" "Outage information" "Outage information" ...
#>  $ ts_business_type                  : chr [1:197911] "A54" "A54" "A54" "A54" ...
#>  $ ts_business_type_def              : chr [1:197911] "Unplanned outage" "Unplanned outage" "Unplanned outage" "Unplanned outage" ...
#>  $ ts_asset_psr_type                 : chr [1:197911] "B21" "B21" "B21" "B21" ...
#>  $ ts_asset_psr_type_def             : chr [1:197911] "AC Link" "AC Link" "AC Link" "AC Link" ...
#>  $ created_date_time                 : POSIXct[1:197911], format: "2025-09-16 10:15:05" "2025-09-16 10:15:05" ...
#>  $ reason_code                       : chr [1:197911] "B18" "B18" "B18" "B18" ...
#>  $ reason_text                       : chr [1:197911] "Failure" "Failure" "Failure" "Failure" ...
#>  $ revision_number                   : num [1:197911] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ unavailability_time_interval_start: POSIXct[1:197911], format: "2025-03-26 15:49:00" "2025-03-26 15:49:00" ...
#>  $ unavailability_time_interval_end  : POSIXct[1:197911], format: "2025-03-27 06:00:00" "2025-03-27 06:00:00" ...
#>  $ ts_resolution                     : chr [1:197911] "PT1M" "PT1M" "PT1M" "PT1M" ...
#>  $ ts_time_interval_start            : POSIXct[1:197911], format: "2025-03-26 15:49:00" "2025-03-26 15:49:00" ...
#>  $ ts_time_interval_end              : POSIXct[1:197911], format: "2025-03-27 06:00:00" "2025-03-27 06:00:00" ...
#>  $ ts_mrid                           : num [1:197911] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start                 : POSIXct[1:197911], format: "2025-03-26 15:49:00" "2025-03-26 15:50:00" ...
#>  $ ts_production_psr_nominal_p       : num [1:197911] 156 156 156 156 156 156 156 156 156 156 ...
#>  $ ts_point_quantity                 : num [1:197911] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ ts_quantity_measure_unit_name     : chr [1:197911] "MAW" "MAW" "MAW" "MAW" ...
```
