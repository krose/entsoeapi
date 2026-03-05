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
#> ── API call ────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> → https://web-api.tp.entsoe.eu/api?documentType=A79&biddingZone_Domain=10Y1001A1001A82H&periodStart=202503042300&periodEnd=202603042300&securityToken=<...>
#> <- HTTP/2 200 
#> <- date: Thu, 05 Mar 2026 16:13:12 GMT
#> <- content-type: application/zip
#> <- content-length: 161728
#> <- content-disposition: attachment; filename="Unavailability_of_offshore_grid_202503261549-202603072100.zip"
#> <- x-content-type-options: nosniff
#> <- x-xss-protection: 0
#> <- strict-transport-security: max-age=15724800; includeSubDomains
#> <- 
#> ✔ response has arrived
#> ✔ /tmp/RtmpksiKAO/001-UNAVAILABILITY_OF_OFFSHORE_GRID_202503261549-202503270600.xml has been read in
#> ✔ /tmp/RtmpksiKAO/002-UNAVAILABILITY_OF_OFFSHORE_GRID_202503280609-202503292259.xml has been read in
#> ✔ /tmp/RtmpksiKAO/003-UNAVAILABILITY_OF_OFFSHORE_GRID_202503280609-202503292259.xml has been read in
#> ✔ /tmp/RtmpksiKAO/004-UNAVAILABILITY_OF_OFFSHORE_GRID_202503280609-202503310640.xml has been read in
#> ✔ /tmp/RtmpksiKAO/005-UNAVAILABILITY_OF_OFFSHORE_GRID_202503280609-202503310640.xml has been read in
#> ✔ /tmp/RtmpksiKAO/006-UNAVAILABILITY_OF_OFFSHORE_GRID_202503280609-202503310640.xml has been read in
#> ✔ /tmp/RtmpksiKAO/007-UNAVAILABILITY_OF_OFFSHORE_GRID_202503280609-202503310640.xml has been read in
#> ✔ /tmp/RtmpksiKAO/008-UNAVAILABILITY_OF_OFFSHORE_GRID_202504111123-202504111755.xml has been read in
#> ✔ /tmp/RtmpksiKAO/009-UNAVAILABILITY_OF_OFFSHORE_GRID_202504151800-202504152015.xml has been read in
#> ✔ /tmp/RtmpksiKAO/010-UNAVAILABILITY_OF_OFFSHORE_GRID_202504200200-202504200446.xml has been read in
#> ✔ /tmp/RtmpksiKAO/011-UNAVAILABILITY_OF_OFFSHORE_GRID_202504200200-202504200446.xml has been read in
#> ✔ /tmp/RtmpksiKAO/012-UNAVAILABILITY_OF_OFFSHORE_GRID_202504200200-202504211518.xml has been read in
#> ✔ /tmp/RtmpksiKAO/013-UNAVAILABILITY_OF_OFFSHORE_GRID_202504211425-202504211508.xml has been read in
#> ✔ /tmp/RtmpksiKAO/014-UNAVAILABILITY_OF_OFFSHORE_GRID_202504211427-202504211508.xml has been read in
#> ✔ /tmp/RtmpksiKAO/015-UNAVAILABILITY_OF_OFFSHORE_GRID_202505161521-202505171256.xml has been read in
#> ✔ /tmp/RtmpksiKAO/016-UNAVAILABILITY_OF_OFFSHORE_GRID_202506211346-202506221045.xml has been read in
#> ✔ /tmp/RtmpksiKAO/017-UNAVAILABILITY_OF_OFFSHORE_GRID_202507100554-202507100611.xml has been read in
#> ✔ /tmp/RtmpksiKAO/018-UNAVAILABILITY_OF_OFFSHORE_GRID_202507100554-202507100611.xml has been read in
#> ✔ /tmp/RtmpksiKAO/019-UNAVAILABILITY_OF_OFFSHORE_GRID_202507100554-202507100611.xml has been read in
#> ✔ /tmp/RtmpksiKAO/020-UNAVAILABILITY_OF_OFFSHORE_GRID_202507100554-202507100611.xml has been read in
#> ✔ /tmp/RtmpksiKAO/021-UNAVAILABILITY_OF_OFFSHORE_GRID_202507100554-202507100611.xml has been read in
#> ✔ /tmp/RtmpksiKAO/022-UNAVAILABILITY_OF_OFFSHORE_GRID_202508080647-202508081412.xml has been read in
#> ✔ /tmp/RtmpksiKAO/023-UNAVAILABILITY_OF_OFFSHORE_GRID_202508080647-202508081412.xml has been read in
#> ✔ /tmp/RtmpksiKAO/024-UNAVAILABILITY_OF_OFFSHORE_GRID_202508080647-202508081412.xml has been read in
#> ✔ /tmp/RtmpksiKAO/025-UNAVAILABILITY_OF_OFFSHORE_GRID_202508080647-202508081429.xml has been read in
#> ✔ /tmp/RtmpksiKAO/026-UNAVAILABILITY_OF_OFFSHORE_GRID_202508080647-202508081429.xml has been read in
#> ✔ /tmp/RtmpksiKAO/027-UNAVAILABILITY_OF_OFFSHORE_GRID_202508080647-202508081431.xml has been read in
#> ✔ /tmp/RtmpksiKAO/028-UNAVAILABILITY_OF_OFFSHORE_GRID_202508080647-202508081431.xml has been read in
#> ✔ /tmp/RtmpksiKAO/029-UNAVAILABILITY_OF_OFFSHORE_GRID_202508101438-202508101609.xml has been read in
#> ✔ /tmp/RtmpksiKAO/030-UNAVAILABILITY_OF_OFFSHORE_GRID_202508101438-202508101609.xml has been read in
#> ✔ /tmp/RtmpksiKAO/031-UNAVAILABILITY_OF_OFFSHORE_GRID_202508101438-202508101618.xml has been read in
#> ✔ /tmp/RtmpksiKAO/032-UNAVAILABILITY_OF_OFFSHORE_GRID_202508101438-202508101618.xml has been read in
#> ✔ /tmp/RtmpksiKAO/033-UNAVAILABILITY_OF_OFFSHORE_GRID_202508101438-202508101620.xml has been read in
#> ✔ /tmp/RtmpksiKAO/034-UNAVAILABILITY_OF_OFFSHORE_GRID_202508101438-202508101620.xml has been read in
#> ✔ /tmp/RtmpksiKAO/035-UNAVAILABILITY_OF_OFFSHORE_GRID_202508101438-202508101701.xml has been read in
#> ✔ /tmp/RtmpksiKAO/036-UNAVAILABILITY_OF_OFFSHORE_GRID_202508111150-202508111726.xml has been read in
#> ✔ /tmp/RtmpksiKAO/037-UNAVAILABILITY_OF_OFFSHORE_GRID_202508111150-202508111821.xml has been read in
#> ✔ /tmp/RtmpksiKAO/038-UNAVAILABILITY_OF_OFFSHORE_GRID_202508111150-202508111821.xml has been read in
#> ✔ /tmp/RtmpksiKAO/039-UNAVAILABILITY_OF_OFFSHORE_GRID_202508111150-202508111846.xml has been read in
#> ✔ /tmp/RtmpksiKAO/040-UNAVAILABILITY_OF_OFFSHORE_GRID_202508111150-202508111846.xml has been read in
#> ✔ /tmp/RtmpksiKAO/041-UNAVAILABILITY_OF_OFFSHORE_GRID_202508310804-202509021031.xml has been read in
#> ✔ /tmp/RtmpksiKAO/042-UNAVAILABILITY_OF_OFFSHORE_GRID_202509020628-202509020937.xml has been read in
#> ✔ /tmp/RtmpksiKAO/043-UNAVAILABILITY_OF_OFFSHORE_GRID_202509020628-202509020937.xml has been read in
#> ✔ /tmp/RtmpksiKAO/044-UNAVAILABILITY_OF_OFFSHORE_GRID_202509181617-202509191407.xml has been read in
#> ✔ /tmp/RtmpksiKAO/045-UNAVAILABILITY_OF_OFFSHORE_GRID_202509181617-202509191407.xml has been read in
#> ✔ /tmp/RtmpksiKAO/046-UNAVAILABILITY_OF_OFFSHORE_GRID_202509181617-202509191407.xml has been read in
#> ✔ /tmp/RtmpksiKAO/047-UNAVAILABILITY_OF_OFFSHORE_GRID_202509181617-202509191413.xml has been read in
#> ✔ /tmp/RtmpksiKAO/048-UNAVAILABILITY_OF_OFFSHORE_GRID_202509181617-202509191413.xml has been read in
#> ✔ /tmp/RtmpksiKAO/049-UNAVAILABILITY_OF_OFFSHORE_GRID_202509181617-202509191413.xml has been read in
#> ✔ /tmp/RtmpksiKAO/050-UNAVAILABILITY_OF_OFFSHORE_GRID_202509181617-202509191413.xml has been read in
#> ✔ /tmp/RtmpksiKAO/051-UNAVAILABILITY_OF_OFFSHORE_GRID_202510012254-202510071943.xml has been read in
#> ✔ /tmp/RtmpksiKAO/052-UNAVAILABILITY_OF_OFFSHORE_GRID_202510012254-202510071943.xml has been read in
#> ✔ /tmp/RtmpksiKAO/053-UNAVAILABILITY_OF_OFFSHORE_GRID_202510012254-202510071943.xml has been read in
#> ✔ /tmp/RtmpksiKAO/054-UNAVAILABILITY_OF_OFFSHORE_GRID_202510012254-202510071943.xml has been read in
#> ✔ /tmp/RtmpksiKAO/055-UNAVAILABILITY_OF_OFFSHORE_GRID_202510012254-202510071950.xml has been read in
#> ✔ /tmp/RtmpksiKAO/056-UNAVAILABILITY_OF_OFFSHORE_GRID_202510012254-202510071950.xml has been read in
#> ✔ /tmp/RtmpksiKAO/057-UNAVAILABILITY_OF_OFFSHORE_GRID_202510301432-202510301751.xml has been read in
#> ✔ /tmp/RtmpksiKAO/058-UNAVAILABILITY_OF_OFFSHORE_GRID_202510301432-202510301751.xml has been read in
#> ✔ /tmp/RtmpksiKAO/059-UNAVAILABILITY_OF_OFFSHORE_GRID_202510301432-202510301758.xml has been read in
#> ✔ /tmp/RtmpksiKAO/060-UNAVAILABILITY_OF_OFFSHORE_GRID_202510301432-202510301758.xml has been read in
#> ✔ /tmp/RtmpksiKAO/061-UNAVAILABILITY_OF_OFFSHORE_GRID_202510301432-202510301823.xml has been read in
#> ✔ /tmp/RtmpksiKAO/062-UNAVAILABILITY_OF_OFFSHORE_GRID_202510301432-202510301826.xml has been read in
#> ✔ /tmp/RtmpksiKAO/063-UNAVAILABILITY_OF_OFFSHORE_GRID_202511041034-202511041223.xml has been read in
#> ✔ /tmp/RtmpksiKAO/064-UNAVAILABILITY_OF_OFFSHORE_GRID_202511041034-202511041223.xml has been read in
#> ✔ /tmp/RtmpksiKAO/065-UNAVAILABILITY_OF_OFFSHORE_GRID_202511041034-202511062114.xml has been read in
#> ✔ /tmp/RtmpksiKAO/066-UNAVAILABILITY_OF_OFFSHORE_GRID_202511041034-202511062114.xml has been read in
#> ✔ /tmp/RtmpksiKAO/067-UNAVAILABILITY_OF_OFFSHORE_GRID_202511051433-202511051733.xml has been read in
#> ✔ /tmp/RtmpksiKAO/068-UNAVAILABILITY_OF_OFFSHORE_GRID_202511051433-202511051735.xml has been read in
#> ✔ /tmp/RtmpksiKAO/069-UNAVAILABILITY_OF_OFFSHORE_GRID_202511051433-202511051738.xml has been read in
#> ✔ /tmp/RtmpksiKAO/070-UNAVAILABILITY_OF_OFFSHORE_GRID_202511051433-202511051738.xml has been read in
#> ✔ /tmp/RtmpksiKAO/071-UNAVAILABILITY_OF_OFFSHORE_GRID_202511051433-202511061250.xml has been read in
#> ✔ /tmp/RtmpksiKAO/072-UNAVAILABILITY_OF_OFFSHORE_GRID_202511180945-202511181350.xml has been read in
#> ✔ /tmp/RtmpksiKAO/073-UNAVAILABILITY_OF_OFFSHORE_GRID_202511180945-202511181410.xml has been read in
#> ✔ /tmp/RtmpksiKAO/074-UNAVAILABILITY_OF_OFFSHORE_GRID_202511180945-202511181410.xml has been read in
#> ✔ /tmp/RtmpksiKAO/075-UNAVAILABILITY_OF_OFFSHORE_GRID_202511180945-202511181416.xml has been read in
#> ✔ /tmp/RtmpksiKAO/076-UNAVAILABILITY_OF_OFFSHORE_GRID_202511180945-202511191100.xml has been read in
#> ✔ /tmp/RtmpksiKAO/077-UNAVAILABILITY_OF_OFFSHORE_GRID_202511211342-202511221712.xml has been read in
#> ✔ /tmp/RtmpksiKAO/078-UNAVAILABILITY_OF_OFFSHORE_GRID_202511221600-202511221843.xml has been read in
#> ✔ /tmp/RtmpksiKAO/079-UNAVAILABILITY_OF_OFFSHORE_GRID_202511221712-202511221843.xml has been read in
#> ✔ /tmp/RtmpksiKAO/080-UNAVAILABILITY_OF_OFFSHORE_GRID_202511280802-202511281942.xml has been read in
#> ✔ /tmp/RtmpksiKAO/081-UNAVAILABILITY_OF_OFFSHORE_GRID_202511280830-202511281726.xml has been read in
#> ✔ /tmp/RtmpksiKAO/082-UNAVAILABILITY_OF_OFFSHORE_GRID_202511280830-202511281726.xml has been read in
#> ✔ /tmp/RtmpksiKAO/083-UNAVAILABILITY_OF_OFFSHORE_GRID_202511281419-202511281638.xml has been read in
#> ✔ /tmp/RtmpksiKAO/084-UNAVAILABILITY_OF_OFFSHORE_GRID_202511281419-202511281645.xml has been read in
#> ✔ /tmp/RtmpksiKAO/085-UNAVAILABILITY_OF_OFFSHORE_GRID_202511281419-202511281645.xml has been read in
#> ✔ /tmp/RtmpksiKAO/086-UNAVAILABILITY_OF_OFFSHORE_GRID_202511281419-202512011215.xml has been read in
#> ✔ /tmp/RtmpksiKAO/087-UNAVAILABILITY_OF_OFFSHORE_GRID_202511290920-202511291030.xml has been read in
#> ✔ /tmp/RtmpksiKAO/088-UNAVAILABILITY_OF_OFFSHORE_GRID_202511290920-202511291115.xml has been read in
#> ✔ /tmp/RtmpksiKAO/089-UNAVAILABILITY_OF_OFFSHORE_GRID_202511290920-202512051425.xml has been read in
#> ✔ /tmp/RtmpksiKAO/090-UNAVAILABILITY_OF_OFFSHORE_GRID_202511290920-202512051425.xml has been read in
#> ✔ /tmp/RtmpksiKAO/091-UNAVAILABILITY_OF_OFFSHORE_GRID_202511301021-202511301807.xml has been read in
#> ✔ /tmp/RtmpksiKAO/092-UNAVAILABILITY_OF_OFFSHORE_GRID_202512010600-202512011844.xml has been read in
#> ✔ /tmp/RtmpksiKAO/093-UNAVAILABILITY_OF_OFFSHORE_GRID_202512011201-202512011303.xml has been read in
#> ✔ /tmp/RtmpksiKAO/094-UNAVAILABILITY_OF_OFFSHORE_GRID_202512011202-202512011303.xml has been read in
#> ✔ /tmp/RtmpksiKAO/095-UNAVAILABILITY_OF_OFFSHORE_GRID_202512011203-202512011310.xml has been read in
#> ✔ /tmp/RtmpksiKAO/096-UNAVAILABILITY_OF_OFFSHORE_GRID_202512021500-202512021947.xml has been read in
#> ✔ /tmp/RtmpksiKAO/097-UNAVAILABILITY_OF_OFFSHORE_GRID_202512031300-202512031935.xml has been read in
#> ✔ /tmp/RtmpksiKAO/098-UNAVAILABILITY_OF_OFFSHORE_GRID_202512041000-202512041953.xml has been read in
#> ✔ /tmp/RtmpksiKAO/099-UNAVAILABILITY_OF_OFFSHORE_GRID_202512051600-202512051612.xml has been read in
#> ✔ /tmp/RtmpksiKAO/100-UNAVAILABILITY_OF_OFFSHORE_GRID_202512051600-202512051612.xml has been read in
#> ✔ /tmp/RtmpksiKAO/101-UNAVAILABILITY_OF_OFFSHORE_GRID_202512160711-202512160911.xml has been read in
#> ✔ /tmp/RtmpksiKAO/102-UNAVAILABILITY_OF_OFFSHORE_GRID_202512160711-202512160911.xml has been read in
#> ✔ /tmp/RtmpksiKAO/103-UNAVAILABILITY_OF_OFFSHORE_GRID_202512160711-202512160912.xml has been read in
#> ✔ /tmp/RtmpksiKAO/104-UNAVAILABILITY_OF_OFFSHORE_GRID_202512160711-202512160912.xml has been read in
#> ✔ /tmp/RtmpksiKAO/105-UNAVAILABILITY_OF_OFFSHORE_GRID_202512160711-202512161518.xml has been read in
#> ✔ /tmp/RtmpksiKAO/106-UNAVAILABILITY_OF_OFFSHORE_GRID_202512211618-202512211807.xml has been read in
#> ✔ /tmp/RtmpksiKAO/107-UNAVAILABILITY_OF_OFFSHORE_GRID_202512211618-202512211807.xml has been read in
#> ✔ /tmp/RtmpksiKAO/108-UNAVAILABILITY_OF_OFFSHORE_GRID_202512211618-202512241825.xml has been read in
#> ✔ /tmp/RtmpksiKAO/109-UNAVAILABILITY_OF_OFFSHORE_GRID_202512211618-202512241825.xml has been read in
#> ✔ /tmp/RtmpksiKAO/110-UNAVAILABILITY_OF_OFFSHORE_GRID_202601101024-202601101621.xml has been read in
#> ✔ /tmp/RtmpksiKAO/111-UNAVAILABILITY_OF_OFFSHORE_GRID_202601101024-202601101621.xml has been read in
#> ✔ /tmp/RtmpksiKAO/112-UNAVAILABILITY_OF_OFFSHORE_GRID_202601101024-202601101621.xml has been read in
#> ✔ /tmp/RtmpksiKAO/113-UNAVAILABILITY_OF_OFFSHORE_GRID_202601101024-202601101625.xml has been read in
#> ✔ /tmp/RtmpksiKAO/114-UNAVAILABILITY_OF_OFFSHORE_GRID_202601101024-202601101625.xml has been read in
#> ✔ /tmp/RtmpksiKAO/115-UNAVAILABILITY_OF_OFFSHORE_GRID_202601141254-202601141548.xml has been read in
#> ✔ /tmp/RtmpksiKAO/116-UNAVAILABILITY_OF_OFFSHORE_GRID_202601141254-202601141548.xml has been read in
#> ✔ /tmp/RtmpksiKAO/117-UNAVAILABILITY_OF_OFFSHORE_GRID_202601141254-202601141548.xml has been read in
#> ✔ /tmp/RtmpksiKAO/118-UNAVAILABILITY_OF_OFFSHORE_GRID_202601141254-202601141548.xml has been read in
#> ✔ /tmp/RtmpksiKAO/119-UNAVAILABILITY_OF_OFFSHORE_GRID_202601141254-202601141548.xml has been read in
#> ✔ /tmp/RtmpksiKAO/120-UNAVAILABILITY_OF_OFFSHORE_GRID_202601141254-202601141552.xml has been read in
#> ✔ /tmp/RtmpksiKAO/121-UNAVAILABILITY_OF_OFFSHORE_GRID_202601141254-202601141552.xml has been read in
#> ✔ /tmp/RtmpksiKAO/122-UNAVAILABILITY_OF_OFFSHORE_GRID_202601161828-202601181842.xml has been read in
#> ✔ /tmp/RtmpksiKAO/123-UNAVAILABILITY_OF_OFFSHORE_GRID_202601161828-202601181842.xml has been read in
#> ✔ /tmp/RtmpksiKAO/124-UNAVAILABILITY_OF_OFFSHORE_GRID_202601161828-202601181845.xml has been read in
#> ✔ /tmp/RtmpksiKAO/125-UNAVAILABILITY_OF_OFFSHORE_GRID_202601161828-202601181847.xml has been read in
#> ✔ /tmp/RtmpksiKAO/126-UNAVAILABILITY_OF_OFFSHORE_GRID_202601161828-202601181900.xml has been read in
#> ✔ /tmp/RtmpksiKAO/127-UNAVAILABILITY_OF_OFFSHORE_GRID_202601161828-202601181902.xml has been read in
#> ✔ /tmp/RtmpksiKAO/128-UNAVAILABILITY_OF_OFFSHORE_GRID_202601231412-202601231604.xml has been read in
#> ✔ /tmp/RtmpksiKAO/129-UNAVAILABILITY_OF_OFFSHORE_GRID_202601231412-202601231604.xml has been read in
#> ✔ /tmp/RtmpksiKAO/130-UNAVAILABILITY_OF_OFFSHORE_GRID_202601251259-202601251300.xml has been read in
#> ✔ /tmp/RtmpksiKAO/131-UNAVAILABILITY_OF_OFFSHORE_GRID_202601271143-202601271446.xml has been read in
#> ✔ /tmp/RtmpksiKAO/132-UNAVAILABILITY_OF_OFFSHORE_GRID_202601290809-202601291115.xml has been read in
#> ✔ /tmp/RtmpksiKAO/133-UNAVAILABILITY_OF_OFFSHORE_GRID_202602151520-202602151838.xml has been read in
#> ✔ /tmp/RtmpksiKAO/134-UNAVAILABILITY_OF_OFFSHORE_GRID_202602151520-202602151838.xml has been read in
#> ✔ /tmp/RtmpksiKAO/135-UNAVAILABILITY_OF_OFFSHORE_GRID_202602151520-202602151838.xml has been read in
#> ✔ /tmp/RtmpksiKAO/136-UNAVAILABILITY_OF_OFFSHORE_GRID_202602151520-202602151906.xml has been read in
#> ✔ /tmp/RtmpksiKAO/137-UNAVAILABILITY_OF_OFFSHORE_GRID_202602151520-202602151906.xml has been read in
#> ✔ /tmp/RtmpksiKAO/138-UNAVAILABILITY_OF_OFFSHORE_GRID_202602151520-202602151908.xml has been read in
#> ✔ /tmp/RtmpksiKAO/139-UNAVAILABILITY_OF_OFFSHORE_GRID_202602151520-202602151908.xml has been read in
#> ✔ /tmp/RtmpksiKAO/140-UNAVAILABILITY_OF_OFFSHORE_GRID_202602170855-202602171138.xml has been read in
#> ✔ /tmp/RtmpksiKAO/141-UNAVAILABILITY_OF_OFFSHORE_GRID_202602170855-202602171138.xml has been read in
#> ✔ /tmp/RtmpksiKAO/142-UNAVAILABILITY_OF_OFFSHORE_GRID_202602170855-202602171138.xml has been read in
#> ✔ /tmp/RtmpksiKAO/143-UNAVAILABILITY_OF_OFFSHORE_GRID_202602170855-202602171144.xml has been read in
#> ✔ /tmp/RtmpksiKAO/144-UNAVAILABILITY_OF_OFFSHORE_GRID_202602170855-202602171144.xml has been read in
#> ✔ /tmp/RtmpksiKAO/145-UNAVAILABILITY_OF_OFFSHORE_GRID_202602170855-202602171145.xml has been read in
#> ✔ /tmp/RtmpksiKAO/146-UNAVAILABILITY_OF_OFFSHORE_GRID_202602170855-202602171145.xml has been read in
#> ✔ /tmp/RtmpksiKAO/147-UNAVAILABILITY_OF_OFFSHORE_GRID_202602220913-202602221116.xml has been read in
#> ✔ /tmp/RtmpksiKAO/148-UNAVAILABILITY_OF_OFFSHORE_GRID_202602220913-202602221116.xml has been read in
#> ✔ /tmp/RtmpksiKAO/149-UNAVAILABILITY_OF_OFFSHORE_GRID_202602220913-202602221116.xml has been read in
#> ✔ /tmp/RtmpksiKAO/150-UNAVAILABILITY_OF_OFFSHORE_GRID_202602220913-202602221122.xml has been read in
#> ✔ /tmp/RtmpksiKAO/151-UNAVAILABILITY_OF_OFFSHORE_GRID_202602220913-202602221122.xml has been read in
#> ✔ /tmp/RtmpksiKAO/152-UNAVAILABILITY_OF_OFFSHORE_GRID_202602220913-202602221126.xml has been read in
#> ✔ /tmp/RtmpksiKAO/153-UNAVAILABILITY_OF_OFFSHORE_GRID_202602220913-202602221126.xml has been read in
#> ✔ /tmp/RtmpksiKAO/154-UNAVAILABILITY_OF_OFFSHORE_GRID_202602221637-202602221855.xml has been read in
#> ✔ /tmp/RtmpksiKAO/155-UNAVAILABILITY_OF_OFFSHORE_GRID_202602221637-202602221856.xml has been read in
#> ✔ /tmp/RtmpksiKAO/156-UNAVAILABILITY_OF_OFFSHORE_GRID_202602221637-202603072100.xml has been read in
#> ✔ /tmp/RtmpksiKAO/157-UNAVAILABILITY_OF_OFFSHORE_GRID_202603031122-202603062259.xml has been read in
#> processing xml list ■■■                                8% | ETA: 12s
#> processing xml list ■■■■■■                            18% | ETA: 11s
#> processing xml list ■■■■■■■■■■■■                      38% | ETA:  9s
#> processing xml list ■■■■■■■■■■■■■■■■■■■               59% | ETA:  6s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■        82% | ETA:  2s
#> processing xml list ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s

str(df)
#> tibble [199,351 × 27] (S3: tbl_df/tbl/data.frame)
#>  $ ts_bidding_zone_domain_mrid       : chr [1:199351] "10Y1001A1001A82H" "10Y1001A1001A82H" "10Y1001A1001A82H" "10Y1001A1001A82H" ...
#>  $ ts_bidding_zone_domain_name       : chr [1:199351] "Germany_Luxemburg" "Germany_Luxemburg" "Germany_Luxemburg" "Germany_Luxemburg" ...
#>  $ ts_asset_location_name            : chr [1:199351] "BORKUM RIFFGRUND 1-DOLWIN ALPHA" "BORKUM RIFFGRUND 1-DOLWIN ALPHA" "BORKUM RIFFGRUND 1-DOLWIN ALPHA" "BORKUM RIFFGRUND 1-DOLWIN ALPHA" ...
#>  $ ts_asset_mrid                     : chr [1:199351] "11TD2L000000281U" "11TD2L000000281U" "11TD2L000000281U" "11TD2L000000281U" ...
#>  $ ts_asset_name                     : chr [1:199351] "TTG/155/BORKUM RIFFGRUND 1-DOLWIN ALPHA/AC130" "TTG/155/BORKUM RIFFGRUND 1-DOLWIN ALPHA/AC130" "TTG/155/BORKUM RIFFGRUND 1-DOLWIN ALPHA/AC130" "TTG/155/BORKUM RIFFGRUND 1-DOLWIN ALPHA/AC130" ...
#>  $ type                              : chr [1:199351] "A79" "A79" "A79" "A79" ...
#>  $ type_def                          : chr [1:199351] "Offshore grid infrastructure unavailability" "Offshore grid infrastructure unavailability" "Offshore grid infrastructure unavailability" "Offshore grid infrastructure unavailability" ...
#>  $ process_type                      : chr [1:199351] "A26" "A26" "A26" "A26" ...
#>  $ process_type_def                  : chr [1:199351] "Outage information" "Outage information" "Outage information" "Outage information" ...
#>  $ ts_business_type                  : chr [1:199351] "A54" "A54" "A54" "A54" ...
#>  $ ts_business_type_def              : chr [1:199351] "Unplanned outage" "Unplanned outage" "Unplanned outage" "Unplanned outage" ...
#>  $ ts_asset_psr_type                 : chr [1:199351] "B21" "B21" "B21" "B21" ...
#>  $ ts_asset_psr_type_def             : chr [1:199351] "AC Link" "AC Link" "AC Link" "AC Link" ...
#>  $ created_date_time                 : POSIXct[1:199351], format: "2025-09-16 10:15:05" "2025-09-16 10:15:05" "2025-09-16 10:15:05" "2025-09-16 10:15:05" ...
#>  $ reason_code                       : chr [1:199351] "B18" "B18" "B18" "B18" ...
#>  $ reason_text                       : chr [1:199351] "Failure" "Failure" "Failure" "Failure" ...
#>  $ revision_number                   : num [1:199351] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ unavailability_time_interval_start: POSIXct[1:199351], format: "2025-03-26 15:49:00" "2025-03-26 15:49:00" "2025-03-26 15:49:00" "2025-03-26 15:49:00" ...
#>  $ unavailability_time_interval_end  : POSIXct[1:199351], format: "2025-03-27 06:00:00" "2025-03-27 06:00:00" "2025-03-27 06:00:00" "2025-03-27 06:00:00" ...
#>  $ ts_resolution                     : chr [1:199351] "PT1M" "PT1M" "PT1M" "PT1M" ...
#>  $ ts_time_interval_start            : POSIXct[1:199351], format: "2025-03-26 15:49:00" "2025-03-26 15:49:00" "2025-03-26 15:49:00" "2025-03-26 15:49:00" ...
#>  $ ts_time_interval_end              : POSIXct[1:199351], format: "2025-03-27 06:00:00" "2025-03-27 06:00:00" "2025-03-27 06:00:00" "2025-03-27 06:00:00" ...
#>  $ ts_mrid                           : num [1:199351] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ ts_point_dt_start                 : POSIXct[1:199351], format: "2025-03-26 15:49:00" "2025-03-26 15:50:00" "2025-03-26 15:51:00" "2025-03-26 15:52:00" ...
#>  $ ts_production_psr_nominal_p       : num [1:199351] 156 156 156 156 156 156 156 156 156 156 ...
#>  $ ts_point_quantity                 : num [1:199351] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ ts_quantity_measure_unit_name     : chr [1:199351] "MAW" "MAW" "MAW" "MAW" ...
```
