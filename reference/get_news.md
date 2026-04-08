# Display the ENTSO-E Transparency Platform news feed

Fetches the RSS news feed from the ENTSO-E Transparency Platform and
displays the entries in the console. Useful for checking platform
maintenance windows, data publication delays, and other announcements
that may affect API availability.

## Usage

``` r
get_news(feed_url = .feed_url, n = 5L)
```

## Arguments

- feed_url:

  the URL of the RSS news feed from the ENTSO-E Transparency Platform.

- n:

  Integer scalar. Maximum number of feed items to display. Defaults to
  `5L`. Use `Inf` to show all items.

## Value

A tibble of feed items with columns `title`, `pub_date`, and
`description`, returned invisibly.

## Examples

``` r
entsoeapi::get_news()
#> 
#> ── ENTSO-E Transparency Platform News ──────────────────────────────────────────────────────────────────────────────────
#> 
#> ── Transparency Platform Quarterly Newsletter subscription  ──
#> 
#> ℹ Tue, 07 Apr 2026 12:45:33 GMT
#> Dear Transparency Platform users,We are pleased to introduce the Transparency Platform Quarterly Newsletter. This
#> newsletter will cover topics such as feature releases, user group meeting announcements, planned events, and any
#> service interruptions.To subscribe, please use the following LINK.Kind regards,Transparency Platform team 
#> 
#> ── PSE: Republication of data under Article 12.1.f  ──
#> 
#> ℹ Fri, 03 Apr 2026 11:59:36 GMT
#> Dear Transparency Platform users,Polskie Sieci Elektroenergetyczne (PSE) hereby informs stakeholders of the
#> republication of data released pursuant to Article 12.1.f. The updated dataset now covers the period commencing on 19
#> March 2025. The data have been republished for both directions, and no netting has been applied.We extend our sincere
#> apologies for any inconvenience this republication may have caused.Kind regards,Transparency Platform team on behalf of
#> PSE
#> 
#> ── R3.19.0.3 on Thursday 02.04.2026 at 15:00 - 16:30 CEST ──
#> 
#> ℹ Wed, 01 Apr 2026 14:56:52 GMT
#> Dear Transparency Platform Users,The deployment of TP Release R3.19.0.3 on the PROD environment is scheduled to begin
#> on Thursday, 2nd April 2026 at 15:00 CEST.Please note that the platform will be unavailable for up to 90 minutes during
#> this deployment window.Scope of Release R3.19.0.3:Publications & Filtering:Wrong resolution published in 11.1. Flow
#> based processed (PT15M→PT60M)Time Horizon & Source published as codes for 17.1.B&C Volumes and Prices of the Contracted
#> Balancing ReservesForecasted Transfer Capacities [11.1] data published with incorrect time zone10.1.A&B Unavailability
#> Transmission Grid - Improvements to filters and sortingGUI Exports improvements:10.1.A&B Unavailability in Transmission
#> Grid - Unexpected error while downloading data12.3.E - Aggregated Balancing Energy Bids – XML export results in
#> errorGUI Performance Improvements:15.1.ABCD - Unavailability of Production & Generation Units chart doesn't
#> load"Published Network Elements" data view errorsOther Marker Information[OMI] - unexpected error12.1.D Energy Prices
#> for DE‑LU Data view errorREST API improvements:Use of the Transfer Capacity [12.1.A] returns wrong periodsAll Outages 4
#> parameters (periodStart, periodEnd, PeriodStartUpdate, PeriodEndUpdate) are ignored when MRID is included11.1
#> Continuous Evolution - Contract_MarketAgreement.Type should remain an optional parameter13.1.A - Redispatches
#> [Internal, Cross Border] , 13.1.B - Contertrading - Corrections to attributes in XML downloads and API responseBest
#> regards,Transparency Platform Team
#> 
#> ── TP PROD Maintenance on 26.03.2026 at 15:00 CET up to 2 hrs ──
#> 
#> ℹ Wed, 01 Apr 2026 09:58:16 GMT
#> Dear Transparency Platform users,Transparency Platform Service Provider will be performing a planned maintenance on
#> 26.03.2026 at 15:00 CET with up to 2 hours outage of the TP PROD environment. During outage period, the Transparency
#> Platform will not be available.Thank you in advance for your understanding.Best regards,Transparency Platform team
#> 
#> ── TP PROD Emergency Maintenance on 04.03.2026 at 17:00 CET up to 1hr ──
#> 
#> ℹ Wed, 01 Apr 2026 09:58:10 GMT
#> Dear Transparency Platform users,Due to the performance issue Service Provider will be perform an emergency outage of
#> the TP PROD environment.During outage period, the Transparency Platform will not be fully available.The outage will
#> take place today at 17:00 CET and is expected to last approximately 60 minutes.Thank you in advance for your
#> understanding.Best regards,Transparency Platform team
```
