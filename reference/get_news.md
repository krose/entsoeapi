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
#> ── Issues with the Transparency Platform API, subscriptions and File Library ──
#> 
#> ℹ Mon, 30 Mar 2026 13:47:39 GMT
#> Dear Transparency Platform users,We are currently experiencing issues affecting the availability of the API service. In
#> addition, delays have been detected in Subscriptions and in the publication of new data in the File Library extracts.
#> Our IT provider is actively investigating these disruptions as a priority, and working on a fix for them. Thank you for
#> your understanding and apologies for the inconvenience caused.Kind regards,Transparency Platform team
#> 
#> ── TP PROD data publication delays ──
#> 
#> ℹ Thu, 26 Mar 2026 13:31:47 GMT
#> Dear Transparency Platform users,The Transparency Platform is experiencing performance issues since 24th March 2026.
#> These issues are affecting data processing, which is causing delays in data publication. Hence, please expect delays up
#> to 10 hours for receiving the updates via all the download channels (API, FMS, Subscriptions and Website downloads).Our
#> service provider is working on the issue with the highest priority.We sincerely apologize for the inconvenience and
#> thank you for your patience and understanding.Best regards,Transparency Platform team
#> 
#> ── Reminder: Transparency Platform Quarterly Newsletter subscription  ──
#> 
#> ℹ Tue, 24 Mar 2026 12:18:19 GMT
#> Dear Transparency Platform users,We are pleased to introduce the Transparency Platform Quarterly Newsletter. This
#> newsletter will cover topics such as feature releases, user group meeting announcements, planned events, and any
#> service interruptions.To subscribe, please use the following LINK.Kind regards,Transparency Platform team 
#> 
#> ── Missing Energy Prices [12.1.D] on 11/03/2026 for Multiple Bidding Zones ──
#> 
#> ℹ Wed, 11 Mar 2026 10:35:00 GMT
#> Dear Transparency Platform users,Please be informed that Energy Prices [12.1.D] for multiple bidding zones provided by
#> data provider JAO - [DE_LU Sequence 1, GR, HR, HU, NL, RO, SK] - are currently missing on the Transparency Platform
#> starting from 11/03/2026 because of no submissions. The data provider has been notified.Data for Energy Prices [12.1.D]
#> bidding zone DE_LU Sequence 2 provided by Tennet DE, is also missing starting from 11/03/2026. The data provider has
#> been notified.We sincerely apologize for the inconvenience and thank you for your patience and understanding.Best
#> regards,Transparency Platform Team
#> 
#> ── HOPS: Planned maintenance of the IT infrastructure on 11th of March 2026 ──
#> 
#> ℹ Tue, 10 Mar 2026 09:48:07 GMT
#> Dear Transparency Platform users,Please be informed that maintenance works on HOPS’s systems will take place on 11
#> March 2026 from 15:00 to 20:00 CET.During this period, there may be potential interruptions in the publication of data
#> on the ENTSO-E Transparency Platform, where certain data may be temporarily unavailable or published with a delay.After
#> the maintenance HOPS will re-upload all missing data to PRO TP.Kind regards,Transparency Platform team on behalf of
#> HOPS
```
