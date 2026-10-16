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
#> ── TP scheduled maintenance on Thursday, 15 October, between 18:00 and 20:00 CEST ──
#> 
#> ℹ Thu, 15 Oct 2026 12:25:33 GMT
#> Dear Transparency Platform users,The Transparency Platform Production environment will be under scheduled maintenance
#> on Thursday, 15 October, between 18:00 and 20:00 CEST.During this maintenance, you may experience brief service
#> interruptions or short connectivity issues.We apologize for any inconvenience and appreciate your understanding.Best
#> regards,Transparency Platform team
#> 
#> ── TP ECP maintenance 13.10.2026 between 13:00 and 13:30 CEST ──
#> 
#> ℹ Mon, 12 Oct 2026 10:03:36 GMT
#> Dear Transparency Platform users,The Transparency Platform Service Provider will perform  ECP  maintenance on
#> 13/10/2026 at 13:00 CEST in the Production environment, which is expected to last up to 30 minutes.During this period,
#> the Transparency Platform will not be able to send or receive messages via ECP up to 15 min.Best regards,Transparency
#> Platform team
#> 
#> ── RTE – IT issue impacting publication of GL EB 12.3.E and TR 17.1.F for “mFRR Specific” and “RR Specific” products ──
#> 
#> ℹ Thu, 08 Oct 2026 10:33:03 GMT
#> Dear Transparency Platform users,Please be informed that since Monday, 05 October 2026, at approximately 10:00 CET, RTE
#> has been experiencing an internal IT issue affecting some data delivery to the Transparency Platform.As a result, two
#> Data Items scheduled for publication since that time has not been successfully submitted and is therefore unavailable
#> on the platform:[GL EB 12.3.E] Aggregated Balancing Energy Bids for “mFRR Specific” and “RR Specific” products[TR
#> 17.1.F] Prices of Activated Balancing Energy for “mFRR Specific” and “RR Specific” productsThe other types of products
#> (aFRR LS Standard, aFRR CS Standard, mFRR Standard and mFRR SA Standard) are not impacted by this IT issue and are well
#> published on the Transparency Platform.Our teams are actively working to resolve the issue and restore normal data
#> delivery as quickly as possible.We sincerely apologize for any inconvenience caused.Kind regards,Transparency Platform
#> team on behalf of RTE
#> 
#> ── Communication Issue between Transelectrica and the Transparency Platform ──
#> 
#> ℹ Tue, 06 Oct 2026 13:32:34 GMT
#> Dear Transparency Platform users,Transelectrica is currently experiencing a communication issue with the Transparency
#> Platform, which affects the publication process on TP.We are working on resolving the unexpected issues and restoring
#> the regular publication of the data as soon as possible.Best regards,Transparency Platform Team on behalf of TEL
#> 
#> ── MAVIR is scheduled to introduce standard aFRR and mFRR products from 01.10.2026 (00h00 local time). ──
#> 
#> ℹ Tue, 06 Oct 2026 13:09:51 GMT
#> Dear Transparency Platform users,MAVIR is scheduled to introduce standard aFRR and mFRR products from 01.10.2026 (00h00
#> local time).A preparatory test with standard aFRR products will be performed on 10.09.2026. 14:00-16:00 local time,
#> related activities ranging between 13:00-17:00. Further information can be found on MAVIR's webpage (only in
#> Hungarian): BSP munkacsoport (MARI-PICASSO) - MAVIR - Site (https://mavir.hu/web/mavir/bsp-munkacsoport)During the test
#> period, some near real-time BALANCING publications will not be available or will only be partially available.Affected
#> dataAggregated Balancing Energy Bids GL EB 12.3.EBalancing Energy bids GL EB 12.3.B&CPrices of Activated Balancing
#> Energy & aFRR CBMPs TR 17.1.F, IF aFRR 3.16Imbalance TR 17.1.G&HVolumes and Prices of the Contracted Balancing Reserves
#> TR 17.1.B&CProcured Capacity GL EB 12.3.FCurrent Balancing State GL EB 12.3.AKind regards,Transparency Platform team on
#> behalf of Mavir
```
