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
#> ── Incorrect values for Belgian Day-Ahead generation forecast - 14.1.C ──
#> 
#> ℹ Wed, 23 Sep 2026 09:06:36 GMT
#> Dear Transparency Platform users,Since 1 September, there have been issues affecting the Belgium Day-Ahead Generation
#> Forecast (14.1.C), causing the published values to be incorrect and unusable. Elia is actively working to resolve the
#> issue as quickly as possible and apologizes for any inconvenience caused.Thanks for your understanding. Kind
#> regards,Transparency Platform team on behalf of Elia
#> 
#> ── Transparency Platform Web API and Subscriptions Service Disruption ──
#> 
#> ℹ Tue, 22 Sep 2026 15:39:58 GMT
#> Dear Transparency Platform users,We would like to inform you about the Transparency Platform delays from the Web API
#> and Subscriptions since 14:05 CEST, due to an Azure infrastructure incident impacting several TP services. We are
#> waiting for a resolution before full service can be restored.We understand the impact that this disruption may have on
#> automated processes and data retrieval activities, and we apologize for the inconvenience caused.We will provide
#> further updates as soon as more information becomes available or the service can be restored.In the meantime, the
#> Transparency Platform File Library (including FMS API) remain functional (link to the guide).Thank you for your
#> patience and understanding.Kind regards,Transparency Platform team 
#> 
#> ── HOPS: Delays of Balancing publications for Croatia ──
#> 
#> ℹ Mon, 21 Sep 2026 14:36:47 GMT
#> Dear Transparency Platform users,Please be informed that Balancing publication (Prices of Activated Balancing Energy
#> and Aggregated Balancing Energy Bids for mFRR) are currently delayed due to technical issues.HOPS is working on
#> resolving unexpected issues and restoring the regular publication of the data as soon as possible.Please accept HOPS
#> apologies for any inconvenience and thank you for your understanding.Kind regards,Transparency Platform team on behalf
#> of HOPS
#> 
#> ── Transparency Platform stabilized following the infrastructure migration ──
#> 
#> ℹ Mon, 14 Sep 2026 14:14:38 GMT
#> Dear Transparency Platform users,We would like to provide an update regarding the stabilization of the Transparency
#> Platform following the migration to the new infrastructure.The platform has now been stabilized, and the previously
#> observed API performance issues and publication backlog have been resolved. Platform services are operating normally,
#> and we continue to monitor the environment to ensure sustained performance and reliability.As part of the final
#> recovery activities:The temporary API request limits are planned be lifted on 15 September 2026 and it will be possible
#> to download 1 year of data with a single request.The temporary website export limits are expected to be lifted on 16
#> September 2026.Subscribers may still experience slight delays in the processing and distribution of subscription
#> messages due to the higher volume of data that has been published in the past days. Our IT providers continue to
#> monitor this area closely and expect the remaining delays to be progressively eliminated.We would like to thank all
#> users for their patience and understanding throughout the migration and stabilization period. We sincerely apologise
#> for the disruption and inconvenience caused.Kind regards,Transparency Platform team
#> 
#> ── Update: Progress on restoring Transparency Platform service following the infrastructure migration ──
#> 
#> ℹ Wed, 09 Sep 2026 06:55:16 GMT
#> Dear Transparency Platform users,We would like to provide an update regarding the ongoing stabilization of the
#> Transparency Platform following the migration to the new infrastructure.The Web API service is now available again.
#> However, we are observing a gradual degradation in API performance, which is being actively monitored and investigated
#> by our service provider.In addition, data processing and publication delays continue to affect certain platform
#> services. As a result, some information may be published later than expected, and subscription deliveries may also be
#> impacted.Our IT providers are continuing their investigation into the underlying causes of both the publication delays
#> and the API performance issues and are working to restore normal service levels as quickly as possible.The Transparency
#> Platform File Library remains available and can be used to download published data in bulk.We sincerely apologise for
#> the inconvenience caused by these ongoing issues and understand the impact they may have on your operations. We
#> appreciate your patience and understanding while we work to fully restore platform stability and performance.Further
#> updates will be provided as significant progress is made.Kind regards,Transparency Platform team
```
