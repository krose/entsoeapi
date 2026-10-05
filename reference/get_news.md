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
if (FALSE) { # there_is_provider()
entsoeapi::get_news()
}
```
