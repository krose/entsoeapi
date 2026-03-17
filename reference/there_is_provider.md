# Check if the ENTSO-E API provider is reachable

Sends a probe request to the ENTSO-E Transparency Platform API and
returns `TRUE` if the server responds with HTTP 401 Unauthorized
(meaning the endpoint is up but the dummy token was rejected). Returns
`FALSE` when there is no internet connection or when the server is
unreachable. Primarily intended as an `@examplesIf` guard in package
documentation.

## Usage

``` r
there_is_provider(
  api_scheme = .api_scheme,
  api_domain = .api_domain,
  api_name = .api_name
)
```

## Arguments

- api_scheme:

  Character. URL scheme, default `"https://"`.

- api_domain:

  Character. API host, default `"web-api.tp.entsoe.eu/"`.

- api_name:

  Character. API path prefix, default `"api?"`.

## Value

A single logical value.

## Examples

``` r
there_is_provider()
#> [1] TRUE
```
