## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

- local Linux (Fedora), R 4.4.x
- R-hub: windows (R-devel), linux (R-devel), macos-arm64 (R-devel)

### URL returning 403

The URL `https://transparencyplatform.zendesk.com/hc/en-us/articles/12845911031188-How-to-get-security-token`
(README.md) returns HTTP 403 to automated checkers because Zendesk blocks
non-browser user agents. The link is valid and accessible in a browser.

## Notes for CRAN reviewers

### Examples wrapped in `\dontrun{}`

All exported functions require a registered ENTSO-E security token
(`ENTSOE_PAT` environment variable) to make live API calls. For this
reason, all examples are wrapped in `\dontrun{}`. There is no way to
provide a meaningful, runnable example without a valid token.

### Method references

There are no published references describing the methods in this package.
The package implements R wrappers for the publicly documented ENTSO-E
Transparency Platform REST API
(<https://transparency.entsoe.eu/content/static_content/Static%20content/web%20api/Guide.html>).
