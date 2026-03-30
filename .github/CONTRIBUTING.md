# Contributing to entsoeapi

Thank you for taking the time to contribute! This document outlines the process for reporting issues and submitting code changes.

## Code of Conduct

Please review and follow our [Code of Conduct](CODE_OF_CONDUCT.md).

## Reporting bugs

Use the [bug report template](https://github.com/krose/entsoeapi/issues/new?template=bug_report.md) and include:

-   A minimal reproducible example (ideally using `reprex::reprex()`)
-   The ENTSO-E API endpoint involved and the query parameters used
-   Your OS, R version, and `entsoeapi` version (`packageVersion("entsoeapi")`)

Do **not** include your ENTSO-E security token in the report.

## Requesting features

Open a [feature request](https://github.com/krose/entsoeapi/issues/new?template=feature_request.md) describing the ENTSO-E endpoint or data domain you would like covered and a brief use case.

## Submitting a pull request

1.  Fork the repository and create a branch from `main`.

2.  Install development dependencies: `devtools::install_dev_deps()`.

3.  Make your changes. Keep each PR focused on a single concern.

4.  Add or update tests — the package targets 100% test coverage.

5.  Run the full check suite locally before pushing:

    ``` r
    lintr::lint_package()
    devtools::document()
    devtools::test()
    devtools::check()
    covr::package_coverage()
    ```

6.  Open the pull request against `main` and describe what changed and why.

### Conventions

-   Follow the existing code style (snake_case, `cli::` for user-facing messages, `checkmate::` for input validation).
-   New user-facing functions must have `@examplesIf` blocks (see existing functions for the pattern).
-   All exported functions need a `@return` tag in their roxygen documentation.
-   ENTSO-E security tokens must never appear in code, tests, or fixtures — use the `ENTSOE_PAT` environment variable.

## Questions

For general usage questions, open a [GitHub Discussion](https://github.com/krose/entsoeapi/discussions) rather than an issue.
