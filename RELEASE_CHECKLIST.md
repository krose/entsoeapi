# Release Checklist — entsoeapi

## 1. Prepare

The package name is valid and available
`pak::pkg_name_check("entsoeapi")`

All planned changes merged into `devel`

`NEWS.md` updated — new version header, all changes documented

Version bumped in `DESCRIPTION` (follow semver: major.minor.patch)

`RoxygenNote` in `DESCRIPTION` matches installed roxygen2 version

Run `lintr::lint_package()` lint the scripts following general
formatting rules

## 2. Documentation

Run
[`devtools::spell_check()`](https://devtools.r-lib.org/reference/spell_check.html)
— no spelling errors in docs

[`urlchecker::url_check()`](https://rdrr.io/pkg/urlchecker/man/url_check.html)
— no broken URLs in docs

Run
[`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
— regenerate all `.Rd` files and `NAMESPACE`

Run
[`devtools::build_readme()`](https://devtools.r-lib.org/reference/build_readme.html)
— locate README.Rmd and build it into a README.md

Check that all exported functions have `@examples` or `@examplesIf`

README examples still work (copy-paste test)

Update vignettes if necessary and render without error:
[`devtools::build_vignettes()`](https://devtools.r-lib.org/reference/build_vignettes.html)

Run
[`devtools::build_site()`](https://devtools.r-lib.org/reference/build_site.html)
locally — build site locally, checking no errors

## 3. Tests

Run `devtools::run_examples(fresh = TRUE)` — all examples pass, no
unexpected errors

Run [`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
— all tests pass, no unexpected skips

[`devtools::test_coverage()`](https://devtools.r-lib.org/reference/test.html)
— no significant test coverage regression

## 4. R CMD CHECK

`devtools::check(cran = TRUE)` — **0 errors, 0 warnings, 0 notes**

Common notes to fix before CRAN: `LazyData` without
`LazyDataCompression`, missing `\value` in `.Rd` files

## 5. Merge & Tag

Push + merge / merge + push: `develop` → `main` (PR or direct)

Create a git tag: `git tag vX.Y.Z && git push origin vX.Y.Z`

Create a GitHub Release with the `NEWS.md` entry as release notes:
`gh release create vX.Y.Z --title "vX.Y.Z" --notes "See NEWS.md for changes" --repo krose/entsoeapi`

## 6. Pkgdown Site

The merge to `main` triggered the `pkgdown.yaml` workflow, verify live
site at <https://krose.github.io/entsoeapi/>

## 7. (When ready) CRAN Submission

Run
[`devtools::install()`](https://devtools.r-lib.org/reference/install.html)
to install package locally

Run
`rhub::rhub_check(gh_url = "https://github.com/krose/entsoeapi", platforms = c("windows", "macos-arm64", "linux"), branch = "main", r_versions = "release")`—
triggers multi-platform check on GitHub

Review [CRAN
policies](https://cran.r-project.org/web/packages/policies.html)

[`devtools::submit_cran()`](https://devtools.r-lib.org/reference/submit_cran.html)
or upload via <https://cran.r-project.org/submit.html>

Reply promptly to CRAN maintainer emails (within 2 weeks)

## 8. Announce

Post on Mastodon with `#rstats` — short example showing new
functionality

Submit to [rweekly.org](https://rweekly.org/submit) (open a PR to their
repo)

Close / reference any related GitHub issues in the release notes
