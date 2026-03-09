# Release Checklist — entsoeapi

## 1. Prepare

-   [ ] All planned changes merged into `develop`
-   [ ] `NEWS.md` updated — new version header, all changes documented
-   [ ] Version bumped in `DESCRIPTION` (follow semver: major.minor.patch)
-   [ ] `RoxygenNote` in `DESCRIPTION` matches installed roxygen2 version
-   [ ] Run `lintr::lint_package()` lint the scripts following general formatting rules

## 2. Documentation

-   [ ] Run `devtools::document()` — regenerate all `.Rd` files and `NAMESPACE`
-   [ ] Run `devtools::build_readme()` — locate README.Rmd and build it into a README.md
-   [ ] Check that all exported functions have `@examples` or `@examplesIf`
-   [ ] README examples still work (copy-paste test)
-   [ ] Vignettes render without error: `devtools::build_vignettes()`

## 3. Tests

-   [ ] `devtools::test()` — all tests pass, no unexpected skips
-   [ ] `covr::package_coverage()` — no significant coverage regression

## 4. R CMD CHECK

-   [ ] `devtools::check()` — **0 errors, 0 warnings, 0 notes**
    -   Common notes to fix before CRAN: `LazyData` without `LazyDataCompression`, missing `\value` in `.Rd` files
-   [ ] `urlchecker::url_check()` — no broken URLs in docs

## 5. Merge & Tag

-   [ ] Merge `develop` → `main` (PR or direct)
-   [ ] Create a git tag: `git tag v0.9.5.1 && git push origin v0.9.5.1`
-   [ ] Create a GitHub Release with the `NEWS.md` entry as release notes

## 6. Pkgdown Site

-   [ ] `pkgdown::build_site()` locally — no errors
-   [ ] Push to `main` to trigger the `pkgdown.yaml` workflow
-   [ ] Verify live site at <https://krose.github.io/entsoeapi/>

## 7. (When ready) CRAN Submission

-   [ ] `rhub::rhub_doctor(gh_url = "https://github.com/krose/entsoeapi")` — multi-platform check
-   [ ] `devtools::spell_check()` — no spelling errors in docs
-   [ ] Review [CRAN policies](https://cran.r-project.org/web/packages/policies.html)
-   [ ] `devtools::submit_cran()` or upload via <https://cran.r-project.org/submit.html>
-   [ ] Reply promptly to CRAN maintainer emails (within 2 weeks)

## 8. Announce

-   [ ] Post on Mastodon with `#rstats` — short example showing new functionality
-   [ ] Submit to [rweekly.org](https://rweekly.org/submit) (open a PR to their repo)
-   [ ] Close / reference any related GitHub issues in the release notes
