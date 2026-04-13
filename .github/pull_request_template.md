## Description

<!-- What does this PR change and why? Link any related issues below. -->

Fixes #<!-- issue number, if applicable -->

## Type of change

- [ ] Bug fix
- [ ] New endpoint / feature
- [ ] Refactor / cleanup
- [ ] Documentation only

## Affected module(s)

<!-- Tick all that apply -->
- [ ] `en_market`
- [ ] `en_load`
- [ ] `en_generation`
- [ ] `en_transmission`
- [ ] `en_outages`
- [ ] `en_balancing`
- [ ] `en_helpers` / `utils`
- [ ] Tests / CI
- [ ] Documentation / vignettes

## Checklist

- [ ] Code follows project conventions (snake_case, `cli::` messages, `checkmate::` validation)
- [ ] New exported functions include `@return` and `@examplesIf` roxygen tags
- [ ] Tests added / updated — `covr::package_coverage()` passes at 100 %
- [ ] `lintr::lint_package()` passes with no new warnings
- [ ] `devtools::document()` run — man pages up-to-date
- [ ] `devtools::check()` passes locally (0 errors, 0 warnings, 0 notes)
- [ ] No `ENTSOE_PAT` tokens or secrets committed
- [ ] `NEWS.md` updated (if user-facing change)
