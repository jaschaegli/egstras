# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`egstras` is a personal R utility package for the DDS 21 research project (survey data work: SPSS-labelled vectors, recoding, item scoring, IRT). Hosted on GitHub at `github.com/jaschaegli/egstras` (formerly `gitlab.uzh.ch/jaschamax.egli/egstras`, now archived); installed via `devtools::install_github("jaschaegli/egstras")`. All dependencies are on CRAN.

## Commands

Run from the package root:

```sh
Rscript -e 'devtools::document()'                     # regenerate man/*.Rd and NAMESPACE from roxygen
Rscript -e 'devtools::test()'                         # run all testthat tests
Rscript -e 'devtools::test(filter = "identifier")'    # run one test file (tests/testthat/test-identifier.R)
Rscript -e 'devtools::check()'                        # full R CMD check
Rscript -e 'devtools::load_all()'                     # load for interactive use
```

New top-level non-package files must be added to `.Rbuildignore`, or `devtools::check()` will flag them.

## Conventions

- **NAMESPACE and `man/` are generated** by roxygen2 (markdown enabled). Never hand-edit them; edit roxygen comments and run `devtools::document()`. Package-level imports live in the `usethis namespace` block in [R/egstras-package.R](R/egstras-package.R) — add new `@importFrom` lines there and add the package to `Imports` in `DESCRIPTION`.
- Despite imports, code calls functions with explicit `pkg::fn()` prefixes; keep doing so.
- Input validation uses `checkmate::qassert()` rule strings (e.g. `"s+"`, `"B1"`, `"d"`, `c("x", "0")`) at the top of exported functions, usually with a trailing comment explaining the rule.
- Many exports have aliases via `@rdname` in two naming styles: `getvl`/`vl_get`, `setvl`/`vl_set`, `modvl`/`vl_mod`, `getcl`/`cl_get`, `setcl`/`cl_set`, `add_backticks`/`backticks_add`, etc. When adding/renaming such a function, keep both forms.
- Native pipe `|>` with `_` placeholder is used throughout (hence `R (>= 4.2)` in `DESCRIPTION`). `pb()` in [R/pipes.R](R/pipes.R) wraps `pipebind::bind` for magrittr-style `.`.
- Commit messages are short and lowercase, prefixed with the file/function area (e.g. `irtheta: fix the coersion logic`); version bumps in `DESCRIPTION` and doc regeneration are committed separately ("bump version", "bump namespace", "bump rd").

## Architecture notes

- **Labelled-vector model**: the package works on haven/SPSS-style vectors where value labels are the `"labels"` attribute and the variable label is the `"label"` attribute. [R/labels.R](R/labels.R) provides the get/set/mod helpers; other functions (e.g. `flip_integer` in [R/flippe.R](R/flippe.R)) use them to preserve labels across transformations. `score_items` outputs `haven::labelled` or `haven::labelled_spss` (when user-missing codes are given); `fit_irt_theta` applies `haven::zap_missing()` before fitting.
- **Metaprogrammed `fcase`**: `classify` and `flip_integer` build `data.table::fcase()` calls by glueing condition/value strings, parsing them with `rlang::parse_exprs`, splicing with `!!!`, and `eval()`-ing. Regex patterns in `classify` are interpolated into code strings, so escaping matters (see backtick helpers in [R/strings.R](R/strings.R)).
- **Tidy-select interfaces**: `score_items` and `fit_irt_theta` take `cols` via `{{ }}` and resolve names with `dplyr::select(data, {{ cols }})` before iterating.
- `explicitise` rewrites `.R` files in the current project in place, adding `pkg::` prefixes. Dependencies come from `renv::dependencies`; calls are located with `utils::getParseData` (so comments/strings are untouched) and prefixes inserted by position. Parser columns expand tabs to multiples of 8, hence `expl_col_to_index`. It refuses to rewrite files git couldn't restore unless `force = TRUE`.
- Test coverage is minimal (`make_ids` and `explicitise`); `tests/testthat.R` also attaches `checkmate`.
