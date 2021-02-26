
# biblids

<!-- badges: start -->
[![Main](https://github.com/subugoe/biblids/workflows/.github/workflows/main.yaml/badge.svg)](https://github.com/subugoe/biblids/actions)
[![Codecov test coverage](https://codecov.io/gh/subugoe/biblids/branch/master/graph/badge.svg)](https://codecov.io/gh/subugoe/biblids?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/biblids)](https://CRAN.R-project.org/package=biblids)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

In bibliometric analysis, we frequently work with specialised identifiers, such as [Digital Object Identifiers (DOIs)](https://www.doi.org) or [Research Organisation Registry IDs (RORs)](https://ror.org).
The goal of biblids is to help you work with these identifiers.
## Vctrs S3 Classes

Biblids implements bibliometric identifiers as S3 classes based on the [vctrs](https://vctrs.r-lib.org) package.
Each bibliometric identifier comes a family of functions:

- A **constructor** function to help you create valid IDs, such as `doi()` for the `biblids_doi` class.
- **Casting** methods to help you **normalise** IDs, such as `as_doi()`.
- **Extraction** functions to extract valid IDs from arbitrary text such as `str_extract_all_doi()` as well as the underlying **regular expressions** `regex_doi()`.
- **Predicate** functions to test whether some object is (`is_doi()`) or could be (`is_doi_ish()`) a bibliometric ID.
- **Some examples** to play with, such as `doi_examples()`.
- **Other methods** to fully support bibliometrics in R, including for presentation (`format()`), logic (`is.na()`) or arithmetic.

For large sets of identifiers (such as the DOIs), only the syntax is validated.
For smaller sets (such as RORs), the package ships with datasets including presently valid identifiers, or wrappers to pull such information from external sources.
## Optional Extensions

Biblids is designed to be lightweight and comes with minimal `Imports` dependencies.
There additional features included for some `Suggests` packages:
### Presentation Methods

Bibliographic IDs are displayed appropriately in various output formats, such as inside [knitr](https://yihui.org/knitr/)/[rmarkdown](https://rmarkdown.rstudio.com) documents and [tibble](http://tibble.tidyverse.org) tables.
For example, `knit_print.biblids_doi()` will automatically render DOIs appropriately.
### Shiny Input Modules

You can rely on ready-made, well-tested [shiny](https://shiny.rstudio.com) input modules to ingest and validate bibliographic identifiers.
This may be helpful inside a bibliographic dashboard or another shiny app, where users can supply their own data for an analysis.

For example, you can ingest DOIs using `doiEntryApp()`.
### Minimal API Clients

Biblids also includes some API clients to resolve bibliomatric IDs and query related databases.

The clients are minimal API clients in the spirit of [gh](https://github.com/r-lib/gh).
They do *not* comprehensively replicate and document external APIs.

These wrappers are limited:
- They only cover APIs which do not already have an R client (such as [rcrossref](https://github.com/ropensci/rcrossref)).
- They only cover relatively simple APIs, where the output can be transformed into a straightforward R object.
- They provide thin wrappers around [httr](https://httr.r-lib.org) for you to write your queries (you're on your own).
- They provide some ready-made queries which yield very simple R objects.

For example, you can
- Test whether a doi has been published with `is_doi_found()`, or retrieve the resolved URL `resolve_doi()`.
- Or you can "roll your own" query with `get_doi_handles()`.

It can be easy to confuse these clients with more comprehensive wrappers geared towards particular services such as [rcrossref](https://github.com/ropensci/rcrossref) because they sometimes accept the same inputs (DOIs!).
Check the documentation for details.
