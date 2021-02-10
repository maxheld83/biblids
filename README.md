
# biblids

<!-- badges: start -->
[![Main](https://github.com/subugoe/biblids/workflows/.github/workflows/main.yaml/badge.svg)](https://github.com/subugoe/biblids/actions)
[![Codecov test coverage](https://codecov.io/gh/subugoe/biblids/branch/master/graph/badge.svg)](https://codecov.io/gh/subugoe/biblids?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/biblids)](https://CRAN.R-project.org/package=biblids)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

In bibliometric analysis, we frequently work with specialised identifiers, such as [Digital Object Identifiers (DOIs)](https://www.doi.org) or [Research Organisation Registry IDs (RORs)](https://ror.org).
The goal of biblids is to help you work with these identifiers.

Biblids *does not include, nor query the metadata associated with the bibliometric IDs*.
For example, you can check whether a DOI is syntactically valid, but to actually resolve a DOI to some metadata, you must use other software (such as the [rcrossref](https://github.com/ropensci/rcrossref) package).
Where available, the documentation for biblids links to related software.

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

- **Presentation methods** to properly display bibliographic IDs inside [knitr](https://yihui.org/knitr/)/[rmarkdown](https://rmarkdown.rstudio.com) and [tibble](http://tibble.tidyverse.org).
- **[Shiny](https://shiny.rstudio.com) input modules** to ingest and validate IDs.
