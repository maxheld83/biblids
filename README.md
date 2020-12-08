
# biblids

<!-- badges: start -->
[![Main](https://github.com/subugoe/biblids/workflows/.github/workflows/main.yaml/badge.svg)](https://github.com/subugoe/biblids/actions)
[![Codecov test coverage](https://codecov.io/gh/subugoe/biblids/branch/master/graph/badge.svg)](https://codecov.io/gh/subugoe/biblids?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/biblids)](https://CRAN.R-project.org/package=biblids)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

In bibliometric analysis, we frequently work with specialised identifiers, such as [Digital Object Identifiers (DOIs)](https://www.doi.org) or [Research Organisation Registry IDs (RORs)](https://ror.org).
The goal of biblids is to validate and (where applicable) normalise selected bibliometric identifiers.

For example, you can validate a DOI by:

```r
library(biblids)
is_doi("10.5281/zenodo.3892950") # TRUE
is_doi("lorem ipsum") # FALSE
```

Such helpers are useful for argument checking (in functions) or assertions (in scripts).

For large sets of identifiers (such as the DOIs), only the syntax is validated.
For smaller sets (such as RORs), the package ships with datasets including presently valid identifiers, or wrappers to pull such information from external sources.

Biblids *does not include, nor query the metadata associated with the bibliometric IDs*.
For example, you can check whether a DOI is syntactically valid, but to actually resolve a DOI to some metadata, you must use other software (such as the [rcrossref](https://github.com/ropensci/rcrossref) package).
Where available, the documentation for biblids links to related software.

## Optional Extensions

The `is_x()`-style predicate functions are designed with minimal dependencies.

Additional wrappers are available when the `Suggests:` packages are installed (`remotes::install_deps(dependencies = TRUE)`):

- Argument checks (`) via [checkmate](https://mllg.github.io/checkmate/)
- Expectations for testing via [testthat](http://testthat.r-lib.org)
- Input modules for [shiny](https://shiny.rstudio.com)
