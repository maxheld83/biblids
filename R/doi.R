# construction ====
#' [Digital Object Identifiers (DOI)](http://doi.org)
#' 
#' S3 record class for DOIs.
#' 
#' @param prefix the naming authority.
#' @param suffix the unique string chosen by the registrant
#' 
#' @example inst/examples/doi/doi.R
#' @examples
#' # DOIs are case insensitive and are compared as such
#' unique(as_doi("10.1000/foo", "10.1000/fOo"))
#' as_doi("10.1000/BAR") == as_doi("10.1000/bar")
#'
#' @export
#' @family doi
doi <- function(prefix = character(), suffix = character()) {
  l <- list(prefix = prefix, suffix = suffix)
  l <- purrr::map(l, vec_cast, to = character())
  l <- rlang::exec(vec_recycle_common, !!!l)
  x <- rlang::exec(new_doi, !!!l)
  validate_doi(x)
}

#' Constructor worker
#' @inheritParams doi
#' @noRd
new_doi <- function(prefix = character(), suffix = character()) {
  vec_assert(prefix, ptype = character())
  vec_assert(suffix, ptype = character())
  new_rcrd(list(prefix = prefix, suffix = suffix), class = "biblids_doi")
}

# validation ====

#' Validator worker 
#' @noRd
validate_doi <- function(x) {
  prefixes_good <- is_doi_syntax(x, "prefix")
  if (!all(prefixes_good)) {
    rlang::abort(
      c(
        "All values must be valid DOI syntax:",
        x = "Bad `prefix` found.",
        i = "Try casting with `as_doi()`."
      )
    )
  }
  suffixes_good <- is_doi_syntax(x, "suffix")
  if (!all(suffixes_good)) {
    rlang::abort(
      c(
        "All values must be valid DOI syntax:",
        x = "Bad `suffix` found.",
        i = "Try casting with `as_doi()`."
      )
    )
  }
  x
}

#' Add delimiters to regex
#' For when fields must *only* include matches, no whitespace etc.
#' @noRd
str_detect_all <- function(string, pattern) {
  stringr::str_detect(string, paste0(r"(^)", pattern, r"($)"))
}

#' Check vector of fields for valid syntax
#' @noRd
is_doi_syntax <- function(x, part = c("prefix", "suffix")) {
  # called part instead of field to aboid name clash with vctrs
  part <- rlang::arg_match(part)
  string <- field(x, part)
  res <- str_detect_all(string = string, pattern = doi_patterns()[part])
  res[is.na(string)] <- TRUE  # an NA field is still valid doi
  res
}

#' @describeIn doi Test for `biblids_doi` class
#' @param x a vector as created by [doi()].
#' @export
#' @examples
#' is_doi(doi_examples())
is_doi <- function(x) inherits(x, "biblids_doi")

#' @describeIn doi Test whether character vector can be cast to DOI
#' @export
#' @example inst/examples/doi/is_doi_ish.R
is_doi_ish <- function(x) !is.na(as_doi(x))

# casting and coercion ====

#' @export
vec_ptype2.biblids_doi.biblids_doi <- function(x, y, ...) new_doi()

#' @export
vec_ptype2.biblids_doi.character <- function(x, y, ...) new_doi()

#' @export
vec_ptype2.character.biblids_doi <- function(x, y, ...) new_doi()

#' @export
vec_cast.biblids_doi.biblids_doi <- function(x, to, ...) x

#' @export
vec_cast.character.biblids_doi <- function(x, to, ...) format(x)

#' @export
vec_cast.biblids_doi.character <- function(x, to, ...) {
  res <- stringr::str_split_fixed(x, pattern = "/", n = 2)
  new_doi(res[, 1], res[, 2])
}

#' @describeIn doi Cast DOIs from other forms
#' @example inst/examples/doi/as_doi.R
#' @examples
#' \dontrun{
#' # there must be only one DOI per element
#' as_doi(c("10.1126/science.169.3946.635 10.6084/m9.figshare.97218"))
#' }
#' @export
as_doi <- function(x, ...) UseMethod("as_doi")

#' @export
as_doi.default <- function(x, ...) vec_cast(x, new_doi())

#' @export
as_doi.character <- function(x, ...) {
  res <- str_extract_all_doi(x)
  if (ncol(res) > 1) {
    rlang::abort(
      c(
        "All elements must include one DOI only:",
        x = "Multiple DOIs found in one or more elements of `x`.",
        i = "Try extracting with `str_extract_all_doi()`."
      )
    )
  }
  res <- stringr::str_split_fixed(string = res[, 1], pattern = "/", n = 2)
  res[which(res == "")] <- NA_character_
  new_doi(prefix = res[, 1], suffix = res[, 2])
}

# presentation methods ====

#' @describeIn doi Display a DOI
#' @param protocol 
#' logical flag, whether to prepend `doi:` handle protocol, as per the official [DOI Handbook](https://doi.org/doi_handbook/2_Numbering.html#2.6.1).
#' @export
format.biblids_doi <- function(x, ..., protocol = FALSE) {
  checkmate::assert_flag(protocol)
  out <- paste0(if (protocol) "doi:", field(x, "prefix"), "/", field(x, "suffix"))
  out[is.na(x)] <- NA_character_
  out
}

#' @export
vec_ptype_abbr.biblids_doi <- function(x, ...) "doi"

#' @export
vec_ptype_full.biblids_doi <- function(x, ...) "digital object identifier"

#' @describeIn doi Print DOIs in [tibble::tibble()]s
#' @exportS3Method pillar::pillar_shaft
#' @method pillar_shaft biblids_doi
#' @examples
#' # there is extra pretty printing inside tibbles
#' tibble::tibble(doi_examples()[1:3])
pillar_shaft.biblids_doi <- function(x, ...) {
  requireNamespace2("pillar")
  out <- format(x, protocol = FALSE)
  out <- stringr::str_replace(out, "/", pillar::style_subtle("/"))
  pillar::new_pillar_shaft_simple(out)
}

#' @describeIn doi Print DOIs in R markdown (when knitr is available):
#' DOIs are hyperlined to the doi.org resolution service.
#' 
#' ```{r}
#' library(knitr)
#' # defaults to crossref style (recommended)
#' doi_examples()[1:3]
#' # or use doi style
#' knitr::knit_print(doi_examples()[1:3], display = "doi")
#' ```
#' 
#' You can also include DOIs inline with `r doi_examples()[1:3]`.
#' @param display character scaling, giving how to display a DOI.
#' Must be one of:
#' - `"crossref"` (recommended) to apply their [display guidelines](https://www.crossref.org/education/metadata/persistent-identifiers/doi-display-guidelines/).
#'    Appears identical to the [DataCite display guidelines](https://support.datacite.org/docs/datacite-doi-display-guidelines).
#' - `"doi"`: to apply the DOI Foundation [presentation guidelines](https://www.doi.org/doi_handbook/2_Numbering.html#2.6).
# TODO @inheritParams inline arg from knitr, blocked by https://github.com/yihui/knitr/issues/1565
#' @param inline 
#' logical flag, giving whether to render DOIs as a chunk output or inline R.
#' Usually set by knitr.
#' @exportS3Method knitr::knit_print
#' @method knit_print biblids_doi
#' @inheritParams knitr::knit_print
knit_print.biblids_doi <- function(x, 
                                  display = getOption("biblids.doi_display", default = "crossref"), 
                                  inline = FALSE,
                                  ...) {
  requireNamespace2("knitr")
  display <- rlang::arg_match(display, values = c("crossref", "doi"))
  link_text <- switch(display,
    "crossref" = paste0("https://doi.org/", format(x)),
    "doi" = format(x, protocol = TRUE)
  )
  with_url <- paste0(
    "[`", link_text, "`]", # text
    "(https://doi.org/", format(x), ")"
  )
  with_url[is.na(x)] <- "`NA`"
  if (inline) {
    requireNamespace2("glue")
    out <- glue::glue_collapse(x = with_url, sep = ", ", last = " and ")
  } else {
    out <- paste0("- ", with_url, "\n")
  }
  knitr::asis_output(out)
}

# other methods ====

#' @describeIn doi Detect if `prefix` and/*or* `suffix` is missing
#' @method is.na biblids_doi
#' @export
#' @examples
#' # this can be constructed but will be NA
#' is.na(doi(prefix = "10.5194", suffix = NA))
is.na.biblids_doi <- function(x, ...) {
  is.na(field(x, "prefix")) | is.na(field(x, "suffix"))
}

#' @export
vec_proxy_equal.biblids_doi <- function(x, ...) {
  data.frame(
    prefix = tolower(field(x, "prefix")),
    suffix = tolower(field(x, "suffix")),
    stringsAsFactors = FALSE
  )
}

# extraction ====

#' Find DOIs with regular expressions
#' 
#' @inheritDotParams stringr::regex
#'
#' @examples
#' regex_doi("doi.org")
#' regex_doi("cr-modern")
#'
#' @family doi
#' @export
regex_doi <- function(type = c("doi.org", "cr-modern"), ...) {
  stringr::regex(pattern = paste0(doi_patterns(type), collapse = "/"))
}

#' @describeIn regex_doi Find DOI fields with regular expressions
#' 
#' @param type
#' a character string giving the type of validation to run.
#' Implemented as regular expressions (see source code).
#' Must be one these syntax specifications:
#' - `"doi.org"` from [doi.org](https://www.doi.org/doi_handbook/2_Numbering.html#2.2), via [stack-overflow](https://stackoverflow.com/questions/27910/finding-a-doi-in-a-document-or-page) (recommended):
#' - `"cr-modern"` from [crossref](https://www.crossref.org/blog/dois-and-matching-regular-expressions/)
#' See examples.
#'
#' @export
doi_patterns <- function(type = c("doi.org", "cr-modern")) {
  checkmate::assert_character(type)
  type <- rlang::arg_match(type)
  res <- list(
    `doi.org` = c(
      prefix = r"(10[.][0-9]{4,}(?:[.][0-9]+)*)",
      suffix = r"((?:(?!["&\'])\S)+)"
    ),
    # comment to repair syntax highlighting '
    `cr-modern` = c(
      prefix = r"(10.\d{4,9})",
      suffix = r"([-._;()/:A-Z0-9]+)"
    ) 
    # comment to repair syntax highlighting "
  )
  res[[type]]
}

#' @describeIn regex_doi Extract *first* DOIs from character strings
#' @inheritParams stringr::str_extract
#' @example inst/examples/doi/str_extract_doi.R
#' @export
str_extract_doi <- function(string) {
  stringr::str_extract(string = string, pattern = regex_doi())
}

#' @describeIn regex_doi Extract *all* DOIs from character strings
#' @inheritParams stringr::str_extract_all
#' @example inst/examples/doi/str_extract_all_doi.R
#' @export
str_extract_all_doi <- function(string, type = "doi.org") {
  stringr::str_extract_all(
    string = string,
    pattern = regex_doi(),
    simplify = TRUE
  )
}


# shiny modules ====

#' Enter DOIs through a Shiny Module
#' 
#' Accept, validate and return DOIs in a shiny app.
#' @family doi
#' @name doiEntry
NULL

#' @describeIn doiEntry Test app
#' @export
doiEntryApp <- function() {
  requireNamespace2("shiny")
  ui <- shiny::fluidPage(doiEntryUI(id = "test"))
  server <- function(input, output, session) {
    doiEntryServer(id = "test")
  }
  shiny::shinyApp(ui, server)
}

#' @describeIn doiEntry Module UI
#' @inheritParams shiny::NS
#' @inheritParams shiny::textAreaInput
#' @inheritDotParams shiny::textAreaInput
#' @export
doiEntryUI <- function(id, width = "100%", ...) {
  requireNamespace2("shiny")
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::textAreaInput(
      inputId = ns("entered"),
      label = "DOIs",
      placeholder = "Enter your DOIs here.",
      width = width,
      rows = 15L
    ),
    shiny::actionButton(
      inputId = ns("validate"),
      label = "Validate your DOIs",
      width = width
    ),
    shiny::div(
      shiny::p(
        "Found ",
        shiny::textOutput(
         outputId = ns("found"),
         inline = TRUE
        ),
        " DOIS."
      )
    )
  )
}

#' @describeIn doiEntry Module server
#' @export
doiEntryServer <- function(id) {
  requireNamespace2("shiny")
  shiny::moduleServer(
    id,
    module = function(input, output, session) {
      # input validation
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("entered", shinyvalidate::sv_required())
      iv$add_rule("entered", ~ if (nchar(.) > 10000L) "Too many DOIs.")
      iv$enable()

      # ingest
      dois <- shiny::eventReactive(input$validate, {
        if (iv$is_valid()) {
          unique(tolower(as.vector(str_extract_all_doi(input$entered))))
        } else {
          shiny::showNotification(
            "Please fix the errors in the form before continuing",
            type = "warning"
          )
          NULL
        }
      })

      output$found <- shiny::renderText({
        length(dois())
      })
      dois
    }
  )
}

is_doi_on_cr <- function(x) {
  res <- httr::HEAD(
    url = paste0("https://api.crossref.org/works/", x)
  )
  if (res$status_code == 200) {
    return(TRUE)
  } else if (res$status_code == 404) {
    return(FALSE)
  } else {
    httr::stop_for_status(res)
  }
}

#' Example DOIs
#' @export
#' @family doi
#' @examples
#' doi_examples()
doi_examples <- function() {
  ex_files <- c(
    "doi.R",
    "as_doi.R"
  )
  res <- purrr::map(ex_files, function(x) {
    source(path_ex_file("doi", x))$value
  })
  purrr::reduce(res, c)
}
