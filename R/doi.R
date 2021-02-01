#' [Digital Object Identifiers (DOI)](http://doi.org)
#' 
#' S3 record class for DOIs.
#' 
#' @param prefix the naming authority.
#' @param suffix the unique string chosen by the registrant
#' 
#' @example R/doi_examples.R
#'
#' @export
#' @family doi
doi <- function(prefix = character(), suffix = character()) {
  x <- new_doi(prefix, suffix)
  validate_doi(x)
}

#' Constructor worker
#' @inheritParams doi
#' @noRd
new_doi <- function(prefix = character(), suffix = character()) {
  vctrs::vec_assert(prefix, ptype = character())
  vctrs::vec_assert(suffix, ptype = character())
  vctrs::new_rcrd(list(prefix = prefix, suffix = suffix), class = "biblids_doi")
}

#' Validator worker
#' @noRd
validate_doi <- function(x) {
  x
}

#' @describeIn doi convert to character vector
#' @param protocol 
#' logical flag, whether to prepend `doi:` handle protocol, as per the official [DOI Handbook](https://doi.org/doi_handbook/2_Numbering.html#2.6.1).
#' Recommended unless protocol is already obvious from the context, such as in a column header.
#' @param subtle_slash
#' logical flag, whether to use [pillar::style_subtle] slash between prefix and suffix.
#' Only for internal use, inside tibble printing.
#' @export
as.character.biblids_doi <- function(x, ..., protocol = TRUE, subtle_slash = FALSE) {
  checkmate::assert_flag(protocol)
  p <- vctrs::field(x, "prefix")
  s <- vctrs::field(x, "suffix")
  complete <- !is.na(p) & !is.na(s)
  out <- rep(NA_character_, vctrs::vec_size(x))
  out[complete] <- paste0(
    if (protocol) "doi:",
    p[complete],
    ifelse(subtle_slash, pillar::style_subtle("/"), "/"),
    s[complete]
  )
  out
}

#' @export
#' @importFrom vctrs vec_ptype_abbr
vec_ptype_abbr.biblids_doi <- function(x, ...) "doi"

#' @export
#' @importFrom vctrs vec_ptype_full
vec_ptype_full.biblids_doi <- function(x, ...) "digital object identifier"

#' @describeIn doi pretty printing in the R console.
#' @export
#' @examples 
#' # you can print bare dois
#' doi_examples
format.biblids_doi <- function(x, ...) {
  # vctrs print method always shows class so protocol is unecessary
  format(as.character.biblids_doi(x, protocol = FALSE))
}

#' @describeIn doi pretty printing in [tibble::tibble()]
#' @exportS3Method pillar::pillar_shaft
#' @method pillar_shaft biblids_doi
#' @examples
#' # there is extra pretty printing inside tibbles
#' tibble::tibble(doi_examples)
pillar_shaft.biblids_doi <- function(x, ...) {
  out <- as.character.biblids_doi(x, protocol = FALSE, subtle_slash = TRUE)
  pillar::new_pillar_shaft_simple(out)
}

#' @describeIn doi pretty printing in R markdown (when knitr is available);
#' DOIs are printed with protocoll and a link to their doi.org resolution.
#' 
#' ```{r}
#' library(knitr)
#' # defaults to crossref style (recommended)
#' doi_examples
#' # or use doi style
#' knitr::knit_print(doi_examples, display = "doi")
#' ```
#' 
#' You can also include DOIs inline with `r doi_examples`.
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
  display <- rlang::arg_match(
    display, 
    values = c("crossref", "doi")
  )
  link_text <- switch(display,
    "crossref" = paste0("https://doi.org/", as.character(x, protocol = FALSE)),
    "doi" = as.character(x, protocol = TRUE)
  )
  with_url <- paste0(
    "[`", link_text, "`]", # text
    "(https://doi.org/", as.character.biblids_doi(x, protocol = FALSE), ")"
  )
  if (inline) {
    requireNamespace2("glue")
    out <- glue::glue_collapse(x = with_url, sep = ", ", last = " and ")
  } else {
    out <- paste0("- ", with_url, "\n")
  }
  knitr::asis_output(out)
}

#' Choose a DOI validation pattern
#' 
#' @param type
#' a character string giving the type of validation to run.
#' Implemented as regular expressions (see source code).
#' Must be one these syntax specifications:
#' - from [crossref](https://www.crossref.org/blog/dois-and-matching-regular-expressions/)
#'   - `"cr-modern"` currently used crossref DOIs.
#'   - `"cr-jws"` for DOIs created by John Wiley & Sons
#' - `"regexpal"` from [regexpal](https://www.regexpal.com/96948) (undocumented, not recommended)
#' 
#' @return a raw string with a regular expression
#' 
#' @family doi
#' 
#' @export
doi_patterns <- function(type = c("cr-modern", "cr-jws", "regexpal")) {
  checkmate::assert_character(type)
  type <- rlang::arg_match(type)
  switch(
    type,
    "cr-modern" = r"(10.\d{4,9}/[-._;()/:A-Z0-9]+)",
    "cr-jws" = r"(10.1002/[^\s]+)",
    "regexpal" = r"(10[.][0-9]{4,}(?:[.][0-9]+)"
  )
}

#' @describeIn doi_patterns Validate a DOI
#'
#' @param x a character string giving a DOI
#' 
#' @inheritDotParams doi_patterns
#'
#' @examples
#' is_doi("10.5281/zenodo.3892950") # TRUE
#' is_doi("http://doi.org/10.5281/zenodo.3892951") # TRUE
#' is_doi("lorem ipsum") # FALSE
#'
#' @export
is_doi <- function(x, ...) {
  checkmate::assert_string(x)
  grepl(pattern = doi_patterns(...), x = x, ignore.case = TRUE)
}

#' @describeIn doi_patterns Extract all DOIs from a string
#' @inheritParams stringr::str_extract_all
#' @inheritDotParams doi_patterns
str_extract_all_doi <- function(string, ...) {
  requireNamespace2("stringr")
  stringr::str_extract_all(
    string = string,
    pattern = stringr::regex(doi_patterns(...), ignore_case = TRUE),
    simplify = TRUE
  )
}


# shiny modules ====

#' Shiny Module for DOI input
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

#' Some example dois
#' @export
#' @family doi
#' @examples
#' doi_examples
#' @name doi_examples
NULL
