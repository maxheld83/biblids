#' [Digital Object Identifiers (DOI)](http://doi.org)
#' 
#' Helper function to create DOIs.
#' 
#' @param prefix denoting a unique naming authority
#' @param suffix unique string chosen by the registrant
#' @export
#' @family doi
doi <- function(prefix, suffix) {
  new_doi(prefix, suffix)
}

new_doi <- function(prefix = character(), suffix = character()) {
  vctrs::vec_assert(prefix, ptype = character())
  vctrs::vec_assert(suffix, ptype = character())
  vctrs::new_rcrd(list(prefix = prefix, suffix = suffix), class = "biblids_doi")
}

#' @export
format.biblids_doi <- function(x, ...) {
  p <- vctrs::field(x, "prefix")
  s <- vctrs::field(x, "suffix")
  out <- paste0(p, "/", s)
  out[is.na(p) | is.na(s)] <- NA
  out
}

#' @export
#' @importFrom vctrs vec_ptype_abbr
vec_ptype_abbr.biblids_doi <- function(x, ...) "doi"

#' @export
#' @importFrom vctrs vec_ptype_full
vec_ptype_full.biblids_doi <- function(x, ...) "digital object identifier"


#' @describeIn doi Choose a DOI validation pattern
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

#' @describeIn doi Validate a DOI
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

#' @describeIn doi Extract all DOIs from a string
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
