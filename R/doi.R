#' [Digital Object Identifiers (DOI)](http://doi.org)
#' 
#' To resolve DOIs to the associated metadata, you can use the [rcrossref](https://www.crossref.org/blog/dois-and-matching-regular-expressions/) R package.
#' @family doi
#' @name doi
NULL

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
      rows = 10L
    ),
    shiny::actionButton(
      inputId = ns("validate"),
      label = "Validate your DOIs",
      width = width
    ),
    shiny::div(
      shiny::p("Found these DOIs:")
    ),
    shiny::textOutput(
      outputId = ns("found")
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
      iv$add_rule("entered", ~ if (nchar(.) > 1000) "Too many DOIs.")
      iv$enable()

      # ingestion
      dois <- shiny::eventReactive(input$validate, {
        unique(tolower(as.vector(str_extract_all_doi(input$entered))))
      })
      output$found <- shiny::renderText({
        dois()
      })
      dois
    }
  )
}
