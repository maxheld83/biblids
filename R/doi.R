#' Validate a DOI
#'
#' Validates a [Digital Object Identifier (DOI)](http://doi.org)
#'
#' To resolve DOIs to the associated metadata, you can use the [rcrossref](https://www.crossref.org/blog/dois-and-matching-regular-expressions/) R package.
#'
#' @param x a character string giving a DOI
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
#' @examples
#' is_doi("10.5281/zenodo.3892950") # TRUE
#' is_doi("http://doi.org/10.5281/zenodo.3892951") # TRUE
#' is_doi("lorem ipsum") # FALSE
#' @family doi
#'
#' @export
is_doi <- function(x, type = c("cr-modern", "cr-jws", "regexpal")) {
  checkmate::assert_string(x)
  checkmate::assert_character(type)
  type <- rlang::arg_match(type)

  pattern <- switch(
    type,
    "cr-modern" = r"(^10.\d{4,9}/[-._;()/:A-Z0-9]+$)",
    "cr-jws" = r"(^10.1002/[^\s]+$)",
    "regexpal" = r"(10[.][0-9]{4,}(?:[.][0-9]+)"
  )

  grepl(pattern = pattern, x = x, ignore.case = TRUE)
}


# shiny modules ====

#' Shiny Module for DOI input
#' 
#' Accept, validate and return DOIs in a shiny app.
#' @inheritParams shiny::NS
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
#' @export
doiEntryUI <- function(id) {
  requireNamespace2("shiny")
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::textAreaInput(
      inputId = ns("entered"),
      label = "DOIs",
      placeholder = "10.5281/zenodo.3892950"
    ),
    shiny::actionButton(inputId = ns("submit"), label = "Submit DOIs"),
    shiny::verbatimTextOutput(
      outputId = ns("found"),
      placeholder = TRUE
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
      dois <- shiny::eventReactive(input$submit, {
        input$entered
      })
      output$found <- shiny::renderText({
        dois()
      })
    }
  )
}
