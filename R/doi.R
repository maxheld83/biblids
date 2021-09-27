# construction ====
#' Digital Object Identifiers
#'
#' S3 record class for DOIs.
#'
#' @param prefix The naming authority.
#' @param suffix The unique string chosen by the registrant.
#'
#' @example inst/examples/doi/doi.R
#' @examples
#' # DOIs are case insensitive and are compared as such
#' unique(as_doi(c("10.1000/foo", "10.1000/fOo")))
#' as_doi("10.1000/BAR") == as_doi("10.1000/bar")
#'
#' # convert back to a (normalised) character
#' as.character(as_doi("10.1000/zap"))
#'
#' @note
#' DOIs are returned as an S3 record class constructed by [vctrs::new_rcrd()].
#' Under the hood, these records are implemented as *lists* of fields
#' (here: prefix, suffix).
#' Support for such records may still be limited.
#' For example, [purrr::map()] will erroneously loop over the fields,
#' instead of over the DOIs (see [#51](https://github.com/subugoe/biblids/issues/51)).
#' To avoid such problems,
#' cast the DOI to a simple character vector using [as.character()].
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
  prefixes_bad <- c(1L:length(x))[!is_doi_syntax(x, "prefix")]
  suffixes_bad <- c(1L:length(x))[!is_doi_syntax(x, "suffix")]
  if (length(c(prefixes_bad, suffixes_bad)) > 0) {
    stop_doi_syntax(prefixes_bad = prefixes_bad, suffixes_bad = suffixes_bad)
  }
  x
}

#' Throw error on bad DOI syntax
#' @noRd
stop_doi_syntax <- function(prefixes_bad = integer(0), suffixes_bad = integer(0)) {
  rlang::abort(
    class = "biblids_error_doi_syntax",
    prefixes_bad = prefixes_bad,
    suffixes_bad = suffixes_bad
  )
}

#' Write error message for bad DOI syntax
#' @noRd
#' @export
conditionMessage.biblids_error_doi_syntax <- function(c) {
  rlang::format_error_bullets(c(
    "All values must be valid DOI syntax.",
    if (length(c$prefixes_bad) > 0) {
      c(x = list_bad_parts(part = "prefix", bad_pos = c$prefixes_bad))
    },
    if (length(c$suffixes_bad) > 0) {
      c(x = list_bad_parts(part = "suffix", bad_pos = c$suffixes_bad))
    },
    i = "Try casting with `as_doi()`."
  ))
}

#' Helper to write error message for bad DOI syntax
#' @param part The part of the DOI that is invalid.
#' @param bad_pos An integer vector giving the offending positions.
#' @noRd
list_bad_parts <- function(part = c("prefix", "suffix"), bad_pos) {
  part <- rlang::arg_match(part)
  vec_assert(bad_pos, ptype = integer())
  glue::glue(
    "Found {length(bad_pos)} bad `{part}`(es) at position(s) ",
    glue::glue_collapse(bad_pos, sep = ", ", width = 10, last = " and ")
  )
}

#' Add delimiters to regex
#'
#' For when fields must *only* include matches, no whitespace etc.
#' @noRd
str_detect_all <- function(string, pattern) {
  stringr::str_detect(string, paste0(r"(^)", pattern, r"($)"))
}

#' Check vector of fields for valid syntax
#' @noRd
is_doi_syntax <- function(x, part = c("prefix", "suffix")) {
  # called part instead of field to avoid name clash with vctrs
  part <- rlang::arg_match(part)
  string <- field(x, part)
  res <- str_detect_all(string = string, pattern = doi_patterns()[part])
  res[is.na(string)] <- TRUE  # an NA field is still valid doi
  res
}

#' @describeIn doi Is this a `biblids_doi`?
#' @export
#' @examples
#' is_doi(as_doi("10.1000/1"))
#' is_doi(1L)
is_doi <- function(x) inherits(x, "biblids_doi")

#' @describeIn doi
#' Could this be converted to a `biblids_doi` using [as_doi()]?
#' @param x A vector created by, or convertable to [doi()].
#' @export
is_doi_ish <- function(x) {
  res <- rlang::catch_cnd(as_doi(x), classes = "error")
  !any(class(res) == "error")
}
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
  res <- str_extract_all_doi(x)
  if (ncol(res) > 1) stop_doi_multiple()
  res <- stringr::str_split_fixed(string = res[, 1], pattern = "/", n = 2)
  res[which(res == "")] <- NA_character_
  new_doi(prefix = res[, 1], suffix = res[, 2])
  # no extra validation necessary because above extraction is already doi only
}

#' Throw error on multiple DOIs
#' @noRd
stop_doi_multiple <- function() {
  rlang::abort(class = "biblids_error_doi_multiple")
}

#' Write error message for multiple DOIs
#' @noRd
#' @export
conditionMessage.biblids_error_doi_multiple <- function(c) {
  rlang::format_error_bullets(c(
    "All elements must include one DOI only:",
    x = "Multiple DOIs found in one or more elements of `x`.",
    i = "Try extracting with `str_extract_all_doi()`."
  ))
}

#' @describeIn doi Normalise
#' @inheritParams is_doi
#' @example inst/examples/doi/as_doi.R
#' @examples
#' \dontrun{
#' # there must be only one DOI per element
#' as_doi(c("10.1126/science.169.3946.635 10.6084/m9.figshare.97218"))
#' }
#' @export
as_doi <- function(x, ...) {
  ellipsis::check_dots_empty()
  UseMethod("as_doi")
}

#' @export
as_doi.default <- function(x, ...) vec_cast(x, new_doi())

# presentation methods ====

#' @describeIn doi Display a DOI
#' @param protocol
#' Logical flag, whether to prepend `doi:` handle protocol,
#' as per the official
#' [DOI Handbook](https://doi.org/doi_handbook/2_Numbering.html#2.6.1).
#' @export
format.biblids_doi <- function(x, ..., protocol = FALSE) {
  stopifnot(rlang::is_scalar_logical(protocol))
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
#' tibble::tibble(c(doi_examples(na.rm = FALSE)[1:3]))
pillar_shaft.biblids_doi <- function(x, ...) {
  require_namespace2("pillar")
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
#' doi_examples(na.rm = FALSE)[1:3]
#' # or use doi style
#' knitr::knit_print(doi_examples(na.rm = FALSE)[1:3], display = "doi")
#' ```
#' 
#' You can also include DOIs inline with `r doi_examples()[1:3]`.
#' @param display character scaling, giving how to display a DOI.
#' Must be one of:
#' - `"crossref"` (recommended)
#'     to apply their
#'     [display guidelines](https://www.crossref.org/education/metadata/persistent-identifiers/doi-display-guidelines/).
#'     Appears identical to the
#'     [DataCite display guidelines](https://support.datacite.org/docs/datacite-doi-display-guidelines).
#' - `"doi"`:
#'     to apply the DOI Foundation
#'     [presentation guidelines](https://www.doi.org/doi_handbook/2_Numbering.html#2.6).
# TODO @inheritParams inline arg from knitr, blocked by https://github.com/yihui/knitr/issues/1565
#' @param inline 
#' Logical flag, giving whether to render DOIs as a chunk output or inline R.
#' Usually set by knitr.
#' @exportS3Method knitr::knit_print
#' @method knit_print biblids_doi
#' @inheritParams knitr::knit_print
knit_print.biblids_doi <- function(x, 
                                  display = getOption(
                                    "biblids.doi_display", 
                                    default = "crossref"
                                  ), 
                                  inline = FALSE,
                                  ...) {
  require_namespace2("knitr")
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
    require_namespace2("glue")
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
#' is.na(doi(prefix = "10.1000", suffix = NA))
is.na.biblids_doi <- function(x, ...) {
  # there appears to be nothing to reuse here from vctrs
  # see discussion https://github.com/subugoe/biblids/issues/49
  is.na(field(x, "prefix")) | is.na(field(x, "suffix"))
}

# these may eventually be replaced by defaults in vctrs
# see https://github.com/subugoe/biblids/issues/79

#' @method na.fail biblids_doi
#' @exportS3Method stats::na.fail
na.fail.biblids_doi <- function(object, ...) stats::na.fail(vec_proxy(object))

#' @method na.omit biblids_doi
#' @exportS3Method stats::na.omit
na.omit.biblids_doi <- function(object, ...) {
  df <- stats::na.omit(vec_proxy(object))
  out <- vec_restore(x = df, to = object, ...)
  attr(out, "na.action") <- stats::na.action(df)
  out
}

#' @method na.exclude biblids_doi
#' @exportS3Method stats::na.exclude
na.exclude.biblids_doi <- function(object, ...) {
  df <- stats::na.exclude(vec_proxy(object))
  out <- vec_restore(x = df, to = object, ...)
  attr(out, "na.action") <- stats::na.action(df)
  out
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
#' - `"doi.org"` (recommended) from
#'     [doi.org](https://www.doi.org/doi_handbook/2_Numbering.html#2.2),
#'     via [stack-overflow](https://stackoverflow.com/questions/27910/finding-a-doi-in-a-document-or-page)
#'     uses the actual spec, but can cause problems when DOIs are not separated
#'     by whitespace or linebreaks, because many other characters
#'     are valid DOI and will extracted.
#' - `"cr-modern"` from
#'     [crossref](https://www.crossref.org/blog/dois-and-matching-regular-expressions/)
#'     is less vulnerable to over-extracting, but excludes some DOIs which,
#'     while today uncommon are syntactically valid.
#' See examples.
#'
#' @export
doi_patterns <- function(type = c("doi.org", "cr-modern")) {
  type <- rlang::arg_match(type)
  res <- list(
    `doi.org` = c(
      prefix = r"(10[.][0-9]+(?:[.][0-9]+)*)",
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
  stringi::stri_extract_all(
    str = string,
    regex = regex_doi(),
    simplify = NA
  )
}


# shiny modules ====

#' Enter DOIs through a Shiny Module
#' 
#' Input, validate and return DOIs in a shiny app.
#' @family doi
#' @name doiEntry
NULL

#' @describeIn doiEntry Test app
#' @export
doiEntryApp <- function() {
  require_namespace2("shiny")
  require_namespace2("shiny.i18n")
  app_translator <- app_translator()
  ui <- shiny::fluidPage(
    shiny.i18n::usei18n(app_translator),
    shiny::selectInput(
      inputId = "lang",
      label = app_translator$t("Language"),
      choices = app_translator$get_languages(),
      selected = "en"
    ),
    doiEntryUI(id = "test", translator = doi_entry_translator())
  )
  server <- function(input, output, session) {
    # update lang client side
    lang <- shiny::reactive(input$lang)
    shiny::observe(shiny.i18n::update_lang(session, lang))
    doiEntryServer(id = "test", translator = doi_entry_translator(), lang = lang)
  }
  shiny::shinyApp(ui, server)
}

#' @describeIn doiEntry Module UI
#' @param translator
#' A [shiny.i18n::Translator] object or `NULL` for english-only defaults.
#' Strings inside the module UI are marked as translateable.
#' You can pass a translator object included in the package,
#' or can create your own `translator` using [shiny.i18n::Translator].
#' This must not be a reactive, it is only set at shiny startup.
#' To update the language reactively *during* a shiny session, see `lang`.
#' @inheritParams shiny::NS
#' @inheritParams shiny::textAreaInput
#' @inheritDotParams shiny::textAreaInput
#' @export
doiEntryUI <- function(id,
                       translator = NULL,
                       width = "100%",
                       height = "400px",
                       ...) {
  require_namespace2("shiny")
  require_namespace2("shinyjs")
  if (is.null(translator)) {
    translator <- dummy_i18n
    i18n_js_insert <- NULL
  } else {
    stopifnot_i18n(translator)
    i18n_js_insert <- shiny.i18n::usei18n(translator)
  }
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    i18n_js_insert,
    shiny::textAreaInput(
      inputId = ns("entered"),
      label = translator$t("Entered DOIs"),
      # this gets translated server side, b/c it's not HTML
      placeholder = "Enter your DOIs here.",
      width = width,
      height = height,
      resize = "vertical",
      ...
    ),
    shiny::div(
      shiny::h5(
        translator$t("Found "),
        shiny::textOutput(
          outputId = ns("found"),
          container = shiny::tags$u,
          inline = TRUE
        ),
        translator$t(" DOIs")
      ),
      shiny::div(view_doi_matchesOutput(outputId = ns("matched")))
    ),
    shiny::div(
      class = "btn-toolbar",
      shiny::actionButton(
        class = "btn-group",
        inputId = ns("fill_ex"),
        label = translator$t("Fill in example"),
        icon = shiny::icon("paste")
      ),
      shiny::actionButton(
        class = "btn-group active",
        inputId = ns("edit"),
        label = translator$t("Edit"),
        icon = shiny::icon("pencil", lib = "glyphicon"),
        disabled = TRUE
      ),
      shiny::actionButton(
        class = "btn-group btn-primary",
        inputId = ns("submit"),
        label = translator$t("Submit"),
        icon = shiny::icon("save", lib = "glyphicon"),
        disabled = TRUE
      )
    )
  )
}

#' @describeIn doiEntry Module server
#' @param example_dois
#' A vector created by, or convertable to [doi()],
#' to be used as as examples.
#' To initiate the UI with the example,
#' pass them to [doiEntryUI()].
#' @param char_limit
#' Integer scalar, giving the maximum number of characters.
#' To protect shiny against overlong strings, you can limit the maximum
#' length of strings allowed.
#' This limit is still enforced server-side, not client-side,
#' so the protection is not bullet-proof.
#' @param lang a reactive variable, returning a character scalar.
#' Must be one of the languages in `translator`.
#' Defaults to `shiny::reactive("en")`,
#' in which case no server side translation
#' is triggered.
#' @return
#' An object of class `biblids_doi` as returned by [doi()].
#' @export
doiEntryServer <- function(id,
                           example_dois = doi_examples(),
                           char_limit = 100L,
                           translator = NULL,
                           lang = shiny::reactive("en")) {
  require_namespace2("shiny")
  require_namespace2("shinyjs")
  require_namespace2("glue")
  require_namespace2("shiny.i18n")
  stopifnot(!shiny::is.reactive(example_dois))
  stopifnot(!shiny::is.reactive(char_limit))
  stopifnot(rlang::is_scalar_integer(char_limit))
  stopifnot_i18n(translator)
  stopifnot(shiny::is.reactive(lang))
  example_dois <- paste(
    as.character(as_doi(example_dois)),
    collapse = " "
  )

  shiny::moduleServer(
    id,
    module = function(input, output, session) {
      # trigger translations if necessary
      if (is.null(translator)) {
        # dummy translator to simplify below code
        translWithLang <- shiny::reactive(list(translate = function(x) x))
      } else {
        translWithLang <- shiny::reactive({
          translator$set_translation_language(lang())
          translator
        })
        shiny::observeEvent(lang(), {
          # client side
          shiny.i18n::update_lang(session, lang())
          # server side
          # this needs special server side updating b/c
          # placeholder cannot be wrapped in t() in UI.
          shiny::updateTextAreaInput(
            session = session,
            inputId = "entered",
            placeholder = translWithLang()$translate("Enter your DOIs here.")
          )
        })
      }

      # input validation
      iv <- shinyvalidate::InputValidator$new()
      shiny::observe({
        iv$add_rule(
          "entered",
          shinyvalidate::sv_required(translWithLang()$translate("Required"))
        )
        iv$add_rule(
          "entered",
          not_longer_than,
          char_limit = char_limit,
          translator = translWithLang()
        )
        iv$add_rule("entered", one_doi, translator = translWithLang())
      })

      # highlight matched DOIs
      output$matched <- renderView_doi_matches(
        view_doi_matches_perline(input$entered)
      )
      # edit and submit UX logic
      shiny::observeEvent(
        input$entered,
        {
          iv$enable()
          if (iv$is_valid()) {
            shinyjs::enable("submit")
            shinyjs::addClass("submit", "active")
          } else {
            shinyjs::disable("submit")
            shinyjs::removeClass("submit", "active")
          }
        },
        ignoreInit = TRUE
      )
      shiny::observeEvent(input$submit, {
        shinyjs::disable("submit")
        shinyjs::removeClass("submit", "active")
        toggle_editable()
      })
      shiny::observeEvent(input$edit, {
        toggle_editable()
      })

      # paste example doi
      shiny::observeEvent(input$fill_ex, {
        shiny::freezeReactiveValue(input, "entered")
        shiny::updateTextAreaInput(
          session = session,
          inputId = "entered",
          value = example_dois
        )
      })

      # ingest
      dois <- shiny::eventReactive(input$submit, {
        shiny::req(iv$is_valid())
        stats::na.omit(as_doi(as.vector(str_extract_all_doi(input$entered))))
      })

      # show number of found DOIs
      output$found <- shiny::renderText({
        length(dois())
      })
      dois
    }
  )
}

#' @describeIn doiEntry Translator
#' Translations shipping with the package,
#' including `r doi_entry_translator()$get_languages()`.
#'
#' To find the keys you need to include in your own translations,
#' look at `biblids::doi_entry_translator()$translations()`.
#' @return a [shiny.i18n::Translator] object.
#' @export
doi_entry_translator <- function() {
  find_translator("doi_entry.json")
}

#' Translator for the showcase app.
#' @noRd
app_translator <- function() {
  find_translator("app.json")
}

#' Find the translator
#' @noRd
find_translator <- function(filename) {
  require_namespace2("shiny.i18n")
  shiny.i18n::Translator$new(
    translation_json_path = system.file(
      package = "biblids",
      "i18n",
      filename
    )
  )
}

#' Check whether translator is legit
#' @export
#' @keywords internal
stopifnot_i18n <- function(translator = NULL) {
  if (!is.null(translator)) {
    stopifnot(!shiny::is.reactive(translator))
    stopifnot(inherits(translator, "Translator"))
    require_namespace2("shiny.i18n")
  }
  invisible(translator)
}

#' Dummy i18n translator object
#' To avoid taking a dependency on shiny.i18n,
#' when preparing a shiny app for translation,
#' the dummy here created can be used.
#' @noRd
dummy_i18n <- list(t = function(keyword) keyword, translate = t)

#' Toggle DOI entry editable state
#' Starts in state from app start, with editable active
#' @noRd
toggle_editable <- function() {
  shinyjs::toggleState("entered")
  shinyjs::toggleState("fill_ex")
  shinyjs::toggleState("edit")
  shinyjs::toggleClass("edit", "active")
}

#' Validate entered string against character limit
#' @noRd
not_longer_than <- function(value,
                            char_limit, 
                            translator = doi_entry_translator()) {
  # this limit should be enforced client side as per
  # https://github.com/rstudio/shiny/issues/3305
  if (nchar(value) > char_limit) {
    glue::glue_collapse(
      glue::glue_safe(
        translator$translate(
          "Cannot parse more than {char_limit} characters."
        ),
        translator$translate("Please provide a shorter input.")
      ),
      sep = " "
    )
  }
}

#' Ensure there's at least one good DOI
#' @noRd
one_doi <- function(value, translator = doi_entry_translator()) {
  # would also be nice to enforce this client-side
  # https://github.com/subugoe/biblids/issues/12
  if (!any(stringr::str_detect(value, regex_doi()))) {
    translator$translate("Please provide at least one valid DOI.")
  }
}

#' View HTML rendering of matched DOIs
#' htmlwidget to show how DOIs are matched in strings.
#'
#' @details
#' Highlighting DOIs matching the regular expression
#' can help users troubleshoot their input,
#' for example inside shiny apps.
#'
#' @inheritParams stringr::str_view_all
#' @examples
#' view_doi_matches("lorem ipsum 10.1000/foo dolor ist 10.1000/bar")
#' @family doi
#' @export
view_doi_matches <- function(string) {
  stringr::str_view_all(
    string = string,
    pattern = regex_doi(),
  )
}

#' @describeIn view_doi_matches
#' Accepts only a scalar string, but retains linebreaks.
#' Helpful for showing matches from input widgets.
#' @export
view_doi_matches_perline <- function(string) {
  stopifnot(rlang::is_scalar_character(string))
  string <- stringr::str_split(string, "\n")[[1]]
  view_doi_matches(string)
}

#' @describeIn view_doi_matches
#' Shiny output widget to show matched DOIs
#' @inheritParams htmlwidgets::shinyWidgetOutput
#' @export
view_doi_matchesOutput <- function(outputId, width = "100%", height = "auto") {
  require_namespace2("htmlwidgets")
  htmlwidgets::shinyWidgetOutput(
    outputId = outputId,
    name = "str_view",
    width = width,
    height = height,
    package = "stringr"
  )
}

#' @describeIn view_doi_matches
#' Shiny render function to show matched DOIs
#' @inheritParams htmlwidgets::shinyRenderWidget
#' @export
renderView_doi_matches <- function(expr, env = parent.frame(), quoted = FALSE) {
  require_namespace2("htmlwidgets")
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(
    expr = expr,
    outputFunction = view_doi_matchesOutput,
    env = env,
    quoted = TRUE
  )
}


# doi.org handles API ====

#' Use the doi.org handles API
#'
#' Queries the
#' [DOI resolution proxy server REST API](https://www.doi.org/factsheets/DOIProxy.html#rest-api).
#' - Retries failed requests.
#' - Caches results (when [memoise::memoise()] is installed).
#'
#' @section Warning:
#' If you are using this in your own package, or create a lot of traffic,
#' please set your own [httr::user_agent()].
#'
#' @section Other APIs:
#' This client only queries the doi.org API.
#' The doi.org API only includes information on DOI resolution,
#' not other metadata.
#' For other APIs, see [doi_ras()]
#'
#' @family doi
#' @name doi_api
NULL

#' VERB the doi.org API
#' @noRd
verb_doi <- function(...) {
  retry_doi(...)
}

#' RETRY the doi.org API
#' @noRd
retry_doi <- function(...) {
  require_namespace2("httr")
  httr::RETRY(
    url = "https://doi.org",
    httr::user_agent("http://github.com/subugoe/biblids"),
    ...
  )
}

#' GET the doi.org API
#' @noRd
get_doi <- function(...) verb_doi(verb = "GET", ...)

#' VERB the doi.org handles endpoint
#' @noRd
verb_doi_handle <- function(x, verb, ...) {
  verb_doi(
    verb = verb,
    path = paste0("api/handles/", doi2path(x)),
    ...
  )
}

#' Create the path to a doi.org API from a [doi()]
#' @noRd
doi2path <- function(x) {
  require_namespace2("curl")
  x <- as_doi(x)
  if (vctrs::vec_size(x) != 1) rlang::abort("Must be a doi vector of length 1.")
  if (is.na(x)) rlang::abort("Must not be NA.")
  paste0(
    curl::curl_escape(vctrs::field(x, "prefix")),
    "/",
    curl::curl_escape(vctrs::field(x, "suffix"))
  )
}

#' GET the doi.org handles endpoint
#' @noRd
get_doi_handle <- function(x, ...) {
  resp <- verb_doi_handle(x, verb = "GET", ...)
  res <- verifynparse_doi_api_resp(resp)
  if (res$responseCode == 200) {
    # this is the json response code, which indicates missing values
    # the http resp code would actually be 200 in this case
    rlang::warn(c(
      "The handle exists on doi.org, but the values were not found, because the handle",
      "has no values or",
      "has no values for types and indices specified."
    ))
  }
  res
  }

#' Verify and parse the DOI API response for get handle and whichRA requests
#' @noRd
verifynparse_doi_api_resp <- function(resp) {
  require_namespace2("jsonlite")
  # more informative error message than httr:stop_for_status
  # status codes as per https://www.doi.org/doi_handbook/3_Resolution.html#3.8.1
  if (httr::status_code(resp) == 404) {
    doi_not_found()
  }
  # catch other errors
  httr::stop_for_status(resp)
  if (httr::http_type(resp) != "application/json") {
    rlang::abort("API did not return json.")
  }
  jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
}

doi_not_found <- function() {
  rlang::abort("Handle not found on doi.org")
}

#' @describeIn doi_api
#' Query the handles endpoint.
#' For details, see the
#' [DOI REST API documentation](https://www.doi.org/factsheets/DOIProxy.html#rest-api).
#'
#' @inheritParams as_doi
#' @param query
#' A named list of
#' [query parameters](https://www.doi.org/factsheets/DOIProxy.html#query-parameters).
#' @inheritParams httr::GET
#' @inheritDotParams httr::GET -url
#' @example inst/examples/doi/get_doi_handles.R
#' @export
get_doi_handles <- function(x,
                            query = NULL,
                            ...) {
  x <- as_doi(x)
  # hack-fix until https://github.com/subugoe/biblids/issues/51
  x <- as.character(x)
  purrr::map(.x = x, .f = get_doi_handle, query = query, ...)
}

#' @describeIn doi_api
#' Get the resolved URL for a DOI.
#' Returns `NA` if there is no URL value (rare, but theoretically possible).
#' @example inst/examples/doi/resolve_doi.R
#' @export
resolve_doi <- function(x, ...) {
  res <- get_doi_handles(x, query = list(type = "URL"), ...)
  purrr::map_chr(res, purrr::pluck, "values", 1, "data", "value", .default = NA)
}

#' @describeIn doi_api
#' Tests whether there is a URL to resolve to.
#' Simple wrapper around [resolve_doi()]
#' @examples
#' is_doi_resolvable(c("10.1000/1", "10.1000/2"))
#' @export
is_doi_resolvable <- function(x, ...) !(is.na(resolve_doi(x, ...)))

#' HEAD the doi.org handles endpoint
#' @noRd
head_doi_handle <- function(x, ...) {
  resp <- verb_doi_handle(x = x, verb = "HEAD", terminate_on = 404,...)
  if (httr::status_code(resp) == 200) return(TRUE)
  if (httr::status_code(resp) == 404) return(FALSE)
  httr::stop_for_status(resp)
}

#' @describeIn doi_api Test whether DOI handle can be found on doi.org.
#' @example inst/examples/doi/is_doi_found.R
#' @export
is_doi_found <- function(x, ...) {
  x <- as_doi(x)
  # hack-fix until https://github.com/subugoe/biblids/issues/51
  x <- as.character(x)
  purrr::map_lgl(.x = x, .f = head_doi_handle, ...)
}


# doi.org which RA API ====

#' Get the DOI Registration Agency (RA)
#'
#' Registrants do not register DOIs directly with the DOI Foundation,
#' but go through RAs.
#'
#' @details
#' Some of these RAs have their own APIs to access additional metadata.
#'
#' Selected APIs with existing R wrappers include:
#' - [Crossref](http://crossref.org),
#'    wrapped by the [rcrossref](https://docs.ropensci.org/rcrossref/) R client,
#' - [Datacite](https://datacite.org),
#'    wrapped by the [rdatacite](https://docs.ropensci.org/rdatacite/) R client,
#' - [OP](https://op.europa.eu/),
#'    (partially ?) wrapped bye the
#'    [eurlex](https://cran.r-project.org/web/packages/eurlex/index.html)
#'    R client.
#' 
#' It's easy to confuse these more comprehensive wrappers
#' geared towards particular RAs or services
#' with those API wrappers included in biblids such as [get_doi_handles()],
#' because *both types of API accept DOIs as inputs*.
#'
#' @name doi_ra
#' @family doi
NULL

#' @describeIn doi_ra
#' All current DOI RAs, returned as a character string,
#' named with its short name.
#' Taken from [doi.org](https://www.doi.org/RA_Coverage.html),
#' where you can also learn more about areas of coverage.
#' @examples
#' doi_ras()
#' @export
doi_ras <- function() {
  c(
    `Airiti` = "Airiti, Inc.",
    `CNKI` = "China National Knowledge Infrastructure",
    `Crossref` = "Crossref",
    `DataCite` = "DataCite",
    `EIDR` = "Entertainment Identifier Registry",
    `ISTIC` = "The Institute of Scientific and Technical Information of China",
    `JaLC` = "Japan Link Center",
    `KISTI` = "Korea Institute of Science and Technology Information",
    `mEDRA` = "Multilingual European DOI Registration Agency",
    `OP` = "Publications Office of the European Union",
    `Public` = "International DOI Foundation"
  )
}

#' @describeIn doi_ra
#' Get DOI RA using the doi.org
#' [Which RA?](https://www.doi.org/factsheets/DOIProxy.html#whichra) service.
#' @inheritSection doi_api Warning
#' @inheritParams as_doi
#' @inheritParams httr::GET
#' @inheritDotParams httr::GET -url
#' @example inst/examples/doi/get_doi_ra.R
#' @export
get_doi_ra <- function(x, ...) {
  # hack-fix until https://github.com/subugoe/biblids/issues/51
  x <- as.character(x)
  purrr::map_chr(.x = x, .f = get_doi_ra1, ...)
}

#' Helper to get one DOI RA
#'
#' The API actually supports native vectorisation,
#' but encoding that in a URL seems to risky and ambiguous
#' and anyway not in line with the rest of the pkg.
#' @noRd
get_doi_ra1 <- function(x, ...) {
  resp <- verb_doi(verb = "GET", path = paste0("doiRA/", doi2path(x)), ...)
  res <- verifynparse_doi_api_resp(resp)[[1]]
  status <- res$status
  # unfortunately, the structure of the object changes depending on res :(
  if (!is.null(status)) {
    switch(status,
      # this really should have been caught by syntax validation,
      # but you never know ...
      `Invalid DOI` = rlang::abort("Invalid DOI"),
      # you'd expect that this were caught above by a 404,...
      # but no, the which RA api gives http status 200 on non-existent DOIs
      # the handles API does give http 404, so the behavior is different
      `DOI does not exist` = doi_not_found(),
      `Unknown` = return(NA_character_)
    )
  }
  res$RA
}

#' @describeIn doi_ra
#' Test whether a DOI is registed by an RA
#' @param ra Character scalar, must be one of `names(doi_ras())`.
#' @example inst/examples/doi/is_doi_from_ra.R
#' @export
is_doi_from_ra <- function(x, ra = names(doi_ras()), ...) {
  rlang::arg_match(ra)
  get_doi_ra(x, ...) == ra
}

# example DOIs ====

#' Example DOIs
#' @param na.rm
#' Logical scalar, whether to remove `NA`s.
#' Helpful for testing and documentation.
#' @export
#' @family doi
#' @examples
#' doi_examples()
doi_examples <- function(na.rm = TRUE) {
  # this reduces doi examples from elsewhere in the pkg source
  res <- c(
    source_pef("doi", "doi.R"),
    source_pef("doi", "as_doi.R"),
    source_pef("doi", "str_extract_doi.R"),
    as.vector(source_pef("doi", "str_extract_all_doi.R")),
    as_doi(brio::read_lines(path_ex_file("doi", "get_doi_ra.R")))
  )
  if (na.rm) {
    res <- stats::na.omit(res)
  }
  res
}
