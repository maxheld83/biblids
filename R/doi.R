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
        "All values must be valid DOI syntax.",
        x = "Bad `prefix` found.",
        i = "Try casting with `as_doi()`."
      )
    )
  }
  suffixes_good <- is_doi_syntax(x, "suffix")
  if (!all(suffixes_good)) {
    rlang::abort(
      c(
        "All values must be valid DOI syntax.",
        x = "Bad `suffix` found.",
        i = "Try casting with `as_doi()`."
      )
    )
  }
  x
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
  # called part instead of field to aboid name clash with vctrs
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
  res <- stringr::str_split_fixed(x, pattern = "/", n = 2)
  new_doi(res[, 1], res[, 2])
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
  # no extra validation necessary because above extraction is already doi only
}

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
  require_namespace2("shiny")
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
#' @param char_limit
#' Integer scalar, giving the maximum number of characters.
#' To protect shiny against overlong strings, you can limit the maximum
#' length of strings allowed.
#' This limit is still enforced server-side, not client-side,
#' so the protection is not bullet-proof.
#' @export
doiEntryServer <- function(id, char_limit = 100000L) {
  require_namespace2("shiny")
  require_namespace2("glue")
  stopifnot(rlang::is_scalar_integer(char_limit))
  shiny::moduleServer(
    id,
    module = function(input, output, session) {
      # input validation
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("entered", shinyvalidate::sv_required())
      # this limit should be enforced client side as per
      # https://github.com/rstudio/shiny/issues/3305
      iv$add_rule(
        "entered",
        function(value) {
          if (nchar(value) > char_limit) {
            glue::glue(
              "Cannot parse more than {char_limit} characters.",
              "Please provide a shorter input.",
              sep = " "
            )
          }
        }
      )
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
  require_namespace2("jsonlite")
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
    # this should be na.omit https://github.com/subugoe/biblids/issues/50
    res <- res[!is.na(res)]
  }
  # this should not be necessary https://github.com/subugoe/biblids/issues/60
  res <- res[res != "/"]
  res
}
