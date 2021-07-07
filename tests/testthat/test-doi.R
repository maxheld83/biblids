expect_snapshot_value2 <- purrr::partial(
  expect_snapshot_value,
  style = "json2"
)

# to test very narrow behavior, use fake DOIs ("10.1000/foo")
# to test more complex behavior (printing etc.) use real DOIs from inst/

# construction ====
test_that("helper creates empty prototype", {
  expect_length(doi(), 0L)
})

test_that("helper creates proper records", {
  expect_snapshot_value2(source_pef("doi", "doi.R"))
})

test_that("fields are cast from more constrained types", {
  expect_snapshot_value2(doi(factor("10.1000"), factor("norf")))
})

test_that("fields are recycled", {
  expect_snapshot_value2(doi("10.1000", c("zap", "zong")))
})

test_that("helper errors on invalid field inputs", {
  expect_error(doi(prefix = 1L, suffix = 2.2))
})

# validation ====

test_that("doi validates fields", {
  # foo is a valid suffix
  expect_visible(doi("10.1000", "foo"))
  # registrant prefixes are usually, but need not be 4 long
  # for more info, see https://github.com/subugoe/biblids/issues/63
  expect_visible(doi("10.987.12345", "9990"))
  expect_error(doi("bar", "foo"))
  expect_error(doi(" 10.1000", "foo"))
  expect_error(doi("10.1000 ", "foo"))
  expect_error(doi("a10.1000", "foo"))
  # 10.1000 is a valid prefix
  expect_visible(doi("10.1000", "foo"))
  expect_error(doi("10.1000", "&"))
  expect_error(doi("10.1000", " foo"))
  expect_error(doi("10.1000", "foo "))
  expect_error(doi("10.1000", ""))
})

test_that("doi_ish can be detected", {
  expect_equal(is_doi_ish(c("10.1000/1", "foo")), TRUE) # foo will be NA
  # NA will be cast to character by c()
  expect_equal(is_doi_ish(c("10.1000/1", NA)), TRUE)
  expect_equal(is_doi_ish(1L), FALSE)
  # multiple DOIs cannot be cast unambiguosly
  expect_equal(is_doi_ish(c("10.1000/1", "10.1000/2 10.1000/3")), FALSE)
  expect_equal(is_doi_ish(NA_character_), TRUE)
  expect_equal(is_doi_ish(NA_integer_), FALSE)
})

# casting and coercion ====
test_that("DOIs can be coerced", {
  expect_snapshot_value2(c(as_doi("10.1000/foo"), "10.1000/zap"))
  # c acts differently depending on the order unfortunately
  expect_snapshot_value2(vctrs::vec_c("10.1000/frotz", as_doi("10.1000/qux")))
})

test_that("Bad DOIs will not be coerced", {
  expect_equal(
    c(as_doi("10.1000/foo"), ""), # should be coerced to NA
    as_doi(c("10.1000/foo", ""))
  )
})

test_that("DOIs can be cast to characters", {
  expect_snapshot_value2(as.character(doi("10.1000", c("grault", NA, "thud"))))
})

test_that("characters can be cast to DOIs", {
  expect_snapshot_value2(source_pef("doi", "as_doi.R"))
  expect_error(as_doi(c("10.1000/ham 10.1000/spam")))
})

# presentation methods ====

test_that("DOIs are printed and formatted", {
  expect_snapshot_output(format(doi_examples(na.rm = FALSE)[1:3]))
  expect_snapshot_value2(knitr::knit_print(doi_examples(na.rm = FALSE)[1:3]))
  expect_snapshot_value2(
    knitr::knit_print(doi_examples(na.rm = FALSE)[1:3], display = "doi")
  )
  expect_snapshot_value2(
    knitr::knit_print(doi_examples(na.rm = FALSE)[1:3], inline = TRUE)
  )
})

test_that("DOIs make pretty tibble columns", {
  expect_snapshot_output(tibble::tibble(doi_examples(na.rm = FALSE)[1:3]))
})


# other methods ====

test_that("DOIs with one NA field become all NA", {
  expect_true(is.na(doi(NA, "frotz")))
  expect_snapshot_value2(doi(c(NA, "10.1000"), c("gizmo", "acme")))
})

test_that("DOIs are compared with case insensitivity", {
  expect_true(as_doi("10.1000/dingBAT") == as_doi("10.1000/dingbat"))
})

# extraction ====

test_that("single DOIs are extracted", {
  expect_snapshot_value2(source_pef("doi", "str_extract_doi.R"))
})

test_that("multiple DOIs are extracted", {
  expect_snapshot_value2(source_pef("doi", "str_extract_all_doi.R"))
  expect_equal(as_doi('zap'), doi(NA, NA))
})


# doi.org handles api ====

test_that("doi.org handles api works", {
  expect_snapshot_value(source_pef("doi", "get_doi_handles.R"))
})

test_that("doi.org handles api warns on empty value", {
  expect_warning(get_doi_handles("10.1000/1", query = list(type = "zap")))
})

test_that("DOI can be resolved to a url", {
  expect_equal(
    source_pef("doi", "resolve_doi.R"),
    c(
      "http://www.nature.com/articles/nphys1170",
      "http://www.doi.org/index.html"
    )
  )
  # would be good to test a doi which is has no URL which should give NA
  # but I don't have such a DOI if it exists at all
})

test_that("DOI resolvability can be tested", {
  expect_true(is_doi_resolvable("10.1000/1"))
})

test_that("DOI indexation can be detected", {
  expect_equal(
    source_pef("doi", "is_doi_found.R"),
    c(FALSE, TRUE)
  )
})

test_that("DOI api is cached", {
  random_dois <- paste0("10.1000/", sample(LETTERS, 10), sample(1:100, 10))
  before <- system.time(is_doi_found(random_dois))["elapsed"]
  after <- system.time(is_doi_found(random_dois))["elapsed"]
  expect_lt(after, before / 10L)
})

# doi.org which ra api ====

test_that("DOI RAs can be identified", {
  expect_equal(
    source_pef("doi", "get_doi_ra.R"),
    names(doi_ras())
  )
})

test_that("DOI RAs can be tested", {
  expect_equal(
  source_pef("doi", "is_doi_from_ra.R"),
    c(FALSE, TRUE)
  )
})

# examples ====
test_that("Examples are unique", {
  expect_true(!any(duplicated(doi_examples())))
})
