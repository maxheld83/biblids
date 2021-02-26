expect_snapshot_value2 <- purrr::partial(
  expect_snapshot_value,
  style = "json2"
)

# construction ====
test_that("doi creates empty prototype", {
  expect_length(doi(), 0L)
})

test_that("doi helper creates DOI records", {
  expect_snapshot_value2(doi_examples()[1:3])
})

test_that("doi fields are cast from more constrained types", {
  expect_snapshot_value2(doi(factor("10.13003"), factor("5jchdy")))
})

test_that("dois fields are recycled", {
  expect_snapshot_value2(doi(
    "10.1371",
    c("journal.pbio.0020449", "journal.pbio.0020449.g001")
  ))
})

test_that("doi helper errors on invalid field inputs", {
  expect_error(doi(prefix = 1L, suffix = 2.2))
})

# validation ====

test_that("doi validates fields", {
  # foo is a valid suffix
  expect_visible(doi("10.1000", "foo"))
  expect_error(doi("bar", "foo"))
  expect_error(doi(" 10.1000", "foo"))
  expect_error(doi("10.1000 ", "foo"))
  expect_error(doi("a10.1000", "foo"))
  # 10.1000 is a valid prefix
  expect_visible(doi("10.1000", "foo"))
  expect_error(doi("10.1000", "&"))
  expect_error(doi("10.1000", " foo"))
  expect_error(doi("10.1000", "foo "))
})

test_that("doi_ish can be detected", {
  expect_equal(
    source_pef("doi", "is_doi_ish.R"),
    c(FALSE, TRUE, TRUE)
  )
})


# casting and coercion ====
test_that("dois can be coerced", {
  expect_snapshot_value2(c(doi_examples()[1], "10.1002/0470841559.ch1"))
  # c acts differently depending on the order unfortunately
  expect_snapshot_value2(vctrs::vec_c("10.1002/0470841559.ch1", doi_examples()[1]))
})

test_that("dois can be cast to characters", {
  expect_snapshot_value2(as.character(doi_examples()[1:3]))
})

test_that("characters can be cast to dois", {
  expect_snapshot_value2(source_pef("doi", "as_doi.R"))
  expect_error(as_doi(c("10.1126/science.169.3946.635 10.6084/m9.figshare.97218")))
})

# presentation methods ====

test_that("DOIs are printed and formatted", {
  expect_snapshot_output(format(doi_examples()[1:3]))
  expect_snapshot_value2(knitr::knit_print(doi_examples()[1:3]))
  expect_snapshot_value2(knitr::knit_print(doi_examples()[1:3], display = "doi"))
  expect_snapshot_value2(knitr::knit_print(doi_examples()[1:3], inline = TRUE))
})

test_that("DOIs make pretty tibble columns", {
  expect_snapshot_output(tibble::tibble(doi_examples()[1:3]))
})


# other methods ====

test_that("doi with one NA field become all NA", {
  expect_true(is.na(doi(NA, "5jchdy")))
  expect_snapshot_value2(doi(c(NA, "10.13003"), c("5jchdy", "5jchdy")))
})

test_that("DOIs are compared with case insensitivity", {
  expect_true(as_doi("10.1000/foo") == as_doi("10.1000/fOo"))
})

# extraction ====

test_that("single DOIs are extracted", {
  expect_snapshot_value2(source_pef("doi", "str_extract_doi.R"))
})

test_that("multiple DOIs are extracted", {
  expect_snapshot_value2(source_pef("doi", "str_extract_all_doi.R"))
})


# doi.org handles api ====

test_that("DOI API works", {
  expect_snapshot_value(source_pef("doi", "get_doi_handles.R"))
})

test_that("DOI API gives warning on empty value", {
  expect_warning(get_doi_handles("10.1000/1", query = list(type = "zap")))
})

test_that("DOI can be resolved to url", {
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

test_that("DOI resolvability can be detected", {
  expect_true(is_doi_resolvable("10.1000/1"))
})

test_that("DOI indexation can be detected", {
  expect_equal(
    source_pef("doi", "is_doi_found.R"),
    c(FALSE, TRUE)
  )
})

# doi.org which ra api ====

test_that("DOI RAs can be identified", {
  expect_equal(
    source_pef("doi", "get_doi_ra.R"),
    names(doi_ras())
  )
})
