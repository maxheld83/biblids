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


# casting and coercion ====
test_that("dois can be coerced", {
  expect_snapshot_value2(c(doi_examples()[1], "10.1002/0470841559.ch1"))
})

test_that("dois can be cast to characters", {
  expect_snapshot_value2(as.character(doi_examples()[1:3]))
})

test_that("characters can be cast to dois", {
  expect_snapshot_value2(source(path_ex_file("doi", "as_doi.R"))$value)
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

# extraction ====

test_that("DOIs are extracted", {
  expect_true(TRUE)
})

test_that("crossref DOIs are identified", {
  doi_cr <- "10.5194/wes-2019-70"
  doi_not_cr <- "10.5194/wes-5-819-202"
  doi_bad <- " create a url with spaces"
  expect_true(is_doi_on_cr(doi_cr))
  expect_false(is_doi_on_cr(doi_not_cr))
  expect_error(is_doi_on_cr(doi_bad))
})
