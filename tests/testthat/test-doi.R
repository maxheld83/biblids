# construction ====
test_that("doi creates empty prototype", {
  testthat::expect_length(doi(), 0L)
})

test_that("doi helper creates doi records", {
  testthat::expect_snapshot_output(doi_examples()[1:3])
})

test_that("doi fields are cast from more constrained types", {
  testthat::expect_snapshot(doi(factor("10.13003"), factor("5jchdy")))
})

test_that("dois fields are recycled", {
  testthat::expect_snapshot(doi(
    "10.1371",
    c("journal.pbio.0020449", "journal.pbio.0020449.g001")
  ))
})

test_that("doi helper errors on invalid field inputs", {
  testthat::expect_error(doi(prefix = 1L, suffix = 2.2))
})

# casting and coercion ====

test_that("dois can be coerced", {
  testthat::expect_snapshot(c(doi_examples()[1], "10.1002/0470841559.ch1"))
})

test_that("dois can be cast to characters", {
  testthat::expect_snapshot(as.character(doi_examples()[1:3]))
})

test_that("characters can be cast to dois", {
  testthat::expect_snapshot(source(path_ex_file("doi", "as_doi.R"))$value)
})

# presentation methods ====

test_that("DOIs are printed and formatted", {
  testthat::expect_snapshot(format(doi_examples()[1:3]))
  testthat::expect_snapshot(knitr::knit_print(doi_examples()[1:3]))
  testthat::expect_snapshot(
    knitr::knit_print(doi_examples()[1:3], display = "doi")
  )
  testthat::expect_snapshot(
    knitr::knit_print(doi_examples()[1:3], inline = TRUE)
  )
})

test_that("DOIs make pretty tibble columns", {
  testthat::expect_snapshot_output(tibble::tibble(doi_examples()[1:3]))
})


# other methods ====

test_that("doi with one NA field become all NA", {
  testthat::expect_true(is.na(doi(NA, "5jchdy")))
  testthat::expect_snapshot(doi(c(NA, "10.13003"), c("5jchdy", "5jchdy")))
})


test_that("good crossref DOI is accepted", {
  # from https://www.crossref.org/education/metadata/persistent-identifiers/doi-display-guidelines/
  expect_true(is_doi2("10.13003/5jchdy"))
})

test_that("other strings are rejected", {
  expect_false(is_doi2("lorem ipsum"))
})

test_that("DOIs are extracted", {
  doimash <- c(
    paste0(
      noise = "foo",
      doi1 = "10.13003/5jchdy",
      comma = ",",
      noise_w_space = " zap",
      linebreak = "\\n",
      doi2 = "10.5281/zenodo.3892950"
    ),
    paste0(
      noise_w_space = "bar ",
      doi1_url = "http://doi.org/10.5281/zenodo.3892951",
      linebreak = "\\n",
      doi2_url = "https://doi.org/10.1109/5.771073"
    )
  )
  expect_snapshot(str_extract_all_doi(doimash))
})

test_that("crossref DOIs are identified", {
  doi_cr <- "10.5194/wes-2019-70"
  doi_not_cr <- "10.5194/wes-5-819-202"
  doi_bad <- " create a url with spaces"
  expect_true(is_doi_on_cr(doi_cr))
  expect_false(is_doi_on_cr(doi_not_cr))
  expect_error(is_doi_on_cr(doi_bad))
})
