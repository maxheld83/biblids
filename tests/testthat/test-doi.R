test_that("doi object is created", {
  expect_snapshot(doi(prefix = "10.1038", suffix = "nphys1170"))
})

test_that("good crossref DOI is accepted", {
  # from https://www.crossref.org/education/metadata/persistent-identifiers/doi-display-guidelines/
  expect_true(is_doi("10.13003/5jchdy"))
})

test_that("other strings are rejected", {
  expect_false(is_doi("lorem ipsum"))
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
