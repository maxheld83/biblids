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
