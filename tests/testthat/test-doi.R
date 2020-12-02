test_that("good crossref DOI is accepted", {
  # from https://www.crossref.org/education/metadata/persistent-identifiers/doi-display-guidelines/
  expect_true(is_doi("10.13003/5jchdy"))
})

test_that("other strings are rejected", {
  expect_false(is_doi("lorem ipsum"))
})
