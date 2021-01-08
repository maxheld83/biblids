.onLoad <- function(...) {
  vctrs::s3_register(generic = "pillar::pillar_shaft", class = "biblids_doi")
  vctrs::s3_register(generic = "knitr::knit_print", class = "biblids_doi")
  invisible()
}
