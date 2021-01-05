.onLoad <- function(...) {
  vctrs::s3_register(generic = "pillar::pillar_shaft", class = "biblids_doi")
  invisible()
}
