str_extract_all_doi(string = c(
  "10.7666/d.y351065",  # nothing to do here
  "10.11467/isss2003.7.1_11 doi:10.7875/leading.author.2.e008",  # space separated
  "doi:10.1430/8105&doi:10.1392/BC1.0 ",  # separated by forbidden
  "quux" # no DOI here
))
