str_extract_all_doi(string = c(
  # nothing to do here
  "10.17487/rfc1149",
    # space separated
  "10.1016/j.iheduc.2003.11.004 doi:10.7875/leading.author.2.e008",
  # separated by forbidden
  "doi:10.6084/m9.figshare.97218&doi:10.1126/science.169.3946.635 ",
  # separated by linebreak
  "10.5194/wes-2019-70\n10.5194/wes-5-819-202",
  # no DOI here
  "quux"
))
