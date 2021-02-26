as_doi(c(
  # example DOIs are from https://www.doi.org/demos.html
  "10.1594/PANGAE.726855",
  " 10.1594/GFZ.GEOFON.gfz2009kciu ",  # leading/trailing spaces are removed
  "https://doi.org/10.1000/182",  # URL form is parsed
  "doi:10.1000/7",  # DOI from is parsed
  "foo bar"  # returns NA
))
