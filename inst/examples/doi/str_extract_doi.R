str_extract_doi(string = c(
  "10.1594/PANGAEA.726855",  # nothing to do here
  "10.1119/1.16433 ",  # remove space
  " 10.1594/PANGAEA.667386", # remove space
  "doi:10.3866/PKU.WHXB201112303", # remove DOI
  "http://dx.doi.org/10.3352/jeehp.2013.10.3", # parse URL
  "10.3972/water973.0145.db&", # remove forbidden symbol
  "foo bar" # no DOI here
))
