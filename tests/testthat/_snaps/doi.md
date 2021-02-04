# doi helper creates DOI records

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["prefix", "suffix"]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["biblids_doi", "vctrs_rcrd", "vctrs_vctr"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["10.1038", null, "10.11467"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["nphys1170", "foobar", "isss2003.7.1_11"]
        }
      ]
    }

# doi fields are cast from more constrained types

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["prefix", "suffix"]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["biblids_doi", "vctrs_rcrd", "vctrs_vctr"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["10.13003"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["5jchdy"]
        }
      ]
    }

# dois fields are recycled

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["prefix", "suffix"]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["biblids_doi", "vctrs_rcrd", "vctrs_vctr"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["10.1371", "10.1371"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["journal.pbio.0020449", "journal.pbio.0020449.g001"]
        }
      ]
    }

# dois can be coerced

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["prefix", "suffix"]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["biblids_doi", "vctrs_rcrd", "vctrs_vctr"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["10.1038", "10.1002"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["nphys1170", "0470841559.ch1"]
        }
      ]
    }

# dois can be cast to characters

    {
      "type": "character",
      "attributes": {},
      "value": ["10.1038/nphys1170", null, "10.11467/isss2003.7.1_11"]
    }

# characters can be cast to dois

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["prefix", "suffix"]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["biblids_doi", "vctrs_rcrd", "vctrs_vctr"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["10.1594", "10.1594"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["PANGAE.726855", "GFZ.GEOFON.gfz2009kciu"]
        }
      ]
    }

# DOIs are printed and formatted

    [1] "10.1038/nphys1170"        NA                        
    [3] "10.11467/isss2003.7.1_11"

---

    {
      "type": "character",
      "attributes": {
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["knit_asis"]
        },
        "knit_cacheable": {
          "type": "logical",
          "attributes": {},
          "value": [null]
        }
      },
      "value": ["- [`https://doi.org/10.1038/nphys1170`](https://doi.org/10.1038/nphys1170)\n", "- `NA`\n", "- [`https://doi.org/10.11467/isss2003.7.1_11`](https://doi.org/10.11467/isss2003.7.1_11)\n"]
    }

---

    {
      "type": "character",
      "attributes": {
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["knit_asis"]
        },
        "knit_cacheable": {
          "type": "logical",
          "attributes": {},
          "value": [null]
        }
      },
      "value": ["- [`doi:10.1038/nphys1170`](https://doi.org/10.1038/nphys1170)\n", "- `NA`\n", "- [`doi:10.11467/isss2003.7.1_11`](https://doi.org/10.11467/isss2003.7.1_11)\n"]
    }

---

    {
      "type": "character",
      "attributes": {
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["knit_asis"]
        },
        "knit_cacheable": {
          "type": "logical",
          "attributes": {},
          "value": [null]
        }
      },
      "value": ["[`https://doi.org/10.1038/nphys1170`](https://doi.org/10.1038/nphys1170), `NA` and [`https://doi.org/10.11467/isss2003.7.1_11`](https://doi.org/10.11467/isss2003.7.1_11)"]
    }

# DOIs make pretty tibble columns

    # A tibble: 3 x 1
      `doi_examples()[1:3]`   
      <doi>                   
    1 10.1038/nphys1170       
    2 NA                      
    3 10.11467/isss2003.7.1_11

# doi with one NA field become all NA

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["prefix", "suffix"]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["biblids_doi", "vctrs_rcrd", "vctrs_vctr"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": [null, "10.13003"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["5jchdy", "5jchdy"]
        }
      ]
    }

