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

# doi_ish can be detected

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["value", "visible"]
        }
      },
      "value": [
        {
          "type": "logical",
          "attributes": {},
          "value": [false, true, true]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true]
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
          "value": ["10.1594", "10.1594", "10.1000", "10.1000", null]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["PANGAE.726855", "GFZ.GEOFON.gfz2009kciu", "182", "1", null]
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

# single DOIs are extracted

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["value", "visible"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["10.1594/PANGAEA.726855", "10.1594/GFZ.GEOFON.gfz2009kciu", "10.1594/PANGAEA.667386", "10.3207/2959859860", "10.3866/PKU.WHXB201112303", "10.3972/water973.0145.db", null]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true]
        }
      ]
    }

# multiple DOIs are extracted

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["value", "visible"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {
            "dim": {
              "type": "integer",
              "attributes": {},
              "value": [5, 2]
            }
          },
          "value": ["10.7666/d.y351065", "10.11467/isss2003.7.1_11", "10.1430/8105", "10.5194/wes-2019-70", "", "", "10.7875/leading.author.2.e008", "10.1392/BC1.0", "10.5194/wes-5-819-202", ""]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true]
        }
      ]
    }

# DOI urls are percent escaped

    "https://doi.org/api/handles/10.1000%2Ffoo%23bar"

