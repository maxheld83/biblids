# helper creates proper records

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
          "value": ["10.1038", "10.1000", "10.1007"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["nphys1170", null, "978-3-642-65840-2_5"]
        }
      ]
    }

# fields are cast from more constrained types

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
          "value": ["10.1000"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["norf"]
        }
      ]
    }

# fields are recycled

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
          "value": ["10.1000", "10.1000"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["zap", "zong"]
        }
      ]
    }

# DOIs can be coerced

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
          "value": ["10.1000", "10.1000"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["foo", "zap"]
        }
      ]
    }

---

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
          "value": ["10.1000", "10.1000"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["frotz", "qux"]
        }
      ]
    }

# DOIs can be cast to characters

    {
      "type": "character",
      "attributes": {},
      "value": ["10.1000/grault", null, "10.1000/thud"]
    }

# characters can be cast to DOIs

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
          "value": ["PANGAE.726855", "GFZ.GEOFON.gfz2009kciu", "182", "7", null]
        }
      ]
    }

# DOIs are printed and formatted

    [1] "10.1038/nphys1170"           NA                           
    [3] "10.1007/978-3-642-65840-2_5"

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
      "value": ["- [`https://doi.org/10.1038/nphys1170`](https://doi.org/10.1038/nphys1170)\n", "- `NA`\n", "- [`https://doi.org/10.1007/978-3-642-65840-2_5`](https://doi.org/10.1007/978-3-642-65840-2_5)\n"]
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
      "value": ["- [`doi:10.1038/nphys1170`](https://doi.org/10.1038/nphys1170)\n", "- `NA`\n", "- [`doi:10.1007/978-3-642-65840-2_5`](https://doi.org/10.1007/978-3-642-65840-2_5)\n"]
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
      "value": ["[`https://doi.org/10.1038/nphys1170`](https://doi.org/10.1038/nphys1170), `NA` and [`https://doi.org/10.1007/978-3-642-65840-2_5`](https://doi.org/10.1007/978-3-642-65840-2_5)"]
    }

# DOIs make pretty tibble columns

    # A tibble: 3 x 1
      `doi_examples(na.rm = FALSE)[1:3]`
      <doi>                             
    1 10.1038/nphys1170                 
    2 NA                                
    3 10.1007/978-3-642-65840-2_5       

# DOIs with one NA field become all NA

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
          "value": [null, "10.1000"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["gizmo", "acme"]
        }
      ]
    }

# single DOIs are extracted

    {
      "type": "character",
      "attributes": {},
      "value": ["10.1594/PANGAEA.726855", "10.1119/1.16433", "10.1594/PANGAEA.667386", "10.3866/PKU.WHXB201112303", "10.3352/jeehp.2013.10.3", "10.3972/water973.0145.db", null]
    }

# multiple DOIs are extracted

    {
      "type": "character",
      "attributes": {
        "dim": {
          "type": "integer",
          "attributes": {},
          "value": [5, 2]
        }
      },
      "value": ["10.17487/rfc1149", "10.1016/j.iheduc.2003.11.004", "10.6084/m9.figshare.97218", "10.5194/wes-2019-70", "", "", "10.7875/leading.author.2.e008", "10.1126/science.169.3946.635", "10.5194/wes-5-819-202", ""]
    }

# doi.org handles api works

    [
      {
        "responseCode": 1,
        "handle": "10.1000/1",
        "values": [
          {
            "index": 1,
            "type": "URL",
            "data": {
              "format": "string",
              "value": "http://www.doi.org/index.html"
            },
            "ttl": 86400,
            "timestamp": "2014-09-26T14:40:46Z"
          }
        ]
      },
      {
        "responseCode": 1,
        "handle": "10.1038/nphys1170",
        "values": [
          {
            "index": 1,
            "type": "URL",
            "data": {
              "format": "string",
              "value": "http://www.nature.com/articles/nphys1170"
            },
            "ttl": 86400,
            "timestamp": "2017-12-29T06:49:37Z"
          }
        ]
      }
    ]

