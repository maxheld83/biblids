# doi helper creates doi records

    <digital object identifier[3]>
    [1] 10.1038/nphys1170        <NA>                     10.11467/isss2003.7.1_11

# doi fields are cast from more constrained types

    Code
      doi(factor("10.13003"), factor("5jchdy"))
    Output
      <digital object identifier[1]>
      [1] 10.13003/5jchdy

# dois fields are recycled

    Code
      doi("10.1371", c("journal.pbio.0020449", "journal.pbio.0020449.g001"))
    Output
      <digital object identifier[2]>
      [1] 10.1371/journal.pbio.0020449      10.1371/journal.pbio.0020449.g001

# dois can be coerced

    Code
      c(doi_examples()[1], "10.1002/0470841559.ch1")
    Output
      <digital object identifier[2]>
      [1] 10.1038/nphys1170      10.1002/0470841559.ch1

# dois can be cast to characters

    Code
      as.character(doi_examples()[1:3])
    Output
      [1] "10.1038/nphys1170"        NA                        
      [3] "10.11467/isss2003.7.1_11"

# characters can be cast to dois

    Code
      source(path_ex_file("doi", "as_doi.R"))$value
    Output
      <digital object identifier[2]>
      [1] 10.1594/PANGAE.726855          10.1594/GFZ.GEOFON.gfz2009kciu

# DOIs are printed and formatted

    Code
      format(doi_examples()[1:3])
    Output
      [1] "10.1038/nphys1170"        NA                        
      [3] "10.11467/isss2003.7.1_11"

---

    Code
      knitr::knit_print(doi_examples()[1:3])
    Output
      [1] "- [`https://doi.org/10.1038/nphys1170`](https://doi.org/10.1038/nphys1170)\n"              
      [2] "- `NA`\n"                                                                                  
      [3] "- [`https://doi.org/10.11467/isss2003.7.1_11`](https://doi.org/10.11467/isss2003.7.1_11)\n"
      attr(,"class")
      [1] "knit_asis"
      attr(,"knit_cacheable")
      [1] NA

---

    Code
      knitr::knit_print(doi_examples()[1:3], display = "doi")
    Output
      [1] "- [`doi:10.1038/nphys1170`](https://doi.org/10.1038/nphys1170)\n"              
      [2] "- `NA`\n"                                                                      
      [3] "- [`doi:10.11467/isss2003.7.1_11`](https://doi.org/10.11467/isss2003.7.1_11)\n"
      attr(,"class")
      [1] "knit_asis"
      attr(,"knit_cacheable")
      [1] NA

---

    Code
      knitr::knit_print(doi_examples()[1:3], inline = TRUE)
    Output
      [1] "[`https://doi.org/10.1038/nphys1170`](https://doi.org/10.1038/nphys1170), `NA` and [`https://doi.org/10.11467/isss2003.7.1_11`](https://doi.org/10.11467/isss2003.7.1_11)"
      attr(,"class")
      [1] "knit_asis"
      attr(,"knit_cacheable")
      [1] NA

# DOIs make pretty tibble columns

    # A tibble: 3 x 1
      `doi_examples()[1:3]`   
      <doi>                   
    1 10.1038/nphys1170       
    2 NA                      
    3 10.11467/isss2003.7.1_11

# doi with one NA field become all NA

    Code
      doi(c(NA, "10.13003"), c("5jchdy", "5jchdy"))
    Output
      <digital object identifier[2]>
      [1] <NA>            10.13003/5jchdy

# DOIs are extracted

    Code
      str_extract_all_doi(doimash)
    Output
           [,1]                     [,2]                    
      [1,] "10.13003/5jchdy"        "10.5281/zenodo.3892950"
      [2,] "10.5281/zenodo.3892951" "10.1109/5.771073"      

