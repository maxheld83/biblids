# doi helper creates doi records

    <digital object identifier[4]>
    [1] 10.1038/nphys1170        10.1002/0470841559.ch1   NA                      
    [4] 10.11467/isss2003.7.1_11

---

    # A tibble: 4 x 1
      doi_examples            
      <doi>                   
    1 10.1038/nphys1170       
    2 10.1002/0470841559.ch1  
    3 NA                      
    4 10.11467/isss2003.7.1_11

# dois are cast from more constrained types

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

# doi with one NA field become all NA

    Code
      doi(c(NA, "10.13003"), c("5jchdy", "5jchdy"))
    Output
      <digital object identifier[2]>
      [1] NA              10.13003/5jchdy

# DOIs are printed and formatted

    Code
      as.character(dois_2)
    Output
      [1] "doi:10.1038/nphys1170"      "doi:10.1002/0470841559.ch1"

---

    Code
      format(dois_2)
    Output
      [1] "10.1038/nphys1170     " "10.1002/0470841559.ch1"

---

    Code
      knitr::knit_print(dois_2)
    Output
      [1] "- [`https://doi.org/10.1038/nphys1170`](https://doi.org/10.1038/nphys1170)\n"          
      [2] "- [`https://doi.org/10.1002/0470841559.ch1`](https://doi.org/10.1002/0470841559.ch1)\n"
      attr(,"class")
      [1] "knit_asis"
      attr(,"knit_cacheable")
      [1] NA

---

    Code
      knitr::knit_print(dois_2, display = "doi")
    Output
      [1] "- [`doi:10.1038/nphys1170`](https://doi.org/10.1038/nphys1170)\n"          
      [2] "- [`doi:10.1002/0470841559.ch1`](https://doi.org/10.1002/0470841559.ch1)\n"
      attr(,"class")
      [1] "knit_asis"
      attr(,"knit_cacheable")
      [1] NA

---

    Code
      knitr::knit_print(dois_2, inline = TRUE)
    Output
      [1] "[`https://doi.org/10.1038/nphys1170`](https://doi.org/10.1038/nphys1170) and [`https://doi.org/10.1002/0470841559.ch1`](https://doi.org/10.1002/0470841559.ch1)"
      attr(,"class")
      [1] "knit_asis"
      attr(,"knit_cacheable")
      [1] NA

# DOIs are extracted

    Code
      str_extract_all_doi(doimash)
    Output
           [,1]                     [,2]                    
      [1,] "10.13003/5jchdy"        "10.5281/zenodo.3892950"
      [2,] "10.5281/zenodo.3892951" "10.1109/5.771073"      

