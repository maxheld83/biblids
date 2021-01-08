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

# DOIs are extracted

    Code
      str_extract_all_doi(doimash)
    Output
           [,1]                     [,2]                    
      [1,] "10.13003/5jchdy"        "10.5281/zenodo.3892950"
      [2,] "10.5281/zenodo.3892951" "10.1109/5.771073"      

