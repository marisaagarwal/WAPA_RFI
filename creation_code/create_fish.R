# 2022-11-09

## 1. Set up

    # point to data locale
    data_locale = "data/"
    
    # point to data file
    data_file1 = "RFI_metadata.xlsx"
    data_file2 = "RFI_fish.xlsx"
    
    # call to data
    metadata = 
        paste0(data_locale, data_file1) %>%
        read_excel() %>%
        mutate(unit_transect = paste(Unit, "_", Transect))
    
    fishdata = 
        paste0(data_locale, data_file2) %>%
        read_excel(sheet = "Data") 
    
    fishcodes = 
        paste0(data_locale, data_file2) %>%
        read_excel(sheet = "Species Codes")


## 2. Data grooming


    
    
    
    