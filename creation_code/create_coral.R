# 2022-11-07

## 1. Set up

    # point to data locale
    data_locale = "data/"
    
    # point to data file
    data_file1 = "RFI_metadata.xlsx"
    data_file2 = "RFI_coralNMDS.xlsx"

    # call to data
    metadata = 
        paste0(data_locale, data_file1) %>%
        read_excel() %>%
        mutate(unit_transect = paste(Unit, "_", Transect))
    
    coralNMDSdata = 
        paste0(data_locale, data_file2) %>%
        read_excel(sheet = "data") 
    
    coralNMDScodes = 
        paste0(data_locale, data_file2) %>%
        read_excel(sheet = "species_codes")
    

## 2. Data grooming
    
    coralNMDSdata %<>%
        dplyr::rename(Unit = "Site")

        
    
    