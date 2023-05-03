# 2023-02-14 <3 

## 1. Set up

    # point to data locale
    data_locale = "data/"
    
    # point to data file
    data_file1 = "RFI_inverts.xlsx"
    data_file2 = "RFI_metadata.xlsx"
    
    # call to data
    invert_data = 
        paste0(data_locale, data_file1) %>%
        read_excel(sheet = "Data") %>%
        rename(Code = "Species")
    
    invert_meta = 
        paste0(data_locale, data_file1) %>%
        read_excel(sheet = "Species Codes")
    
    metadata = 
        paste0(data_locale, data_file2) %>%
        read_excel()


## 2. Groom data
    
    # merge species codes with species names
    invert_data = merge(invert_data,invert_meta) %>% dplyr::select(!Date)
    
    # merge invert data with metadata
    invert_data = 
        merge(invert_data, metadata) %>% 
        dplyr::select(!c(Year, Date, Depth, Time, Bearing, 
                         Invert_Surveyor, Fish_Surveyor, Coral_Algae_Surveyor, 
                         Weather, Notes))

    
