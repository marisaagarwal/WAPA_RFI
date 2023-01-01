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
    
    # ADD LEHA to fishcodes
    fishcodes %<>%
        add_row(Family = "Lethrinidae", Taxon_Name = "Lethrinus harak", Genus = "Lethrinus", Species = "harak",
                Species_Code = "LEHA", A_value = 0.0281, B_value = 2.89)
    
    
## 3. Create vegan/NMDS data
    
    fishNMDSdata =  
        fishdata %>% 
            dplyr::select(-c(Date, Total_Length, Notes)) %>%
            group_by(Unit, Transect, Species) %>%
            dplyr::summarise(count = length(Species)) %>%
            pivot_wider(names_from = "Species", values_from = "count", values_fill = 0)
            
            
            
        
        
        
        


    
    
    
    