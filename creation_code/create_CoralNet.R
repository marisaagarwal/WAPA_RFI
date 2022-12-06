# 2022-12-06


## 1. Set up

    # point to data locale
    data_locale = "data/"
    
    # point to data file
    data_file1 = "RFI_metadata.xlsx"
    data_file2 = "RFI_CoralNet_labelset.csv"
    data_file3 = "RFI_CoralNet_annotations.csv"
    
    # call to data
    metadata = 
        paste0(data_locale, data_file1) %>%
        read_excel() %>%
        mutate(unit_transect = paste(Unit, "_", Transect))
    
    CoralNetlabels = 
        paste0(data_locale, data_file2) %>%
        read.csv() 
    
    CoralNetannotations = 
        paste0(data_locale, data_file3) %>%
        read.csv() 
    
    
## 2. Data grooming
    
    CoralNetannotations %<>%
        dplyr::select(c(Date, Site, Transect, Label)) %>%
        dplyr::rename(Species_Code = "Label")
    
    CoralNetannotations = merge(CoralNetlabels, CoralNetannotations)
    
    
    
    