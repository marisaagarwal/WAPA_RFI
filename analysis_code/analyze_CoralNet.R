# 2022-12-06


## 1. Set up ----

    # point to data locale
    data_locale = "creation_code/"
    
    # load in the data
    source(paste0(data_locale, "create_CoralNet.R"))
    

## 2. Percent Cover (all labels) ----
    
    annotations_per_transect = 
        CoralNetannotations %>%
            group_by(Site, Transect) %>%
            dplyr::summarise(n_annotations = n())
    
    annotations_per_site = 
        CoralNetannotations %>%
            group_by(Site) %>%
            dplyr::summarise(n_annotations = n())
    
    # by species
    species_cover = 
        merge(annotations_per_transect, CoralNetannotations %>%
                                          group_by(Site, Transect, Species_Code) %>%
                                          dplyr::summarise(count = n()))
    
    species_cover %<>%
        group_by(Site, Transect, Species_Code) %>%
        dplyr::mutate(percent_cover = (count/n_annotations)*100)
    
    merge(annotations_per_site, CoralNetannotations %>%
              group_by(Site, Species_Code) %>%
              dplyr::summarise(count = n())) %>%
        dplyr::mutate(percent_cover = (count/n_annotations)*100)
        
        
    # by detailed functional group
    detailed_func_cover = 
        merge(annotations_per_transect, CoralNetannotations %>%
                  group_by(Site, Transect, Detailed_Func_Group) %>%
                  dplyr::summarise(count = n()))
    
    detailed_func_cover %<>%
        group_by(Site, Transect, Detailed_Func_Group) %>%
        dplyr::mutate(percent_cover = (count/n_annotations)*100)
    
    
    # by broad functional group
    broad_func_cover = 
        merge(annotations_per_transect, CoralNetannotations %>%
                  group_by(Site, Transect, Functional_Group) %>%
                  dplyr::summarise(count = n()))
    
    broad_func_cover %<>%
        group_by(Site, Transect, Functional_Group) %>%
        dplyr::mutate(percent_cover = (count/n_annotations)*100)
    
    merge(annotations_per_site, CoralNetannotations %>%
                                  group_by(Site, Functional_Group) %>%
                                  dplyr::summarise(count = n())) %>%
        dplyr::mutate(percent_cover = (count/n_annotations)*100)
        
    

    
## 3. Most common labels by site ----
    
    common_species_labels = 
        CoralNetannotations %>%
            group_by(Site, Species_Code) %>%
            dplyr::summarise(n_annotations = n())
    
    common_detailed_func_groups = 
        CoralNetannotations %>%
            group_by(Site, Detailed_Func_Group) %>%
            dplyr::summarise(n_annotations = n())
    
    common_broad_func_groups = 
        CoralNetannotations %>%
            group_by(Site, Functional_Group) %>%
            dplyr::summarise(n_annotations = n())
         
    
    
    
    