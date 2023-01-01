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
    

    detailed_func_cover %>%
        group_by(Site, Detailed_Func_Group) %>%
        dplyr::summarise(mean_cover = mean(percent_cover), 
                         std_error_cover = std.error(percent_cover))
    
    
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
    
    # differences in functional group cover by site 
    CoralNetannotations %>%
        group_by(Site, Transect, Functional_Group) %>%
        dplyr::summarise(count = n()) %>%
        ungroup() %>%
        group_by(Functional_Group) %>%
        nest() %>%
            mutate(t_test = map(data, 
                                ~t.test(count ~ Site, data = .x))) %>%
            mutate(tidy_test = map(t_test, tidy)) %>%
            unnest(tidy_test)
         
    
## 4. Most common labels by dominant benthic habitat ----   
    
    common_labels_benthic = 
    merge(CoralNetannotations, metadata %>%
                                    dplyr::rename("Site" = "Unit") %>%
                                    dplyr::select(c(Site, Transect, Substrate_Characterization, Dominant_Benthic_Habitat_Type)))
    
    habitat_freqs = 
        common_labels_benthic %>%
            group_by(Dominant_Benthic_Habitat_Type) %>%
            dplyr::summarise(total_annotations = n())
        
    common_labels_benthic %<>%
        group_by(Dominant_Benthic_Habitat_Type, Functional_Group) %>%
        dplyr::summarise(n_annotations = n()) %>%
        full_join(habitat_freqs) %>%
        dplyr::mutate(percent_annotations = (n_annotations/total_annotations) * 100 )
    
    # differences in functional group cover by benthic habitat types 
    CoralNetannotations %>%
        full_join(metadata %>% 
                      dplyr::rename(Site = Unit) %>%
                      dplyr::select(c(Site, Transect, Dominant_Benthic_Habitat_Type))) %>%
        group_by(Dominant_Benthic_Habitat_Type, Transect, Functional_Group) %>%
        dplyr::summarise(count = n()) %>%
        ungroup() %>%
        group_by(Functional_Group) %>%
        nest() %>%
        mutate(anova_test = map(data, 
                                ~aov(count ~ Dominant_Benthic_Habitat_Type, data = .x)),
               tukey = map(data, 
                           ~ TukeyHSD(aov(count ~ Dominant_Benthic_Habitat_Type, data = .x), conf.level=.95))) %>%
        mutate(anova_test = map(anova_test, tidy), 
               tukey = map(tukey, tidy)) 
        # %>% unnest(anova_test)
    
    
    
    