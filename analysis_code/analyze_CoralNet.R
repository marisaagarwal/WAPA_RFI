# 2022-12-06


## 1. Set up ----

    # point to data locale 1
    data_locale = "creation_code/"
    
    # load in the data 1
    source(paste0(data_locale, "create_CoralNet.R"))
    
    # point to data locale 2
    data_locale = "creation_code/"
    
    # load in the data 2
    source(paste0(data_locale, "create_coral.R"))
    

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
    
    detailed_func_cover %>%
        group_by(Site, Transect, Detailed_Func_Group) %>%
        dplyr::mutate(percent_cover = (count/n_annotations)*100) %>%
        filter(Detailed_Func_Group == "Hexacorallia")
    
    
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
    
    
## 5. Differences in coral cover by site ----
    
    # CoralNetannotations %>%
    #     group_by(Site, Transect, Functional_Group) %>%
    #     dplyr::summarise(count = n()) %>%
    #     ungroup() %>%
    #     group_by(Functional_Group) %>%
    #     nest() %>%
    #     mutate(t_test = map(data, 
    #                         ~t.test(count ~ Site, data = .x))) %>%
    #     mutate(tidy_test = map(t_test, tidy)) %>%
    #     unnest(tidy_test)
    
    # summary
    coral_cover = 
        CoralNetannotations %>%
            mutate(Site = as.factor(Site), 
                   Transect = as.factor(Transect), 
                   Functional_Group = as.factor(Functional_Group)) %>%
            group_by(Site, Transect, Functional_Group, .drop = F) %>%
            summarise(n_annotations = n()) %>%
            mutate(total_annotations = sum(n_annotations),
                   prop_cover = n_annotations/total_annotations, 
                   percent_cover = prop_cover*100) %>%
            filter(Functional_Group == "Coral") %>%
            drop_na(percent_cover) 
    
    coral_cover %>%
            group_by(Site) %>%
            summarise(mean_cover = mean(percent_cover),
                      se_cover = std.error(percent_cover))
    
    # stat test 
    coral_cover %>%
        ungroup() %>%
        wilcox_test(percent_cover ~ Site)
    
    coral_cover %>%
        ungroup() %>%
        wilcox_effsize(percent_cover ~ Site)
    

## 6. Differences in coral cover by benthic habitat ----
    
    coral_cover = merge(metadata, coral_cover %>% rename(Unit = "Site"))
    
    # summary
    coral_cover %>%
        group_by(Dominant_Benthic_Habitat_Type) %>%
        summarise(mean_cover = mean(percent_cover),
                  se_cover = std.error(percent_cover))
    
    # stat test 
    coral_cover %>%
        ungroup() %>%
        kruskal_test(percent_cover ~ Dominant_Benthic_Habitat_Type)
    
    coral_cover %>%
        ungroup() %>%
        kruskal_effsize(percent_cover ~ Dominant_Benthic_Habitat_Type)
    
    coral_cover %>%
        ungroup() %>%
        dunn_test(percent_cover ~ Dominant_Benthic_Habitat_Type) %>%
        filter(p.adj <= 0.05)
        
  
## 7. Differences in coral cover by substrate type  ----
    
    # summary
    coral_cover %>%
        group_by(Substrate_Characterization) %>%
        summarise(mean_cover = mean(percent_cover),
                  se_cover = std.error(percent_cover))
    
    # testing parametric assumptions --> not great
        # extreme outliers? one.
        coral_cover %>% 
            group_by(Substrate_Characterization) %>% 
            identify_outliers(percent_cover) %>%
            dplyr::select(c(Unit, Transect, Substrate_Characterization, percent_cover, is.extreme))
        # normal? no.
        coral_cover %>% 
            group_by(Substrate_Characterization) %>% 
            dplyr::filter(!Substrate_Characterization %in% c("AggregateReef", "RockBoulder")) %>%
            shapiro_test(percent_cover)
        ggqqplot(coral_cover, x = "percent_cover", facet.by = "Substrate_Characterization")
        # equal variances? yes. 
        coral_cover %>% levene_test(percent_cover ~ Substrate_Characterization)
    
    # perform non-parametric stat test 
    coral_cover %>%
        ungroup() %>%
        kruskal_test(percent_cover ~ Substrate_Characterization)
    
    coral_cover %>%
        ungroup() %>%
        kruskal_effsize(percent_cover ~ Substrate_Characterization)
    
    coral_cover %>%
        ungroup() %>%
        dunn_test(percent_cover ~ Substrate_Characterization) %>%
        filter(p.adj <= 0.05)
    
    
## 8. Difference in coral cover by distance from shore/crest/freshwater? 
    
    # to shore
    summary(lm(percent_cover ~ Shore_Dist, data = coral_cover))
    
    # to crest
    summary(lm(percent_cover ~ Crest_Dist, data = coral_cover))
    
    # to freshwater output
    summary(lm(percent_cover ~ Fresh_Dist, data = coral_cover))
    
    # multiple linear regression
    summary(lm(percent_cover ~ Shore_Dist + Fresh_Dist + Crest_Dist, data = coral_cover))   
    
    
    
    
    