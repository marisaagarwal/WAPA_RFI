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
            dplyr::summarise(n_annotations = n()) %>%
            ungroup() %>%
            group_by(Site) %>%
            mutate(total_annotations = sum(n_annotations), 
                   prop_annotations = n_annotations / total_annotations, 
                   percent_annotations = prop_annotations * 100)
         
    
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
    
    
## 8. Algae & seagrass  ----
    
    # set up data 
    
    algae_species = c("AMPH", "BOOD", "BRCA", "CARA", "CAUL", "CHFR", "DICT", "HALI", "MALG", "NESP", "PADN", "SARG", "TURB", "VALO")
    seagrass_species = c("EACO", "HUNI", "HMIN")
    
    algae_seagrass_data = 
        merge(species_cover %>% rename(Unit = "Site"), metadata) %>%
            dplyr::select(c("Unit", "Transect", "n_annotations", "Species_Code", "count", "percent_cover", 
                            "Crest_Dist", "Shore_Dist", "Fresh_Dist", "Substrate_Characterization", "Dominant_Benthic_Habitat_Type")) %>%
            filter(Species_Code %in% c(algae_species, seagrass_species))
    
    algae_seagrass_data %>%
        group_by(Unit, Species_Code) %>%
        summarise(sum_species = sum(count)) %>%
        filter(Species_Code %in% algae_species)
    
    algae_seagrass_data %>%
        group_by(Unit, Species_Code) %>%
        summarise(sum_species = sum(count)) %>%
        filter(Species_Code %in% seagrass_species)
    
    algae_seagrass_data = merge(algae_seagrass_data, CoralNetlabels)
    
    CoralNetannotations %>%
        group_by(Site, Functional_Group) %>%
        dplyr::summarise(n_annotations = n()) %>%
        ungroup() %>%
        group_by(Site) %>%
        mutate(total_annotations = sum(n_annotations), 
               prop_annotations = n_annotations / total_annotations, 
               percent_annotations = prop_annotations * 100) %>%
        filter(Functional_Group == "Seagrass")
    

    # SEAGRASS percent cover
    
        # summary
        algae_seagrass_data %>%
            group_by(Unit, Transect, Functional_Group, n_annotations) %>%
            summarise(cover = sum(count)) %>%
            mutate(percent_cover = (cover / n_annotations) * 100) %>%
            filter(Functional_Group == "Seagrass") %>%
            group_by(Unit) %>%
            summarise(mean_cover = mean(percent_cover), 
                      se_cover = std.error(percent_cover))
        
        algae_seagrass_data %>%
            group_by(Unit, Transect, Functional_Group, Taxon_Name) %>%
            summarise(sum_species = sum(count)) %>%
            filter(Functional_Group == "Seagrass") %>%
            ungroup() %>%
            mutate(total_seagrass_obs = sum(sum_species)) %>%
            group_by(Taxon_Name, total_seagrass_obs) %>%
            summarise(sum_species  = sum(sum_species)) %>%
            mutate(prop_species = sum_species / total_seagrass_obs, 
                   percent_species = prop_species * 100)
        
        # by unit
    
            # testing assumptions
                # extreme outliers? 4 at Agat. 
                algae_seagrass_data %>%
                    group_by(Unit, Transect, Functional_Group, n_annotations) %>%
                    summarise(cover = sum(count)) %>%
                    mutate(percent_cover = (cover / n_annotations) * 100) %>%
                    filter(Functional_Group == "Seagrass") %>%
                    ungroup() %>%
                    group_by(Unit) %>%
                    identify_outliers(percent_cover)
                # normal? not at Agat. 
                algae_seagrass_data %>%
                    group_by(Unit, Transect, Functional_Group, n_annotations) %>%
                    summarise(cover = sum(count)) %>%
                    mutate(percent_cover = (cover / n_annotations) * 100) %>%
                    filter(Functional_Group == "Seagrass") %>%
                    ungroup() %>%
                    group_by(Unit) %>% 
                    shapiro_test(percent_cover)
                ggqqplot(algae_seagrass_data %>%
                             group_by(Unit, Transect, Functional_Group, n_annotations) %>%
                             summarise(cover = sum(count)) %>%
                             mutate(percent_cover = (cover / n_annotations) * 100) %>%
                             filter(Functional_Group == "Seagrass") %>%
                             ungroup(),
                         x = "percent_cover", facet.by = "Unit")
                # equal variances? yes. 
                algae_seagrass_data %>%
                    group_by(Unit, Transect, Functional_Group, n_annotations) %>%
                    summarise(cover = sum(count)) %>%
                    mutate(percent_cover = (cover / n_annotations) * 100) %>%
                    filter(Functional_Group == "Seagrass") %>%
                    ungroup() %>%
                    levene_test(percent_cover ~ Unit)
    
            # perform non-parametric test
            algae_seagrass_data %>%
                group_by(Unit, Transect, Functional_Group, n_annotations) %>%
                summarise(cover = sum(count)) %>%
                mutate(percent_cover = (cover / n_annotations) * 100) %>%
                filter(Functional_Group == "Seagrass") %>%
                ungroup() %>%
                wilcox_test(percent_cover ~ Unit)
            algae_seagrass_data %>%
                group_by(Unit, Transect, Functional_Group, n_annotations) %>%
                summarise(cover = sum(count)) %>%
                mutate(percent_cover = (cover / n_annotations) * 100) %>%
                filter(Functional_Group == "Seagrass") %>%
                ungroup() %>%
                wilcox_effsize(percent_cover ~ Unit)
            
        # by substrate type
            
            # testing assumptions --> NOT MET
                # extreme outliers? none. 
                algae_seagrass_data %>%
                    group_by(Unit, Transect, Functional_Group, Substrate_Characterization, n_annotations) %>%
                    summarise(cover = sum(count)) %>%
                    mutate(percent_cover = (cover / n_annotations) * 100) %>%
                    filter(Functional_Group == "Seagrass") %>%
                    ungroup() %>%
                    group_by(Substrate_Characterization) %>% 
                    identify_outliers(percent_cover)
                # normal? not for two groups 
                algae_seagrass_data %>%
                    group_by(Unit, Transect, Functional_Group, Substrate_Characterization, n_annotations) %>%
                    summarise(cover = sum(count)) %>%
                    mutate(percent_cover = (cover / n_annotations) * 100) %>%
                    filter(Functional_Group == "Seagrass") %>%
                    ungroup() %>%
                    filter(!Substrate_Characterization %in% c("AggregateReef", "Pavement", "SandScatteredCoral")) %>%
                    group_by(Substrate_Characterization) %>% 
                    shapiro_test(percent_cover)
                ggqqplot(algae_seagrass_data %>%
                             group_by(Unit, Transect, Functional_Group, Substrate_Characterization, n_annotations) %>%
                             summarise(cover = sum(count)) %>%
                             mutate(percent_cover = (cover / n_annotations) * 100) %>%
                             filter(Functional_Group == "Seagrass") %>%
                             ungroup(),
                         x = "percent_cover", facet.by = "Substrate_Characterization")
                # equal variances? no.  
                algae_seagrass_data %>%
                    group_by(Unit, Transect, Functional_Group, Substrate_Characterization, n_annotations) %>%
                    summarise(cover = sum(count)) %>%
                    mutate(percent_cover = (cover / n_annotations) * 100) %>%
                    filter(Functional_Group == "Seagrass") %>%
                    ungroup() %>%
                    levene_test(percent_cover ~ Substrate_Characterization)
                
            # perform non-parametric test
            algae_seagrass_data %>%
                group_by(Unit, Transect, Functional_Group, Substrate_Characterization, n_annotations) %>%
                summarise(cover = sum(count)) %>%
                mutate(percent_cover = (cover / n_annotations) * 100) %>%                    filter(Functional_Group == "Seagrass") %>%
                ungroup() %>% 
                kruskal_test(percent_cover ~ Substrate_Characterization)
            algae_seagrass_data %>%
                group_by(Unit, Transect, Functional_Group, Substrate_Characterization, n_annotations) %>%
                summarise(cover = sum(count)) %>%
                mutate(percent_cover = (cover / n_annotations) * 100) %>%                    filter(Functional_Group == "Seagrass") %>%
                ungroup() %>%
                kruskal_effsize(percent_cover ~ Substrate_Characterization)
                
        # by dominant benthic habitat
                
            # testing assumptions --> NOT MET
                # extreme outliers? 3.  
                algae_seagrass_data %>%
                    group_by(Unit, Transect, Functional_Group, Dominant_Benthic_Habitat_Type, n_annotations) %>%
                    summarise(cover = sum(count)) %>%
                    mutate(percent_cover = (cover / n_annotations) * 100) %>%                    filter(Functional_Group == "Seagrass") %>%
                    ungroup() %>%
                    group_by(Dominant_Benthic_Habitat_Type) %>% 
                    identify_outliers(percent_cover)
                # normal? not for one group. 
                algae_seagrass_data %>%
                    group_by(Unit, Transect, Functional_Group, Dominant_Benthic_Habitat_Type, n_annotations) %>%
                    summarise(cover = sum(count)) %>%
                    mutate(percent_cover = (cover / n_annotations) * 100) %>%                    filter(Functional_Group == "Seagrass") %>%
                    ungroup() %>%
                    group_by(Dominant_Benthic_Habitat_Type) %>% 
                    shapiro_test(percent_cover)
                ggqqplot(algae_seagrass_data %>%
                             group_by(Unit, Transect, Functional_Group, Dominant_Benthic_Habitat_Type, n_annotations) %>%
                             summarise(cover = sum(count)) %>%
                             mutate(percent_cover = (cover / n_annotations) * 100) %>%                    filter(Functional_Group == "Seagrass") %>%
                             ungroup(),
                         x = "percent_cover", facet.by = "Dominant_Benthic_Habitat_Type")
                # equal variances? no.  
                algae_seagrass_data %>%
                    group_by(Unit, Transect, Functional_Group, Dominant_Benthic_Habitat_Type, n_annotations) %>%
                    summarise(cover = sum(count)) %>%
                    mutate(percent_cover = (cover / n_annotations) * 100) %>%                    filter(Functional_Group == "Seagrass") %>%
                    ungroup() %>%
                    levene_test(percent_cover ~ Dominant_Benthic_Habitat_Type)
                
            # perform non-parametric test
            algae_seagrass_data %>%
                group_by(Unit, Transect, Functional_Group, Dominant_Benthic_Habitat_Type, n_annotations) %>%
                summarise(cover = sum(count)) %>%
                mutate(percent_cover = (cover / n_annotations) * 100) %>%
                filter(Functional_Group == "Seagrass") %>%
                ungroup() %>% 
                kruskal_test(percent_cover ~ Dominant_Benthic_Habitat_Type)
            algae_seagrass_data %>%
                group_by(Unit, Transect, Functional_Group, Dominant_Benthic_Habitat_Type, n_annotations) %>%
                summarise(cover = sum(count)) %>%
                mutate(percent_cover = (cover / n_annotations) * 100) %>%
                filter(Functional_Group == "Seagrass") %>%
                ungroup() %>%
                kruskal_effsize(percent_cover ~ Dominant_Benthic_Habitat_Type)
      
        # by distance from freshwater/shore/reef crest
        summary(lm(percent_cover ~ Shore_Dist + Fresh_Dist + Crest_Dist, 
                   data = algae_seagrass_data %>%
                           filter(Functional_Group == "Seagrass") %>%
                           group_by(Unit, Transect, Functional_Group, Crest_Dist, Shore_Dist, Fresh_Dist, n_annotations) %>%
                           summarise(n_points = sum(count)) %>%
                           mutate(percent_cover = (n_points / n_annotations) * 100)))    
            
    # ALGAE OVERALL percent cover
                
        # summary 
        algae_seagrass_data %>%
            group_by(Unit, Transect, Functional_Group, n_annotations) %>%
            summarise(cover = sum(count)) %>%
            mutate(percent_cover = (cover / n_annotations) * 100) %>%
            filter(Functional_Group == "Algae") %>%
            group_by(Unit, Functional_Group) %>%
            summarise(mean_cover = mean(percent_cover), 
                      se_cover = std.error(percent_cover))
                
        algae_seagrass_data %>%
                group_by(Unit, Transect, Functional_Group, Taxon_Name) %>%
                summarise(sum_species = sum(count)) %>%
                filter(Functional_Group == "Algae") %>%
                ungroup() %>%
                mutate(total_algae_obs = sum(sum_species)) %>%
                group_by(Taxon_Name, total_algae_obs) %>%
                summarise(sum_species  = sum(sum_species)) %>%
                mutate(prop_species = sum_species / total_algae_obs, 
                       percent_species = prop_species * 100)
                
        # by unit
                
            # testing assumptions
                # extreme outliers? none. 
                algae_seagrass_data %>%
                    group_by(Unit, Transect, Functional_Group, n_annotations) %>%
                    summarise(cover = sum(count)) %>%
                    mutate(percent_cover = (cover / n_annotations) * 100) %>%
                    filter(Functional_Group == "Algae") %>%
                    ungroup() %>%
                    group_by(Unit) %>%
                    identify_outliers(percent_cover)
                # normal? not at asan. 
                algae_seagrass_data %>%
                    group_by(Unit, Transect, Functional_Group, n_annotations) %>%
                    summarise(cover = sum(count)) %>%
                    mutate(percent_cover = (cover / n_annotations) * 100) %>%
                    filter(Functional_Group == "Algae") %>%
                    ungroup() %>%
                    group_by(Unit) %>% 
                    shapiro_test(percent_cover)
                ggqqplot(algae_seagrass_data %>%
                             group_by(Unit, Transect, Functional_Group, n_annotations) %>%
                             summarise(cover = sum(count)) %>%
                             mutate(percent_cover = (cover / n_annotations) * 100) %>%
                             filter(Functional_Group == "Algae") %>%
                             ungroup(),
                         x = "percent_cover", facet.by = "Unit")
                # equal variances? yes. 
                algae_seagrass_data %>%
                    group_by(Unit, Transect, Functional_Group, n_annotations) %>%
                    summarise(cover = sum(count)) %>%
                    mutate(percent_cover = (cover / n_annotations) * 100) %>%
                    filter(Functional_Group == "Algae") %>%
                    ungroup() %>%
                    levene_test(percent_cover ~ Unit)
                
            # perform non-parametric test
            algae_seagrass_data %>%
                group_by(Unit, Transect, Functional_Group, n_annotations) %>%
                summarise(cover = sum(count)) %>%
                mutate(percent_cover = (cover / n_annotations) * 100) %>%
                filter(Functional_Group == "Algae") %>%
                ungroup() %>%
                wilcox_test(percent_cover ~ Unit)
            algae_seagrass_data %>%
                group_by(Unit, Transect, Functional_Group, n_annotations) %>%
                summarise(cover = sum(count)) %>%
                mutate(percent_cover = (cover / n_annotations) * 100) %>%
                filter(Functional_Group == "Algae") %>%
                ungroup() %>%
                wilcox_effsize(percent_cover ~ Unit)
                
        # by substrate type
                
            # testing assumptions
                # extreme outliers? 2 
                algae_seagrass_data %>%
                    group_by(Unit, Transect, Functional_Group, Substrate_Characterization, n_annotations) %>%
                    summarise(cover = sum(count)) %>%
                    mutate(percent_cover = (cover / n_annotations) * 100) %>%
                    filter(Functional_Group == "Algae") %>%
                    ungroup() %>%
                    group_by(Substrate_Characterization) %>% 
                    identify_outliers(percent_cover)
                # normal? not for one group 
                algae_seagrass_data %>%
                    group_by(Unit, Transect, Functional_Group, Substrate_Characterization, n_annotations) %>%
                    summarise(cover = sum(count)) %>%
                    mutate(percent_cover = (cover / n_annotations) * 100) %>%
                    filter(Functional_Group == "Algae") %>%
                    ungroup() %>%
                    filter(!Substrate_Characterization %in% c("AggregateReef", "RockBoulder")) %>%
                    group_by(Substrate_Characterization) %>% 
                    shapiro_test(percent_cover)
                ggqqplot(algae_seagrass_data %>%
                             group_by(Unit, Transect, Functional_Group, Substrate_Characterization, n_annotations) %>%
                             summarise(cover = sum(count)) %>%
                             mutate(percent_cover = (cover / n_annotations) * 100) %>%
                             filter(Functional_Group == "Algae") %>%
                             ungroup(),
                         x = "percent_cover", facet.by = "Substrate_Characterization")
                # equal variances? yes.  
                algae_seagrass_data %>%
                    group_by(Unit, Transect, Functional_Group, Substrate_Characterization, n_annotations) %>%
                    summarise(cover = sum(count)) %>%
                    mutate(percent_cover = (cover / n_annotations) * 100) %>%
                    filter(Functional_Group == "Algae") %>%
                    ungroup() %>%
                    levene_test(percent_cover ~ Substrate_Characterization)
                
            # perform non-parametric test
            algae_seagrass_data %>%
                group_by(Unit, Transect, Functional_Group, Substrate_Characterization, n_annotations) %>%
                summarise(cover = sum(count)) %>%
                mutate(percent_cover = (cover / n_annotations) * 100) %>%                    
                filter(Functional_Group == "Algae") %>%
                ungroup() %>% 
                kruskal_test(percent_cover ~ Substrate_Characterization)
            algae_seagrass_data %>%
                group_by(Unit, Transect, Functional_Group, Substrate_Characterization, n_annotations) %>%
                summarise(cover = sum(count)) %>%
                mutate(percent_cover = (cover / n_annotations) * 100) %>%                    
                filter(Functional_Group == "Algae") %>%
                ungroup() %>%
                kruskal_effsize(percent_cover ~ Substrate_Characterization)
                
        # by dominant benthic habitat
                
            # testing assumptions --> NOT MET
                # extreme outliers? 1.  
                algae_seagrass_data %>%
                    group_by(Unit, Transect, Functional_Group, Dominant_Benthic_Habitat_Type, n_annotations) %>%
                    summarise(cover = sum(count)) %>%
                    mutate(percent_cover = (cover / n_annotations) * 100) %>%        
                    filter(Functional_Group == "Algae") %>%
                    ungroup() %>%
                    group_by(Dominant_Benthic_Habitat_Type) %>% 
                    identify_outliers(percent_cover)
                # normal? not for one group. 
                algae_seagrass_data %>%
                    group_by(Unit, Transect, Functional_Group, Dominant_Benthic_Habitat_Type, n_annotations) %>%
                    summarise(cover = sum(count)) %>%
                    mutate(percent_cover = (cover / n_annotations) * 100) %>%       
                    filter(Functional_Group == "Algae") %>%
                    ungroup() %>%
                    group_by(Dominant_Benthic_Habitat_Type) %>% 
                    shapiro_test(percent_cover)
                ggqqplot(algae_seagrass_data %>%
                             group_by(Unit, Transect, Functional_Group, Dominant_Benthic_Habitat_Type, n_annotations) %>%
                             summarise(cover = sum(count)) %>%
                             mutate(percent_cover = (cover / n_annotations) * 100) %>%     
                             filter(Functional_Group == "Algae") %>%
                             ungroup(),
                         x = "percent_cover", facet.by = "Dominant_Benthic_Habitat_Type")
                # equal variances? yes.  
                algae_seagrass_data %>%
                    group_by(Unit, Transect, Functional_Group, Dominant_Benthic_Habitat_Type, n_annotations) %>%
                    summarise(cover = sum(count)) %>%
                    mutate(percent_cover = (cover / n_annotations) * 100) %>%    
                    filter(Functional_Group == "Algae") %>%
                    ungroup() %>%
                    levene_test(percent_cover ~ Dominant_Benthic_Habitat_Type)
                
            # perform non-parametric test
            algae_seagrass_data %>%
                group_by(Unit, Transect, Functional_Group, Dominant_Benthic_Habitat_Type, n_annotations) %>%
                summarise(cover = sum(count)) %>%
                mutate(percent_cover = (cover / n_annotations) * 100) %>%
                filter(Functional_Group == "Algae") %>%
                ungroup() %>% 
                kruskal_test(percent_cover ~ Dominant_Benthic_Habitat_Type)
            
            algae_seagrass_data %>%
                group_by(Unit, Transect, Functional_Group, Dominant_Benthic_Habitat_Type, n_annotations) %>%
                summarise(cover = sum(count)) %>%
                mutate(percent_cover = (cover / n_annotations) * 100) %>%
                filter(Functional_Group == "Algae") %>%
                ungroup() %>%
                kruskal_effsize(percent_cover ~ Dominant_Benthic_Habitat_Type)
                

    # ALGAE FUNCTIONAL GROUP percent cover
            
        # summary 
        algae_seagrass_data %>%
            filter(Functional_Group == "Algae") %>%
            group_by(Unit, Transect, Detailed_Func_Group, n_annotations) %>%
            summarise(n_points = sum(count)) %>%
            mutate(percent_cover = (n_points / n_annotations) * 100) %>%
            group_by(Unit, Detailed_Func_Group) %>%
            summarise(mean_cover = mean(percent_cover), 
                      se_cover = std.error(percent_cover))
    
        algae_seagrass_data %>%
            group_by(Unit, Transect, Functional_Group, Detailed_Func_Group) %>%
            summarise(sum_species = sum(count)) %>%
            filter(Functional_Group == "Algae") %>%
            ungroup() %>%
            mutate(total_algae_obs = sum(sum_species)) %>%
            group_by(Detailed_Func_Group, total_algae_obs) %>%
            summarise(sum_species  = sum(sum_species)) %>%
            mutate(prop_species = sum_species / total_algae_obs, 
                   percent_species = prop_species * 100)        
           
        # within each unit
        algae_seagrass_data %>%
            filter(Functional_Group == "Algae") %>%
            group_by(Unit, Transect, Detailed_Func_Group, n_annotations) %>%
            summarise(n_points = sum(count)) %>%
            group_by(Unit) %>%
            nest() %>%
            mutate(kruskal_test = map(data, 
                                      ~kruskal.test(n_points ~ Detailed_Func_Group, data = .x)),
                   kruskal_effsize = map(data, 
                                         ~kruskal_effsize(n_points ~ Detailed_Func_Group, data = .x))) %>%
            mutate(tidy_kruskal = map(kruskal_test, tidy)) %>%
            # unnest(tidy_kruskal)
            mutate(dunn_test = map(data, 
                                   ~dunn_test(n_points ~ Detailed_Func_Group, data = .x)))
            
        # between units
        algae_seagrass_data %>%
            filter(Functional_Group == "Algae") %>%
            group_by(Unit, Transect, Detailed_Func_Group, n_annotations) %>%
            summarise(n_points = sum(count)) %>%
            mutate(percent_cover = (n_points / n_annotations) * 100) %>%
            filter(!Detailed_Func_Group == "Golden Algae") %>%
            ungroup() %>%
            group_by(Detailed_Func_Group) %>%
            nest() %>%
            mutate(wilcox_test = map(data, 
                                     ~wilcox_test(percent_cover ~ Unit, data = .x)),
                   wilcox_effsize = map(data, 
                                        ~wilcox_effsize(percent_cover ~ Unit, data = .x)))  

        # by substrate type
        
            # golden algae  
            golden_substrate = 
                algae_seagrass_data %>%
                filter(Functional_Group == "Algae") %>%
                group_by(Substrate_Characterization, Detailed_Func_Group) %>%
                summarise(n_points = sum(count)) %>%
                ungroup() %>%
                group_by(Substrate_Characterization) %>%
                mutate(total_points = sum(n_points),
                       n_nonpoints = total_points - n_points) %>%
                ungroup() %>%
                filter(Detailed_Func_Group == "Golden Algae") %>%
                dplyr::select(c(n_points, n_nonpoints))
            
            fisher.test(as.data.table(golden_substrate), simulate.p.value = T)
            pairwise_fisher_test(as.data.table(golden_substrate), simulate.p.value = T)
        
            # red algae  
            red_substrate = 
                algae_seagrass_data %>%
                filter(Functional_Group == "Algae") %>%
                group_by(Substrate_Characterization, Detailed_Func_Group) %>%
                summarise(n_points = sum(count)) %>%
                ungroup() %>%
                group_by(Substrate_Characterization) %>%
                mutate(total_points = sum(n_points),
                       n_nonpoints = total_points - n_points) %>%
                ungroup() %>%
                filter(Detailed_Func_Group == "Red Algae") %>%
                dplyr::select(c(n_points, n_nonpoints))
            
            fisher.test(as.data.table(red_substrate), simulate.p.value = T)
            pairwise_fisher_test(as.data.table(red_substrate), simulate.p.value = T)
            
            # green algae  
            green_substrate = 
                algae_seagrass_data %>%
                filter(Functional_Group == "Algae") %>%
                group_by(Substrate_Characterization, Detailed_Func_Group) %>%
                summarise(n_points = sum(count)) %>%
                ungroup() %>%
                group_by(Substrate_Characterization) %>%
                mutate(total_points = sum(n_points),
                       n_nonpoints = total_points - n_points) %>%
                ungroup() %>%
                filter(Detailed_Func_Group == "Green Algae") %>%
                dplyr::select(c(n_points, n_nonpoints))
            
            fisher.test(as.data.table(green_substrate), simulate.p.value = T)
            pairwise_fisher_test(as.data.table(green_substrate), simulate.p.value = T)
            
            # brown algae  
            brown_substrate = 
                algae_seagrass_data %>%
                filter(Functional_Group == "Algae") %>%
                group_by(Substrate_Characterization, Detailed_Func_Group) %>%
                summarise(n_points = sum(count)) %>%
                ungroup() %>%
                group_by(Substrate_Characterization) %>%
                mutate(total_points = sum(n_points),
                       n_nonpoints = total_points - n_points) %>%
                ungroup() %>%
                filter(Detailed_Func_Group == "Brown Algae") %>%
                dplyr::select(c(n_points, n_nonpoints))
            
            fisher.test(as.data.table(brown_substrate), simulate.p.value = T)
            pairwise_fisher_test(as.data.table(brown_substrate), simulate.p.value = T)
            
        # by dominant benthic habitat
            
            # golden algae  
            golden_benthic = 
                algae_seagrass_data %>%
                filter(Functional_Group == "Algae") %>%
                group_by(Dominant_Benthic_Habitat_Type, Detailed_Func_Group) %>%
                summarise(n_points = sum(count)) %>%
                ungroup() %>%
                group_by(Dominant_Benthic_Habitat_Type) %>%
                mutate(total_points = sum(n_points),
                       n_nonpoints = total_points - n_points) %>%
                ungroup() %>%
                filter(Detailed_Func_Group == "Golden Algae") %>%
                dplyr::select(c(n_points, n_nonpoints))
            
            fisher.test(as.data.table(golden_benthic), simulate.p.value = T)
            pairwise_fisher_test(as.data.table(golden_benthic), simulate.p.value = T)
            
            # red algae  
            red_benthic = 
                algae_seagrass_data %>%
                filter(Functional_Group == "Algae") %>%
                group_by(Dominant_Benthic_Habitat_Type, Detailed_Func_Group) %>%
                summarise(n_points = sum(count)) %>%
                ungroup() %>%
                group_by(Dominant_Benthic_Habitat_Type) %>%
                mutate(total_points = sum(n_points),
                       n_nonpoints = total_points - n_points) %>%
                ungroup() %>%
                filter(Detailed_Func_Group == "Red Algae") %>%
                dplyr::select(c(n_points, n_nonpoints))
            
            fisher.test(as.data.table(red_benthic), simulate.p.value = T)
            pairwise_fisher_test(as.data.table(red_benthic), simulate.p.value = T)
            
            # green algae  
            green_benthic = 
                algae_seagrass_data %>%
                filter(Functional_Group == "Algae") %>%
                group_by(Dominant_Benthic_Habitat_Type, Detailed_Func_Group) %>%
                summarise(n_points = sum(count)) %>%
                ungroup() %>%
                group_by(Dominant_Benthic_Habitat_Type) %>%
                mutate(total_points = sum(n_points),
                       n_nonpoints = total_points - n_points) %>%
                ungroup() %>%
                filter(Detailed_Func_Group == "Green Algae") %>%
                dplyr::select(c(n_points, n_nonpoints))
            
            fisher.test(as.data.table(green_benthic), simulate.p.value = T)
            pairwise_fisher_test(as.data.table(green_benthic), simulate.p.value = T)
            
            # brown algae  
            brown_benthic = 
                algae_seagrass_data %>%
                filter(Functional_Group == "Algae") %>%
                group_by(Dominant_Benthic_Habitat_Type, Detailed_Func_Group) %>%
                summarise(n_points = sum(count)) %>%
                ungroup() %>%
                group_by(Dominant_Benthic_Habitat_Type) %>%
                mutate(total_points = sum(n_points),
                       n_nonpoints = total_points - n_points) %>%
                ungroup() %>%
                filter(Detailed_Func_Group == "Brown Algae") %>%
                dplyr::select(c(n_points, n_nonpoints))
            
            fisher.test(as.data.table(brown_benthic), simulate.p.value = T)
            pairwise_fisher_test(as.data.table(brown_benthic), simulate.p.value = T)
            
        # with distance from shore/freshwater/crest
        algae_seagrass_data %>%
            filter(Functional_Group == "Algae") %>%
            group_by(Unit, Transect, Crest_Dist, Shore_Dist, Fresh_Dist, n_annotations) %>%
            summarise(n_points = sum(count)) %>%
            mutate(percent_cover = (n_points / n_annotations) * 100)
            
            # overall
            summary(lm(percent_cover ~ Shore_Dist + Fresh_Dist + Crest_Dist, 
                       data = algae_seagrass_data %>%
                           filter(Functional_Group == "Algae") %>%
                           group_by(Unit, Transect, Crest_Dist, Shore_Dist, Fresh_Dist, n_annotations) %>%
                           summarise(n_points = sum(count)) %>%
                           mutate(percent_cover = (n_points / n_annotations) * 100)))
            
            # for golden algae
            summary(lm(percent_cover ~ Shore_Dist + Fresh_Dist + Crest_Dist, 
                       data = algae_seagrass_data %>%
                               filter(Functional_Group == "Algae") %>%
                               group_by(Unit, Transect, Detailed_Func_Group, Crest_Dist, Shore_Dist, Fresh_Dist, n_annotations) %>%
                               summarise(n_points = sum(count)) %>%
                               mutate(percent_cover = (n_points / n_annotations) * 100)  %>%
                               filter(!Detailed_Func_Group == "Golden Algae")))
            
            # for green algae
            summary(lm(percent_cover ~ Shore_Dist + Fresh_Dist + Crest_Dist, 
                       data = algae_seagrass_data %>%
                           filter(Functional_Group == "Algae") %>%
                           group_by(Unit, Transect, Detailed_Func_Group, Crest_Dist, Shore_Dist, Fresh_Dist, n_annotations) %>%
                           summarise(n_points = sum(count)) %>%
                           mutate(percent_cover = (n_points / n_annotations) * 100)  %>%
                           filter(!Detailed_Func_Group == "Green Algae")))
            
            # for red algae
            summary(lm(percent_cover ~ Shore_Dist + Fresh_Dist + Crest_Dist, 
                       data = algae_seagrass_data %>%
                           filter(Functional_Group == "Algae") %>%
                           group_by(Unit, Transect, Detailed_Func_Group, Crest_Dist, Shore_Dist, Fresh_Dist, n_annotations) %>%
                           summarise(n_points = sum(count)) %>%
                           mutate(percent_cover = (n_points / n_annotations) * 100)  %>%
                           filter(!Detailed_Func_Group == "Red Algae")))
            
            # for brown algae
            summary(lm(percent_cover ~ Shore_Dist + Fresh_Dist + Crest_Dist, 
                       data = algae_seagrass_data %>%
                           filter(Functional_Group == "Algae") %>%
                           group_by(Unit, Transect, Detailed_Func_Group, Crest_Dist, Shore_Dist, Fresh_Dist, n_annotations) %>%
                           summarise(n_points = sum(count)) %>%
                           mutate(percent_cover = (n_points / n_annotations) * 100)  %>%
                           filter(!Detailed_Func_Group == "Brown Algae")))
            
            