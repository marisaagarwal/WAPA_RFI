# 2023-02-14 <3 

## 1. Set up ----

    # point to data locale
    data_locale = "creation_code/"
    
    # load in the data
    source(paste0(data_locale, "create_inverts.R"))


## 2. Summary items ----
    
    # sea cucumbers 
        # number of species
        invert_data %>%
            filter(Group == "Sea Cucumbers") %>%
            summarise(n_cucumber_sp = length(unique(Code)))
        # species present
        invert_data %>%
            filter(Group == "Sea Cucumbers") %>% 
            distinct(Species)
        # which species were most common?
        invert_data %>%
            filter(Group == "Sea Cucumbers") %>% 
            group_by(Species) %>%
            summarise(n_cucumbers_found = n())
        # how many transects had cucumbers
        invert_data %>%
            filter(Group == "Sea Cucumbers") %>%
            group_by(Unit, Transect) %>%
            summarise(sum_cucumbers = n()) %>%
            ungroup() %>%
            group_by(Unit) %>%
            summarise(sum_transects_with_cucumbers = n())
        # total number of cucumbers, prop of community that each cucumber sp occupies
        invert_data %>%
            filter(Group == "Sea Cucumbers") %>% 
            group_by(Species) %>%
            summarise(n_cucumbers_found = n()) %>%
            mutate(total_cucumbers = sum(n_cucumbers_found), 
                   prop_cucumber_community = n_cucumbers_found / total_cucumbers, 
                   percent_cucumber_community = prop_cucumber_community * 100) 
        
    # sea stars 
        # number of species
        invert_data %>%
            filter(Group == "Sea Stars") %>%
            summarise(n_cucumber_sp = length(unique(Code)))
        # species present
        invert_data %>%
            filter(Group == "Sea Stars") %>% 
            distinct(Species)
        # which species were most common?
        invert_data %>%
            filter(Group == "Sea Stars") %>% 
            group_by(Species) %>%
            summarise(n_cucumbers_found = n())
        # how many transects had stars?
        invert_data %>%
            filter(Group == "Sea Stars") %>%
            group_by(Unit, Transect) %>%
            summarise(sum_cucumbers = n()) %>%
            ungroup() %>%
            group_by(Unit) %>%
            summarise(sum_transects_with_cucumbers = n())
        # total number of stars, prop of community that each star sp occupies
        invert_data %>%
            filter(Group == "Sea Stars") %>% 
            group_by(Species) %>%
            summarise(n_stars_found = n()) %>%
            mutate(total_stars = sum(n_stars_found), 
                   prop_star_community = n_stars_found / total_stars, 
                   percent_star_community = prop_star_community * 100) 
        
    # sea urchins
        # number of species
        invert_data %>%
            filter(Group == "Sea Urchins") %>%
            summarise(n_urchin_sp = length(unique(Code)))
        # species present
        invert_data %>%
            filter(Group == "Sea Urchins") %>%
            distinct(Species)
        # which species were most common?
        invert_data %>%
            filter(Group == "Sea Urchins") %>%
            group_by(Species) %>%
            summarise(n_urchin_found = n())
        # how many transects had urchins?
        invert_data %>%
            filter(Group == "Sea Urchins") %>%
            group_by(Unit, Transect) %>%
            summarise(sum_urchins = n()) %>%
            ungroup() %>%
            group_by(Unit) %>%
            summarise(sum_transects_with_urchins = n())
        # total number of urchins, prop of community that each urchin sp occupies
        invert_data %>%
            filter(Group == "Sea Urchins") %>%
            group_by(Species) %>%
            summarise(n_urchins_found = n()) %>%
            mutate(total_urchins = sum(n_urchins_found), 
                   prop_urchin_community = n_urchins_found / total_urchins, 
                   percent_urchin_community = prop_urchin_community * 100)
        
    # tridacna
        # how many transects had clams?
        invert_data %>%
            filter(Group == "Clams") %>%
            group_by(Unit, Transect) %>%
            summarise(sum_clams = n()) %>%
            ungroup() %>%
            group_by(Unit) %>%
            summarise(sum_transects_with_clams = n())
        # total number of clams?
        invert_data %>%
            filter(Group == "Clams") %>%
            group_by(Species) %>%
            summarise(n_clams_found = n())
        
        
        
## 3. Species richness ----
        
    # sea cucumbers
    invert_data %>%
        filter(Group == "Sea Cucumbers") %>%
        group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
        summarise(sp_richness = length(unique(Species)))
        
        # difference by unit?
            # check parametric assumptions --> not met
                # extreme outliers? one. 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(sp_richness = length(unique(Species))) %>%
                    ungroup() %>%
                    group_by(Unit) %>%
                    identify_outliers(sp_richness)
                # normal? no.
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(sp_richness = length(unique(Species))) %>%
                    ungroup() %>%
                    group_by(Unit) %>%
                    shapiro_test(sp_richness)
                ggqqplot(invert_data %>%
                             filter(Group == "Sea Cucumbers") %>%
                             group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                             summarise(sp_richness = length(unique(Species))) %>%
                             ungroup(), x = "sp_richness", facet.by = "Unit")
                # equal variances? no. 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(sp_richness = length(unique(Species))) %>%
                    ungroup() %>% 
                    levene_test(sp_richness ~ Unit)
                
            # perform non-parametric test
            invert_data %>%
                filter(Group == "Sea Cucumbers") %>%
                group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                summarise(sp_richness = length(unique(Species))) %>%
                ungroup() %>%
                wilcox_test(sp_richness ~ Unit)
            
                # determine effect size 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(sp_richness = length(unique(Species))) %>%
                    ungroup() %>%
                    wilcox_effsize(sp_richness ~ Unit)
                
        # difference by substrate type? 
            # check parametric assumptions --> not met
                # extreme outliers? no. 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(sp_richness = length(unique(Species))) %>%
                    ungroup() %>%
                    group_by(Substrate_Characterization) %>%
                    identify_outliers(sp_richness)
                # normal? no.
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(sp_richness = length(unique(Species))) %>%
                    ungroup() %>%
                    filter(!Substrate_Characterization %in% c("AggregateReef", "RockBoulder", "Sand", "SandScatteredRock", "AggregatePatchReef")) %>% 
                    group_by(Substrate_Characterization) %>%
                    shapiro_test(sp_richness)
                ggqqplot(invert_data %>%
                             filter(Group == "Sea Cucumbers") %>%
                             group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                             summarise(sp_richness = length(unique(Species))) %>%
                             ungroup(), x = "sp_richness", facet.by = "Substrate_Characterization")
                # equal variances? yes. 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(sp_richness = length(unique(Species))) %>%
                    ungroup() %>% 
                    levene_test(sp_richness ~ Substrate_Characterization)
                
            # perform non-parametric test
            invert_data %>%
                filter(Group == "Sea Cucumbers") %>%
                group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                summarise(sp_richness = length(unique(Species))) %>%
                ungroup() %>%
                kruskal_test(sp_richness ~ Substrate_Characterization)
                
                # determine effect size 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(sp_richness = length(unique(Species))) %>%
                    ungroup() %>%
                    kruskal_effsize(sp_richness ~ Substrate_Characterization)
                
        # difference by benthic habitat? 
            # check parametric assumptions --> not met
                # extreme outliers? one. 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(sp_richness = length(unique(Species))) %>%
                    ungroup() %>%
                    group_by(Dominant_Benthic_Habitat_Type) %>%
                    identify_outliers(sp_richness)
                # normal? no.
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(sp_richness = length(unique(Species))) %>%
                    ungroup() %>%
                    filter(!Dominant_Benthic_Habitat_Type %in% c("Seagrass", "Uncolonized")) %>% 
                    group_by(Dominant_Benthic_Habitat_Type) %>%
                    shapiro_test(sp_richness)
                ggqqplot(invert_data %>%
                             filter(Group == "Sea Cucumbers") %>%
                             group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                             summarise(sp_richness = length(unique(Species))) %>%
                             ungroup(), x = "sp_richness", facet.by = "Dominant_Benthic_Habitat_Type")
                # equal variances? yes. 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(sp_richness = length(unique(Species))) %>%
                    ungroup() %>% 
                    levene_test(sp_richness ~ Dominant_Benthic_Habitat_Type)
                
            # perform non-parametric test
            invert_data %>%
                filter(Group == "Sea Cucumbers") %>%
                group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                summarise(sp_richness = length(unique(Species))) %>%
                ungroup() %>%
                kruskal_test(sp_richness ~ Dominant_Benthic_Habitat_Type)
                
                # determine effect size 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(sp_richness = length(unique(Species))) %>%
                    ungroup() %>%
                    kruskal_effsize(sp_richness ~ Dominant_Benthic_Habitat_Type)
                
        # difference in richness by distance from shore/crest/freshwater? 
            # to shore --> no
            summary(lm(sp_richness ~ Shore_Dist, data = invert_data %>%
                                                           filter(Group == "Sea Cucumbers") %>%
                                                           group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type, Shore_Dist, Crest_Dist, Fresh_Dist) %>%
                                                           summarise(sp_richness = length(unique(Species))) %>%
                                                           ungroup()))
            # to crest --> no
            summary(lm(sp_richness ~ Crest_Dist, data = invert_data %>%
                                                           filter(Group == "Sea Cucumbers") %>%
                                                           group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type, Shore_Dist, Crest_Dist, Fresh_Dist) %>%
                                                           summarise(sp_richness = length(unique(Species))) %>%
                                                           ungroup()))
            # to freshwater output --> no
            summary(lm(sp_richness ~ Fresh_Dist, data = invert_data %>%
                                                           filter(Group == "Sea Cucumbers") %>%
                                                           group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type, Shore_Dist, Crest_Dist, Fresh_Dist) %>%
                                                           summarise(sp_richness = length(unique(Species))) %>%
                                                           ungroup()))
            # multiple linear regression
            summary(lm(sp_richness ~ Shore_Dist + Fresh_Dist + Crest_Dist, data = invert_data %>%
                                                                                       filter(Group == "Sea Cucumbers") %>%
                                                                                       group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type, Shore_Dist, Crest_Dist, Fresh_Dist) %>%
                                                                                       summarise(sp_richness = length(unique(Species))) %>%
                                                                                       ungroup()))
            
    # sea urchins --> not applicable bc only four species so not very informative
            
    # sea stars --> not applicable bc only two species so not very informative
                

## 4. Density ----
                
    # sea cucumbers
    invert_data %>%
        filter(Group == "Sea Cucumbers") %>%
        group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
        summarise(cucumber_density = n() / 50)
                
        # difference by unit?
            # check parametric assumptions --> not met
                # extreme outliers? 4. 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(cucumber_density = n() / 50) %>%
                    ungroup() %>%
                    group_by(Unit) %>%
                    identify_outliers(cucumber_density)
                # normal? no.
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(cucumber_density = n() / 50) %>%
                    ungroup() %>%
                    group_by(Unit) %>%
                    shapiro_test(cucumber_density)
                ggqqplot(invert_data %>%
                             filter(Group == "Sea Cucumbers") %>%
                             group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                             summarise(cucumber_density = n() / 50) %>%
                             ungroup(), 
                         x = "cucumber_density", facet.by = "Unit")
                # equal variances? yes. 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(cucumber_density = n() / 50) %>%
                    ungroup() %>% 
                    levene_test(cucumber_density ~ Unit)
                
            # perform non-parametric test
            invert_data %>%
                filter(Group == "Sea Cucumbers") %>%
                group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                summarise(cucumber_density = n() / 50) %>%
                ungroup() %>%
                wilcox_test(cucumber_density ~ Unit)
                
                # determine effect size 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(cucumber_density = n() / 50) %>%
                    ungroup() %>%
                    wilcox_effsize(cucumber_density ~ Unit)
                
        # difference by substrate?
            # check parametric assumptions --> not met
                # extreme outliers? no. 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(cucumber_density = n() / 50) %>%
                    ungroup() %>%
                    group_by(Substrate_Characterization) %>%
                    identify_outliers(cucumber_density)
                # normal? no.
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(cucumber_density = n() / 50) %>%
                    ungroup() %>%
                    group_by(Substrate_Characterization) %>%
                    shapiro_test(cucumber_density)
                ggqqplot(invert_data %>%
                             filter(Group == "Sea Cucumbers") %>%
                             group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                             summarise(cucumber_density = n() / 50) %>%
                             ungroup(), 
                         x = "cucumber_density", facet.by = "Substrate_Characterization")
                # equal variances? yes. 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(cucumber_density = n() / 50) %>%
                    ungroup() %>% 
                    levene_test(cucumber_density ~ Substrate_Characterization)
                
            # perform non-parametric test
            invert_data %>%
                filter(Group == "Sea Cucumbers") %>%
                group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                summarise(cucumber_density = n() / 50) %>%
                ungroup() %>%
                kruskal_test(cucumber_density ~ Substrate_Characterization)
            
                # determine effect size
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(cucumber_density = n() / 50) %>%
                    ungroup() %>%
                    kruskal_effsize(cucumber_density ~ Substrate_Characterization)
                    
        # difference by benthic habitat type?
            # check parametric assumptions --> not met
                # extreme outliers? three. 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(cucumber_density = n() / 50) %>%
                    ungroup() %>%
                    group_by(Dominant_Benthic_Habitat_Type) %>%
                    identify_outliers(cucumber_density)
                # normal? no.
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(cucumber_density = n() / 50) %>%
                    ungroup() %>%
                    filter(!Dominant_Benthic_Habitat_Type %in% c("Seagrass", "Uncolonized")) %>%
                    group_by(Dominant_Benthic_Habitat_Type) %>%
                    shapiro_test(cucumber_density)
                ggqqplot(invert_data %>%
                             filter(Group == "Sea Cucumbers") %>%
                             group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                             summarise(cucumber_density = n() / 50) %>%
                             ungroup(), 
                         x = "cucumber_density", facet.by = "Dominant_Benthic_Habitat_Type")
                # equal variances? yes. 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(cucumber_density = n() / 50) %>%
                    ungroup() %>% 
                    levene_test(cucumber_density ~ Dominant_Benthic_Habitat_Type)
                
            # perform non-parametric test
            invert_data %>%
                filter(Group == "Sea Cucumbers") %>%
                group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                summarise(cucumber_density = n() / 50) %>%
                ungroup() %>%
                kruskal_test(cucumber_density ~ Dominant_Benthic_Habitat_Type)
                
                # determine effect size
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(cucumber_density = n() / 50) %>%
                    ungroup() %>%
                    kruskal_effsize(cucumber_density ~ Dominant_Benthic_Habitat_Type)
                
        # difference in density by distance from shore/crest/freshwater? 
            # to shore --> no
            summary(lm(cucumber_density ~ Shore_Dist, data = invert_data %>%
                                                               filter(Group == "Sea Cucumbers") %>%
                                                               group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type, Shore_Dist, Crest_Dist, Fresh_Dist) %>%
                                                               summarise(cucumber_density = n() / 50) %>%
                                                               ungroup()))
            # to crest --> yes
            summary(lm(cucumber_density ~ Crest_Dist, data = invert_data %>%
                                                               filter(Group == "Sea Cucumbers") %>%
                                                               group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type, Shore_Dist, Crest_Dist, Fresh_Dist) %>%
                                                               summarise(cucumber_density = n() / 50) %>%
                                                               ungroup()))
            # to freshwater output --> no
            summary(lm(cucumber_density ~ Fresh_Dist, data = invert_data %>%
                                                               filter(Group == "Sea Cucumbers") %>%
                                                               group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type, Shore_Dist, Crest_Dist, Fresh_Dist) %>%
                                                               summarise(cucumber_density = n() / 50) %>%
                                                               ungroup()))
            # multiple linear regression
            summary(lm(cucumber_density ~ Shore_Dist + Fresh_Dist + Crest_Dist, data = invert_data %>%
                                                                                           filter(Group == "Sea Cucumbers") %>%
                                                                                          group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type, Shore_Dist, Crest_Dist, Fresh_Dist) %>%
                                                                                           summarise(cucumber_density = n() / 50) %>%
                                                                                           ungroup()))
                
    # sea urchins
    invert_data %>%
        filter(Group == "Sea Urchins") %>%
        group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
        summarise(urchin_count = n(),
                  urchin_density = n() / 50) %>%
        ungroup() 
            
        # difference by unit?
            # check parametric assumptions --> not met
                # extreme outliers? one. 
                invert_data %>%
                    filter(Group == "Sea Urchins") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(urchin_density = n() / 50) %>%
                    ungroup() %>%
                    group_by(Unit) %>%
                    identify_outliers(urchin_density)
                # normal? no.
                invert_data %>%
                    filter(Group == "Sea Urchins") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(urchin_density = n() / 50) %>%
                    ungroup() %>%
                    group_by(Unit) %>%
                    shapiro_test(urchin_density)
                ggqqplot(invert_data %>%
                             filter(Group == "Sea Urchins") %>%
                             group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                             summarise(urchin_density = n() / 50) %>%
                             ungroup(), 
                         x = "urchin_density", facet.by = "Unit")
                # equal variances? yes. 
                invert_data %>%
                    filter(Group == "Sea Urchins") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(urchin_density = n() / 50) %>% 
                    ungroup() %>% 
                    levene_test(urchin_density ~ Unit)
            
            # perform non-parametric test
            invert_data %>%
                filter(Group == "Sea Urchins") %>%
                group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                summarise(urchin_density = n() / 50) %>%
            ungroup() %>%
            wilcox_test(urchin_density ~ Unit)
            
            # determine effect size 
            invert_data %>%
                filter(Group == "Sea Urchins") %>%
                group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                summarise(urchin_density = n() / 50) %>%
            ungroup() %>%
            wilcox_effsize(urchin_density ~ Unit)
            
        # difference by substrate characterization? --> not enough transects per substrate type to compare
            # Aggregate Patch Reef (n = 2 transects)
            # Pavement (n = 1)
            # Reef Rubble (n = 3)
            # Sand Scattered Coral (n = 2)
            # Sand Scattered Rock (n = 2)
            
            # sumamary stats 
            invert_data %>%
                filter(Group == "Sea Urchins") %>%
                group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                summarise(urchin_density = n() / 50) %>%
                ungroup() %>%
                group_by(Substrate_Characterization) %>%
                summarise(mean_density = mean(urchin_density), 
                          std_error_density = std.error(urchin_density))
            
        # difference by benthic habitat type?
            # check parametric assumptions --> not met
                # extreme outliers? one. 
                invert_data %>%
                    filter(Group == "Sea Urchins") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(urchin_density = n() / 50) %>%
                    ungroup() %>%
                    group_by(Dominant_Benthic_Habitat_Type) %>%
                    identify_outliers(urchin_density)
                # normal? no.
                invert_data %>%
                    filter(Group == "Sea Urchins") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(urchin_density = n() / 50) %>%
                    ungroup() %>%
                    group_by(Dominant_Benthic_Habitat_Type) %>%
                    shapiro_test(urchin_density)
                ggqqplot(invert_data %>%
                             filter(Group == "Sea Urchins") %>%
                             group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                             summarise(urchin_density = n() / 50) %>%
                             ungroup(), 
                         x = "urchin_density", facet.by = "Dominant_Benthic_Habitat_Type")
                # equal variances? yes. 
                invert_data %>%
                    filter(Group == "Sea Urchins") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(urchin_density = n() / 50) %>%
                    ungroup() %>% 
                    levene_test(urchin_density ~ Dominant_Benthic_Habitat_Type)
                
            # perform non-parametric test
            invert_data %>%
                filter(Group == "Sea Urchins") %>%
                group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                summarise(urchin_density = n() / 50) %>%
                ungroup() %>%
                kruskal_test(urchin_density ~ Dominant_Benthic_Habitat_Type)
            
                # determine effect size 
                invert_data %>%
                    filter(Group == "Sea Urchins") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(urchin_density = n() / 50) %>%
                    ungroup() %>%
                    kruskal_effsize(urchin_density ~ Dominant_Benthic_Habitat_Type)
            
        # difference in density by distance from shore/crest/freshwater? 
            # to shore --> no
            summary(lm(urchin_density ~ Shore_Dist, data = invert_data %>%
                           filter(Group == "Sea Urchins") %>%
                           group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type, Fresh_Dist, Shore_Dist, Crest_Dist) %>%
                           summarise(urchin_density = n() / 50) %>%
                           ungroup()))
            # to crest --> yes
            summary(lm(urchin_density ~ Crest_Dist, data = invert_data %>%
                           filter(Group == "Sea Urchins") %>%
                           group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type, Fresh_Dist, Shore_Dist, Crest_Dist) %>%
                           summarise(urchin_density = n() / 50) %>%
                           ungroup()))
            # to freshwater output --> no
            summary(lm(urchin_density ~ Fresh_Dist, data = invert_data %>%
                           filter(Group == "Sea Urchins") %>%
                           group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type, Fresh_Dist, Shore_Dist, Crest_Dist) %>%
                           summarise(urchin_density = n() / 50) %>%
                           ungroup()))
            # multiple linear regression
            summary(lm(urchin_density ~ Shore_Dist + Fresh_Dist + Crest_Dist, data = invert_data %>%
                           filter(Group == "Sea Urchins") %>%
                           group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type, Fresh_Dist, Shore_Dist, Crest_Dist) %>%
                           summarise(urchin_density = n() / 50) %>%
                           ungroup()))
            
                  
    # sea stars
    invert_data %>%
        filter(Group == "Sea Stars") %>%
        group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
        summarise(star_count = n(),
                  star_density = n() / 50)
    
        # difference by unit?
            # check parametric assumptions --> not met
                # extreme outliers? one. 
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(star_density = n() / 50) %>%
                    ungroup() %>%
                    group_by(Unit) %>%
                    identify_outliers(star_density)
                # normal? no.
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(star_density = n() / 50) %>%
                    ungroup() %>%
                    group_by(Unit) %>%
                    shapiro_test(star_density)
                ggqqplot(invert_data %>%
                             filter(Group == "Sea Stars") %>%
                             group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                             summarise(star_density = n() / 50) %>%
                             ungroup(), 
                         x = "star_density", facet.by = "Unit")
                # equal variances? yes. 
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(star_density = n() / 50) %>%
                    ungroup() %>% 
                    levene_test(star_density ~ Unit)
    
            # perform non-parametric test
            invert_data %>%
                filter(Group == "Sea Stars") %>%
                group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                summarise(star_density = n() / 50) %>%
                ungroup() %>%
                wilcox_test(star_density ~ Unit)
        
                # determine effect size 
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(star_density = n() / 50) %>%
                    ungroup() %>%
                    wilcox_effsize(star_density ~ Unit)
    
        # difference by substrate characterization?
            # check parametric assumptions --> not met
                # extreme outliers? no. 
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(star_density = n() / 50) %>%
                    ungroup() %>%
                    group_by(Substrate_Characterization) %>%
                    identify_outliers(star_density)
                # normal? no.
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(star_density = n() / 50) %>%
                    ungroup() %>%
                    group_by(Substrate_Characterization) %>%
                    shapiro_test(star_density)
                ggqqplot(invert_data %>%
                             filter(Group == "Sea Stars") %>%
                             group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                             summarise(star_density = n() / 50) %>%
                             ungroup(), 
                         x = "star_density", facet.by = "Substrate_Characterization")
                # equal variances? no. 
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(star_density = n() / 50) %>%
                    ungroup() %>% 
                    levene_test(star_density ~ Substrate_Characterization)
            
            # perform non-parametric test
            invert_data %>%
                filter(Group == "Sea Stars") %>%
                group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                summarise(star_density = n() / 50) %>%
                ungroup() %>%
                kruskal_test(star_density ~ Substrate_Characterization)
            
                # determine effect size 
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(star_density = n() / 50) %>%
                    ungroup() %>%
                    kruskal_effsize(star_density ~ Unit)
            
        # difference by benthic habitat type?
            # check parametric assumptions --> not met
                # extreme outliers? one. 
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(star_density = n() / 50) %>%
                    ungroup() %>%
                    group_by(Dominant_Benthic_Habitat_Type) %>%
                    identify_outliers(star_density)
                # normal? no.
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(star_density = n() / 50) %>%
                    ungroup() %>%
                    group_by(Dominant_Benthic_Habitat_Type) %>%
                    shapiro_test(star_density)
                ggqqplot(invert_data %>%
                             filter(Group == "Sea Stars") %>%
                             group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                             summarise(star_density = n() / 50) %>%
                             ungroup(), 
                         x = "star_density", facet.by = "Dominant_Benthic_Habitat_Type")
                # equal variances? yes. 
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(star_density = n() / 50) %>%
                    ungroup() %>% 
                    levene_test(star_density ~ Dominant_Benthic_Habitat_Type)
            
            # perform non-parametric test
            invert_data %>%
                filter(Group == "Sea Stars") %>%
                group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                summarise(star_density = n() / 50) %>%
                ungroup() %>%
                kruskal_test(star_density ~ Dominant_Benthic_Habitat_Type)
                
                # determine effect size 
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
                    summarise(star_density = n() / 50) %>%
                    ungroup() %>%
                    kruskal_effsize(star_density ~ Dominant_Benthic_Habitat_Type)
            
        # difference in density by distance from shore/crest/freshwater? 
            # to shore --> no
            summary(lm(star_density ~ Shore_Dist, data = invert_data %>%
                                                               filter(Group == "Sea Stars") %>%
                                                               group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type, Shore_Dist, Crest_Dist, Fresh_Dist) %>%
                                                               summarise(star_density = n() / 50) %>%
                                                               ungroup()))
            # to crest --> yes
            summary(lm(star_density ~ Crest_Dist, data = invert_data %>%
                                                                   filter(Group == "Sea Stars") %>%
                                                                   group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type, Shore_Dist, Crest_Dist, Fresh_Dist) %>%
                                                                   summarise(star_density = n() / 50) %>%
                                                                   ungroup()))
            # to freshwater output --> no
            summary(lm(star_density ~ Fresh_Dist, data = invert_data %>%
                                                                   filter(Group == "Sea Stars") %>%
                                                                   group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type, Shore_Dist, Crest_Dist, Fresh_Dist) %>%
                                                                   summarise(star_density = n() / 50) %>%
                                                                   ungroup()))
            # multiple linear regression
            summary(lm(star_density ~ Shore_Dist + Fresh_Dist + Crest_Dist, data = invert_data %>%
                                                                                               filter(Group == "Sea Stars") %>%
                                                                                               group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type, Shore_Dist, Crest_Dist, Fresh_Dist) %>%
                                                                                               summarise(star_density = n() / 50) %>%
                                                                                               ungroup()))
                               
                
## 5. Size ----
                
    ## sea cucumbers
    invert_data %>%
        filter(Group == "Sea Cucumbers") %>%
        mutate(Taxon_Name = Species) %>%
        separate(col = Taxon_Name, into = c("Genus", "Species"), sep = " ", remove = F) %>%
        group_by(Genus) %>%
        summarise(mean_length = mean(Size), 
                  se_length = std.error(Size))
                
    invert_data %>%
        filter(Group == "Sea Cucumbers") %>%
        mutate(Taxon_Name = Species) %>%
        group_by(Taxon_Name) %>%
        summarise(mean_length = mean(Size), 
                  se_length = std.error(Size))
    
        # difference by unit?
            # check parametric assumptions --> not met.
                # extreme outliers? four.
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Unit) %>%
                    identify_outliers(Size) %>%
                    filter(is.extreme == T)
                # normal? no. 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    filter(!Code == "SYMA") %>%
                    group_by(Unit) %>%
                    shapiro_test(Size)
                ggqqplot(invert_data %>% 
                             filter(Group == "Sea Cucumbers") %>%
                             filter(!Code == "SYMA"), 
                         x = "Size", facet.by = "Unit")
                # equal variances? no. 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    filter(!Code == "SYMA") %>%
                    levene_test(Size ~ Unit)
        
            # perform non-parametric test
            invert_data %>%
                filter(Group == "Sea Cucumbers") %>%
                wilcox_test(Size ~ Unit)
     
                # determine effect size 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    wilcox_effsize(Size ~ Unit)
            
        ## by substrate type?
            # check parametric assumptions --> not met.
                # extreme outliers? 26.
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Substrate_Characterization) %>%
                    identify_outliers(Size) %>%
                    filter(is.extreme == T)
                # normal? no. 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Substrate_Characterization) %>%
                    shapiro_test(Size)
                ggqqplot(invert_data %>% 
                             filter(Group == "Sea Cucumbers"), 
                         x = "Size", facet.by = "Substrate_Characterization")
                # equal variances? no. 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    levene_test(Size ~ Substrate_Characterization)
                
            # perform non-parametric test
            invert_data %>%
                filter(Group == "Sea Cucumbers") %>%
                kruskal_test(Size ~ Substrate_Characterization)
                
                # determine effect size 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    kruskal_effsize(Size ~ Substrate_Characterization)
                
                # perform post-hoc test
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    dunn_test(Size ~ Substrate_Characterization) %>%
                    filter(p.adj <= 0.05)
                
        # by benthic habitat type?
            # check parametric assumptions --> not met.
                # extreme outliers? 5.
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Dominant_Benthic_Habitat_Type) %>%
                    identify_outliers(Size) %>%
                    filter(is.extreme == T)
                # normal? no. 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    group_by(Dominant_Benthic_Habitat_Type) %>%
                    shapiro_test(Size)
                ggqqplot(invert_data %>% 
                             filter(Group == "Sea Cucumbers"), 
                         x = "Size", facet.by = "Dominant_Benthic_Habitat_Type")
                # equal variances? yes. 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    levene_test(Size ~ Dominant_Benthic_Habitat_Type)
                
            # perform non-parametric test
            invert_data %>%
                filter(Group == "Sea Cucumbers") %>%
                kruskal_test(Size ~ Dominant_Benthic_Habitat_Type)
                
                # determine effect size 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    kruskal_effsize(Size ~ Dominant_Benthic_Habitat_Type)
                
                # perform post-hoc test
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    dunn_test(Size ~ Dominant_Benthic_Habitat_Type) %>%
                    filter(p.adj <= 0.05)
    
        # difference in size by distance from shore/crest/freshwater? 
            # to shore --> no
            summary(lm(Size ~ Shore_Dist, data = invert_data %>% filter(Group == "Sea Cucumbers")))
            # to crest --> no
            summary(lm(Size ~ Crest_Dist, data = invert_data %>% filter(Group == "Sea Cucumbers")))
            # to freshwater output --> yes
            summary(lm(Size ~ Fresh_Dist, data = invert_data %>% filter(Group == "Sea Cucumbers")))
            # multiple linear regression
            summary(lm(Size ~ Shore_Dist + Fresh_Dist + Crest_Dist, 
                       data = invert_data %>% filter(Group == "Sea Cucumbers")))
                

    ## sea urchins
    invert_data %>%
        filter(Group == "Sea Urchins") %>%
        mutate(Taxon_Name = Species) %>%
        group_by(Taxon_Name) %>%
        summarise(mean_radius = mean(Size), 
                  se_radius = std.error(Size))
    
        # difference by unit?
            # check parametric assumptions --> not met.
                # extreme outliers? three.
                invert_data %>%
                    filter(Group == "Sea Urchins") %>%
                    group_by(Unit) %>%
                    identify_outliers(Size) %>%
                    filter(is.extreme == T)
                # normal? no. 
                invert_data %>%
                    filter(Group == "Sea Urchins") %>%
                    group_by(Unit) %>%
                    shapiro_test(Size)
                ggqqplot(invert_data %>% 
                             filter(Group == "Sea Urchins"),
                         x = "Size", facet.by = "Unit")
                # equal variances? yes. 
                invert_data %>%
                    filter(Group == "Sea Urchins") %>%
                    levene_test(Size ~ Unit)
            
            # perform non-parametric test
            invert_data %>%
                filter(Group == "Sea Urchins") %>%
                wilcox_test(Size ~ Unit)
            
                # determine effect size 
                invert_data %>%
                    filter(Group == "Sea Urchins") %>%
                    wilcox_effsize(Size ~ Unit)
        
        # by substrate type?
            # check parametric assumptions --> not met.
                # extreme outliers? one
                invert_data %>%
                    filter(Group == "Sea Urchins") %>%
                    group_by(Substrate_Characterization) %>%
                    identify_outliers(Size) %>%
                    filter(is.extreme == T)
                # normal? no.  
                invert_data %>%
                    filter(Group == "Sea Urchins") %>%
                    group_by(Substrate_Characterization) %>%
                    shapiro_test(Size)
                ggqqplot(invert_data %>% 
                             filter(Group == "Sea Urchins"),
                         x = "Size", facet.by = "Substrate_Characterization")
                # equal variances? no. 
                invert_data %>%
                    filter(Group == "Sea Urchins") %>%
                    levene_test(Size ~ Substrate_Characterization)
        
            # perform non-parametric test
            invert_data %>%
                filter(Group == "Sea Urchins") %>%
                kruskal_test(Size ~ Substrate_Characterization)
            
                # find effect size
                invert_data %>%
                    filter(Group == "Sea Urchins") %>%
                    kruskal_effsize(Size ~ Substrate_Characterization)
            
                # perform post-hoc test
                invert_data %>%
                    filter(Group == "Sea Urchins") %>%
                    dunn_test(Size ~ Substrate_Characterization) %>%
                    filter(p.adj <= 0.05)
        
        # by benthic habitat type?
            # check parametric assumptions --> met.
                # extreme outliers? no.
                invert_data %>%
                    filter(Group == "Sea Urchins") %>%
                    group_by(Dominant_Benthic_Habitat_Type) %>%
                    identify_outliers(Size) %>%
                    filter(is.extreme == T)
                # normal? yes. 
                invert_data %>%
                    filter(Group == "Sea Urchins") %>%
                    filter(!Dominant_Benthic_Habitat_Type == "Coral") %>%
                    group_by(Dominant_Benthic_Habitat_Type) %>%
                    shapiro_test(Size)
                ggqqplot(invert_data %>% 
                             filter(Group == "Sea Urchins"), 
                         x = "Size", facet.by = "Dominant_Benthic_Habitat_Type")
                # equal variances? yes. 
                invert_data %>%
                    filter(Group == "Sea Urchins") %>%
                    levene_test(Size ~ Dominant_Benthic_Habitat_Type)
        
            # perform non-parametric test (assumptions of parametric met but i dont want to remove a habitat type group)
            invert_data %>%
                filter(Group == "Sea Urchins") %>%
                kruskal_test(Size ~ Dominant_Benthic_Habitat_Type)
            
                # calculate effect size
                invert_data %>%
                    filter(Group == "Sea Urchins") %>%
                    kruskal_effsize(Size ~ Dominant_Benthic_Habitat_Type)
        
        # difference in size by distance from shore/crest/freshwater? 
            # to shore --> no
            summary(lm(Size ~ Shore_Dist, data = invert_data %>% filter(Group == "Sea Urchins")))
            # to crest --> no
            summary(lm(Size ~ Crest_Dist, data = invert_data %>% filter(Group == "Sea Urchins")))
            # to freshwater output --> yes
            summary(lm(Size ~ Fresh_Dist, data = invert_data %>% filter(Group == "Sea Urchins")))
            # multiple linear regression
            summary(lm(Size ~ Shore_Dist + Fresh_Dist + Crest_Dist, 
                       data = invert_data %>% filter(Group == "Sea Urchins")))
    
        
    ## sea stars
    invert_data %>%
        filter(Group == "Sea Stars") %>%
        mutate(Taxon_Name = Species) %>%
        group_by(Taxon_Name) %>%
        summarise(mean_radius = mean(Size), 
                  se_radius = std.error(Size))
            
        # difference by unit?
            # check parametric assumptions --> not met.
                # extreme outliers? no
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    group_by(Unit) %>%
                    identify_outliers(Size) %>%
                    filter(is.extreme == T)
                # normal? yes. 
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    group_by(Unit) %>%
                    shapiro_test(Size)
                ggqqplot(invert_data %>% 
                             filter(Group == "Sea Cucumbers") %>%
                             filter(!Code == "SYMA"), 
                         x = "Size", facet.by = "Unit")
                # equal variances? yes. 
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    levene_test(Size ~ Unit)
    
            # perform parametric test
            invert_data %>%
                filter(Group == "Sea Stars") %>%
                t_test(Size ~ Unit)
            
                # determine effect size 
                invert_data %>%
                    filter(Group == "Sea Cucumbers") %>%
                    wilcox_effsize(Size ~ Unit)
    
        # by substrate type?
            # check parametric assumptions --> not met.
                # extreme outliers? none
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    group_by(Substrate_Characterization) %>%
                    identify_outliers(Size) %>%
                    filter(is.extreme == T)
                # normal? basically.  
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    group_by(Substrate_Characterization) %>%
                    shapiro_test(Size)
                ggqqplot(invert_data %>% 
                             filter(Group == "Sea Stars"), 
                         x = "Size", facet.by = "Substrate_Characterization")
                # equal variances? yes. 
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    levene_test(Size ~ Substrate_Characterization)
    
            # perform non-parametric test
            invert_data %>%
                filter(Group == "Sea Stars") %>%
                anova_test(Size ~ Substrate_Characterization)
    
                # perform post-hoc test
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    tukey_hsd(Size ~ Substrate_Characterization) %>%
                    filter(p.adj <= 0.05)
    
        # by benthic habitat type?
            # check parametric assumptions --> not met.
                # extreme outliers? no.
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    group_by(Dominant_Benthic_Habitat_Type) %>%
                    identify_outliers(Size) %>%
                    filter(is.extreme == T)
                # normal? basically. 
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    filter(!Dominant_Benthic_Habitat_Type == "Coral") %>%
                    group_by(Dominant_Benthic_Habitat_Type) %>%
                    shapiro_test(Size)
                ggqqplot(invert_data %>% 
                             filter(Group == "Sea Stars"), 
                         x = "Size", facet.by = "Dominant_Benthic_Habitat_Type")
                # equal variances? yes. 
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    levene_test(Size ~ Dominant_Benthic_Habitat_Type)
    
            # perform parametric test
            invert_data %>%
                filter(Group == "Sea Stars") %>%
                anova_test(Size ~ Dominant_Benthic_Habitat_Type)
            
                # perform post-hoc test
                invert_data %>%
                    filter(Group == "Sea Stars") %>%
                    dunn_test(Size ~ Dominant_Benthic_Habitat_Type)
            
        # difference in size by distance from shore/crest/freshwater? 
            # to shore --> no
            summary(lm(Size ~ Shore_Dist, data = invert_data %>% filter(Group == "Sea Stars")))
            # to crest --> no
            summary(lm(Size ~ Crest_Dist, data = invert_data %>% filter(Group == "Sea Stars")))
            # to freshwater output --> yes
            summary(lm(Size ~ Fresh_Dist, data = invert_data %>% filter(Group == "Sea Stars")))
            # multiple linear regression
            summary(lm(Size ~ Shore_Dist + Fresh_Dist + Crest_Dist, 
                       data = invert_data %>% filter(Group == "Sea Stars")))
    
    
   ## Tridacna 
    invert_data %>%
        filter(Group == "Clams") %>%
        mutate(Taxon_Name = Species) %>%
        group_by(Taxon_Name) %>%
        summarise(mean_length = mean(Size), 
                  se_length = std.error(Size)) 
    
    # difference in size by distance from shore/crest/freshwater? 
        # to shore --> no
        summary(lm(Size ~ Shore_Dist, data = invert_data %>% filter(Group == "Clams")))
        # to crest --> no
        summary(lm(Size ~ Crest_Dist, data = invert_data %>% filter(Group == "Clams")))
        # to freshwater output --> yes
        summary(lm(Size ~ Fresh_Dist, data = invert_data %>% filter(Group == "Clams")))
        # multiple linear regression
        summary(lm(Size ~ Shore_Dist + Fresh_Dist + Crest_Dist, 
                   data = invert_data %>% filter(Group == "Clams")))
    
    
            
            
 