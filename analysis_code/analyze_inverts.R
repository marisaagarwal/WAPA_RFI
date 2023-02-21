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
                

    # number of urchin species
    
    # number of sea star species
                

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
                
                
## 5. Size ----
                
    # sea cucumbers
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
            
        # by substrate type?
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
    

                
    
                

                
    