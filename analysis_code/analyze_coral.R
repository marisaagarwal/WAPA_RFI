# 2022-11-07

## 1. Set up ----

    # point to data locale
    data_locale = "creation_code/"
    
    # load in the data
    source(paste0(data_locale, "create_coral.R"))


## 2. Alpha Diversity ----

    # calculation
    coral_diversity = 
        coralNMDSdata %>%
            pivot_longer(cols = c(3:23), 
                         names_to = "species_code", 
                         values_to = "count") %>%
            filter(!count == 0) %>%
            group_by(Unit, Transect) %>%
            dplyr::summarise(sp_richness = length(unique(species_code))) %>%
            mutate(unit_transect = paste(Unit, "_", Transect))
    
    # combining metadata & diversity
    coralsummary = merge(metadata, coral_diversity) 

    # ANALYSIS
    
        # summary stats for unit
        coralsummary %>%
            group_by(Unit) %>%
            dplyr::summarise(mean_richness = mean(sp_richness),
                             se_richness = std.error(sp_richness))
        
        # difference in richness by site? -> no
        coralsummary %>%
            t_test(sp_richness ~ Unit)
        
        # difference in richness by distance from shore/crest/freshwater? 
            # to shore
            summary(lm(sp_richness ~ Shore_Dist, data = coralsummary))
            # to crest
            summary(lm(sp_richness ~ Crest_Dist, data = coralsummary))
            # to freshwater output
            summary(lm(sp_richness ~ Fresh_Dist, data = coralsummary))
            
        # difference in richness by substrate characterization (overall)? -> yes
        coralsummary %>%
            anova_test(sp_richness ~ Substrate_Characterization)
    
        coralsummary %>%
            tukey_hsd(sp_richness ~ Substrate_Characterization) %>%
            filter(!p.adj > 0.05)
        
        # difference in richness by substrate characterization (for each unit)? -> yes, only Agat
            # Asan
            coralsummary %>%
                filter(Unit == "Asan") %>%
                anova_test(sp_richness ~ Substrate_Characterization)
            
            #Agat
            coralsummary %>%
                filter(Unit == "Agat") %>%
                anova_test(sp_richness ~ Substrate_Characterization)
            
            coralsummary %>%
                filter(Unit == "Agat") %>%
                tukey_hsd(sp_richness ~ Substrate_Characterization)
    
        # difference in richness by benthic habitat type (overall)? -> yes
            coralsummary %>%
                anova_test(sp_richness ~ Dominant_Benthic_Habitat_Type)
            
            coralsummary %>%
                tukey_hsd(sp_richness ~ Dominant_Benthic_Habitat_Type)
            
        # difference in richness by benthic habitat type (for each unit)? -> yes, only Asan
            # Asan
            coralsummary %>%
                filter(Unit == "Asan") %>%
                anova_test(sp_richness ~ Dominant_Benthic_Habitat_Type)
            
            coralsummary %>%
                filter(Unit == "Asan") %>%
                tukey_hsd(sp_richness ~ Dominant_Benthic_Habitat_Type)
            
            #Agat
            coralsummary %>%
                filter(Unit == "Agat") %>%
                anova_test(sp_richness ~ Dominant_Benthic_Habitat_Type)
        
    
## 4. NMDS (species level) ----
        
        # set up data        
        vegan_coralNMDS_data = 
            merge(coralsummary %>%
                      dplyr::select(c(Unit, Transect, Substrate_Characterization, Dominant_Benthic_Habitat_Type)),
                  coralNMDSdata)
        
        # conduct NMDS 
        coralNMDS_object = metaMDS(vegan_coralNMDS_data[,5:ncol(vegan_coralNMDS_data)], 
                                    k = 2,
                                    distance = "bray", 
                                    trymax = 100)
        
        # examine stressplot & baseplot
        stressplot(coralNMDS_object)
        plot(coralNMDS_object)
        
        # create parsed down grouping dataframe and add row_ID column
        reference_coralNMDS = 
            vegan_coralNMDS_data %>%
            dplyr::select(c(Unit, Transect, Substrate_Characterization, Dominant_Benthic_Habitat_Type)) %>%
            ungroup() %>%
            dplyr::mutate(row_ID = row_number())
        
        # extract data for plotting
        plotting_coralNMDS = 
            scores(coralNMDS_object, display = "sites") %>% 
            as.data.frame() %>% 
            rownames_to_column("row_ID")
        
        plotting_coralNMDS = merge(reference_coralNMDS, plotting_coralNMDS)
        
        # fit environmental and species vectors
        coralNMDS_envfit =
            envfit(coralNMDS_object, 
                   reference_coralNMDS, 
                   permutations = 999,
                   na.rm = TRUE) # this fits environmental vectors
        
        coralNMDS_speciesfit =
            envfit(coralNMDS_object, 
                   vegan_coralNMDS_data[,5:ncol(vegan_coralNMDS_data)], 
                   permutations = 999,
                   na.rm = T) # this fits species vectors      
        
        # which species contribute to differences in NMDS plots?
        coral_species_scores =
            as.data.frame(scores(coralNMDS_speciesfit,
                                 display = "vectors"))                                      #save species intrinsic values into dataframe
        
        coral_species_scores = cbind(coral_species_scores, 
                                     Species = rownames(coral_species_scores))        #add species names to dataframe
        
        coral_species_scores = cbind(coral_species_scores,
                                      pval = coralNMDS_speciesfit$vectors$pvals)      #add pvalues to dataframe so you can select species which are significant
        
        
        coral_species_scores = cbind(coral_species_scores,
                                     abrev = abbreviate(coral_species_scores$Species,
                                                          minlength = 4, 
                                                          method = "both"))                #abbreviate species names
        
        significant_coral_species_scores = subset(coral_species_scores,
                                                  pval <= 0.05)                          #subset data to show species significant at 0.05
        
        # which environmental factors contribute to differences in NMDS plots?
        coral_env_scores =
            as.data.frame(scores(coralNMDS_envfit,
                                 display = "vectors"))                        # save species intrinsic values into dataframe
        
        coral_env_scores = cbind(coral_env_scores, 
                                 Species = rownames(coral_env_scores))        # add species names to dataframe
        
        coral_env_scores = cbind(coral_env_scores,
                                 pval = coralNMDS_envfit$vectors$pvals)       # add pvalues to dataframe so you can select species which are significant
        
        
        # current_env_scores = cbind(current_env_scores,
        #                                abrev = abbreviate(current_env_scores$Species,
        #                                                   minlength = 4, 
        #                                                   method = "both"))                #abbreviate environmental factor names
        
        significant_coral_env_scores = subset(coral_env_scores,
                                                pval <= 0.05)                          #subset data to show environmental factors significant at 0.05
        
        
## 5. PERMANOVA of NMDS (species level) ----
        
    # difference in coral community based on Unit? 
        
        # site assumption: do groups have homogeneous variances? 
        dis = vegdist(vegan_coralNMDS_data[,5:ncol(vegan_coralNMDS_data)], method="bray")
        mod = betadisper(dis, reference_coralNMDS$Unit)
        anova(mod)      # p>0.05, proceed
        plot(mod)
        
        adonis2(vegan_coralNMDS_data[,5:ncol(vegan_coralNMDS_data)] ~ Unit, 
                data = reference_coralNMDS, 
                permutations = 9999,
                method = "bray")                     # NO difference in community based on site
        
    # difference in coral community based on substrate characterization? 
        
        # site assumption: do groups have homogeneous variances? 
        dis = vegdist(vegan_coralNMDS_data[,5:ncol(vegan_coralNMDS_data)], method="bray")
        mod = betadisper(dis, reference_coralNMDS$Substrate_Characterization)
        anova(mod)      # p>0.05, proceed
        plot(mod)
        
        adonis2(vegan_coralNMDS_data[,5:ncol(vegan_coralNMDS_data)] ~ Substrate_Characterization, 
                data = reference_coralNMDS, 
                permutations = 9999,
                method = "bray")                     # NO difference in community based on substrate characterization
        
    # difference in coral community based on dominant benthic habitat type? 
        
        # site assumption: do groups have homogeneous variances? 
        dis = vegdist(vegan_coralNMDS_data[,5:ncol(vegan_coralNMDS_data)], method="bray")
        mod = betadisper(dis, reference_coralNMDS$Dominant_Benthic_Habitat_Type)
        anova(mod)      # p>0.05, proceed
        plot(mod)
        
        adonis2(vegan_coralNMDS_data[,5:ncol(vegan_coralNMDS_data)] ~ Dominant_Benthic_Habitat_Type, 
                data = reference_coralNMDS, 
                permutations = 9999,
                method = "bray")                     # YES difference in community based on dominant benthic habitat type
        
        
        
## 6. Coral Density ----
        
    # calculations
    coral_density = 
        coralNMDSdata %>%
            pivot_longer(cols = c(3:23), 
                         names_to = "species_code", 
                         values_to = "count") %>%
                group_by(Unit, Transect) %>%
                dplyr::summarise(total_corals = sum(count)) %>%
                mutate(coral_density = total_corals/10, 
                       unit_transect = paste(Unit, "_", Transect))
        
    # combining metadata & diversity
    coralsummary = merge(coralsummary, coral_density) 
    
    # prune
    coralsummary = 
        coralsummary %>%
            dplyr::select(c(Unit, Transect, unit_transect, Shore_Dist, Crest_Dist, Fresh_Dist,
                            Substrate_Characterization, Dominant_Benthic_Habitat_Type, 
                            sp_richness, coral_density))

    # ANALYSIS
    
        # summary stats for unit
        coralsummary %>%
            group_by(Unit) %>%
            dplyr::summarise(mean_density = mean(coral_density),
                             se_density = std.error(coral_density))
    
        # difference in density by site? -> yes
        coralsummary %>%
            t_test(coral_density ~ Unit)
        
        # difference in density by substrate characterization (overall)? -> no
        coralsummary %>%
            anova_test(coral_density ~ Substrate_Characterization)
        
        # difference in density by substrate characterization (for each unit)? -> yes, only Asan
            # Asan
            coralsummary %>%
                filter(Unit == "Asan") %>%
                anova_test(coral_density ~ Substrate_Characterization)
    
            coralsummary %>%
                filter(Unit == "Asan") %>%
                tukey_hsd(coral_density ~ Substrate_Characterization)
        
            #Agat
            coralsummary %>%
                filter(Unit == "Agat") %>%
                anova_test(coral_density ~ Substrate_Characterization)
        
        # difference in density by benthic habitat type (overall)? -> no
        coralsummary %>%
            anova_test(coral_density ~ Dominant_Benthic_Habitat_Type)
        
        # difference in density by benthic habitat type (for each unit)? -> yes, only Asan
            # Asan
            coralsummary %>%
                filter(Unit == "Asan") %>%
                anova_test(coral_density ~ Dominant_Benthic_Habitat_Type)
            
            coralsummary %>%
                filter(Unit == "Asan") %>%
                tukey_hsd(coral_density ~ Dominant_Benthic_Habitat_Type)
        
            #Agat
            coralsummary %>%
                filter(Unit == "Agat") %>%
                anova_test(coral_density ~ Dominant_Benthic_Habitat_Type)

        # difference in density by distance from shore/crest/freshwater? 
            # to shore
            summary(lm(coral_density ~ Shore_Dist, data = coralsummary))
            # to crest
            summary(lm(coral_density ~ Crest_Dist, data = coralsummary))
            # to freshwater output
            summary(lm(coral_density ~ Fresh_Dist, data = coralsummary))              
        
        
        
        
        
        
        
        
    
    
    
    
