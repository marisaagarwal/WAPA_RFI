# 2022-11-07

## 1. Set up ----

    # point to data locale
    data_locale = "creation_code/"
    
    # load in the data
    source(paste0(data_locale, "create_fish.R"))
    
    
## 2. Alpha Diversity ----
    
    # calculation
    fish_diversity = 
        fishdata %>%
            group_by(Unit, Transect) %>%
            dplyr::summarise(sp_richness = length(unique(Species))) %>%
            mutate(unit_transect = paste(Unit, "_", Transect))

    #combining metadata and diversity
    fishsummary = merge(metadata, fish_diversity)
    
    #ANALYSIS
    
        # summary stats
        fishsummary %>%
            group_by(Unit) %>%
            dplyr::summarise(mean_richness = mean(sp_richness),
                             se_richness = std.error(sp_richness))

        # difference in richness by unit? --> no
        fishsummary %>%
            t_test(sp_richness ~ Unit)
        
        # difference in richness by distance from shore/crest/freshwater? 
            # to shore --> yes
            summary(lm(sp_richness ~ Shore_Dist, data = fishsummary))
            # to crest --> yes
            summary(lm(sp_richness ~ Crest_Dist, data = fishsummary))
            # to freshwater output --> yes
            summary(lm(sp_richness ~ Fresh_Dist, data = fishsummary))
        
        # difference in richness by substrate characterization (overall)? -> yes
        fishsummary %>%
            anova_test(sp_richness ~ Substrate_Characterization)
        
        fishsummary %>%
            tukey_hsd(sp_richness ~ Substrate_Characterization) %>%
            filter(!p.adj > 0.05)
        
            # Asan only? --> yes
            fishsummary %>%
                filter(Unit == "Asan") %>%
                anova_test(sp_richness ~ Substrate_Characterization)
            
            fishsummary %>%
                filter(Unit == "Asan") %>%
                tukey_hsd(sp_richness ~ Substrate_Characterization) %>%
                filter(!p.adj > 0.05)
            
            # Agat only? --> no
            fishsummary %>%
                filter(Unit == "Agat") %>%
                anova_test(sp_richness ~ Substrate_Characterization)
            
        # difference in richness by benthic habitat type (overall)? -> yes
        fishsummary %>%
            anova_test(sp_richness ~ Dominant_Benthic_Habitat_Type)
            
        fishsummary %>%
            tukey_hsd(sp_richness ~ Dominant_Benthic_Habitat_Type) %>%
            filter(!p.adj > 0.05)
        
            # Asan only? --> no
            fishsummary %>%
                filter(Unit == "Asan") %>%
                anova_test(sp_richness ~ Dominant_Benthic_Habitat_Type)
            
            # Agat only? --> yes
            fishsummary %>%
                filter(Unit == "Agat") %>%
                anova_test(sp_richness ~ Dominant_Benthic_Habitat_Type)
            
            fishsummary %>%
                filter(Unit == "Agat") %>%
                tukey_hsd(sp_richness ~ Dominant_Benthic_Habitat_Type) %>%
                filter(!p.adj > 0.05)
        
        
## 3. Fish density ----
        
    # calculations
    fish_density = 
        fishdata %>%
            group_by(Unit, Transect) %>%
            dplyr::summarise(total_fish = sum(Transect)) %>%
            mutate(fish_density = total_fish/50, 
                   unit_transect = paste(Unit, "_", Transect))
        
    # combining metadata & diversity
    fishsummary = merge(fishsummary, fish_density) 
        
    # prune
    fishsummary = 
        fishsummary %>%
            dplyr::select(c(Unit, Transect, unit_transect, Shore_Dist, Crest_Dist, Fresh_Dist,
                            Substrate_Characterization, Dominant_Benthic_Habitat_Type, 
                            sp_richness, fish_density))
        
    # ANALYSIS
        
        # summary stats for unit
        fishsummary %>%
            group_by(Unit) %>%
            dplyr::summarise(mean_density = mean(fish_density),
                             se_density = std.error(fish_density))
        
        # difference in density by site? -> no
        fishsummary %>%
            t_test(fish_density ~ Unit)
        
        # difference in density by substrate characterization (overall)? -> yes
        fishsummary %>%
            anova_test(fish_density ~ Substrate_Characterization)
        
        fishsummary %>%
            tukey_hsd(fish_density ~ Substrate_Characterization) %>%
            filter(!p.adj > 0.05)
        
            # Asan only? --> yes
            fishsummary %>%
                filter(Unit == "Asan") %>%
                anova_test(fish_density ~ Substrate_Characterization)
            
            fishsummary %>%
                filter(Unit == "Asan") %>%
                tukey_hsd(fish_density ~ Substrate_Characterization) %>%
                filter(!p.adj > 0.05)
            
            #Agat only? --> no
            fishsummary %>%
                filter(Unit == "Agat") %>%
                anova_test(fish_density ~ Substrate_Characterization)
        
        # difference in richness by benthic habitat type (overall)? -> yes
        fishsummary %>%
            anova_test(fish_density ~ Dominant_Benthic_Habitat_Type)
        
        fishsummary %>%
            tukey_hsd(fish_density ~ Dominant_Benthic_Habitat_Type) %>%
            filter(!p.adj > 0.05)
        
            # Asan only? --> no
            fishsummary %>%
                filter(Unit == "Asan") %>%
                anova_test(fish_density ~ Dominant_Benthic_Habitat_Type)
            
            #Agat only? --> yes
            fishsummary %>%
                filter(Unit == "Agat") %>%
                anova_test(fish_density ~ Dominant_Benthic_Habitat_Type)
            
            fishsummary %>%
                filter(Unit == "Agat") %>%
                tukey_hsd(fish_density ~ Dominant_Benthic_Habitat_Type) %>%
                filter(!p.adj > 0.05)
        
        # difference in density by distance from shore/crest/freshwater? 
            # to shore
            summary(lm(fish_density ~ Shore_Dist, data = fishsummary))
            # to crest
            summary(lm(fish_density ~ Crest_Dist, data = fishsummary))
            # to freshwater output
            summary(lm(fish_density ~ Fresh_Dist, data = fishsummary))              
        
    
## 4. NMDS (species level) ----
            
    # set up data        
    vegan_fishNMDS_data = 
        merge(fishsummary %>%
                dplyr::select(c(Unit, Transect, Substrate_Characterization, Dominant_Benthic_Habitat_Type)),
                fishNMDSdata) 
            
    # conduct NMDS 
    fishNMDS_object = metaMDS(vegan_fishNMDS_data[,5:ncol(vegan_fishNMDS_data)], 
                              k = 2,
                              distance = "bray", 
                              trymax = 100)
            
    # examine stressplot & baseplot
    stressplot(fishNMDS_object)
    plot(fishNMDS_object)
            
    # create parsed down grouping dataframe and add row_ID column
    reference_fishNMDS = 
        vegan_fishNMDS_data %>%
            dplyr::select(c(Unit, Transect, Substrate_Characterization, Dominant_Benthic_Habitat_Type)) %>%
            ungroup() %>%
            dplyr::mutate(row_ID = row_number())
            
    # extract data for plotting
    plotting_fishNMDS = 
        scores(fishNMDS_object, display = "sites") %>% 
        as.data.frame() %>% 
        rownames_to_column("row_ID")
            
    plotting_fishNMDS = merge(reference_fishNMDS, plotting_fishNMDS)
            
    # fit environmental and species vectors
    fishNMDS_envfit =
        envfit(fishNMDS_object, 
               reference_fishNMDS, 
               permutations = 999,
               na.rm = TRUE) # this fits environmental vectors
    
    fishNMDS_speciesfit =
        envfit(fishNMDS_object, 
               vegan_fishNMDS_data[,5:ncol(vegan_fishNMDS_data)], 
               permutations = 999,
               na.rm = T) # this fits species vectors      
    
    # which species contribute to differences in NMDS plots?
    fish_species_scores =
        as.data.frame(scores(fishNMDS_speciesfit,
                             display = "vectors"))                                      #save species intrinsic values into dataframe
    
    fish_species_scores = cbind(fish_species_scores, 
                                 Species = rownames(fish_species_scores))        #add species names to dataframe
    
    fish_species_scores = cbind(fish_species_scores,
                                 pval = fishNMDS_speciesfit$vectors$pvals)      #add pvalues to dataframe so you can select species which are significant
    
    
    fish_species_scores = cbind(fish_species_scores,
                                 abrev = abbreviate(fish_species_scores$Species,
                                                    minlength = 4, 
                                                    method = "both"))                #abbreviate species names
    
    significant_fish_species_scores = subset(fish_species_scores,
                                              pval <= 0.05)                          #subset data to show species significant at 0.05
    
    # which environmental factors contribute to differences in NMDS plots?
    fish_env_scores =
        as.data.frame(scores(fishNMDS_envfit,
                             display = "vectors"))                        # save species intrinsic values into dataframe
    
    fish_env_scores = cbind(fish_env_scores, 
                             Species = rownames(fish_env_scores))        # add species names to dataframe
    
    fish_env_scores = cbind(fish_env_scores,
                             pval = fishNMDS_envfit$vectors$pvals)       # add pvalues to dataframe so you can select species which are significant
    
    
    # current_env_scores = cbind(current_env_scores,
    #                                abrev = abbreviate(current_env_scores$Species,
    #                                                   minlength = 4, 
    #                                                   method = "both"))                #abbreviate environmental factor names
    
    significant_fish_env_scores = subset(fish_env_scores,
                                          pval <= 0.05)     #subset data to show environmental factors significant at 0.05
    
    
## 5. PERMANOVA of NMDS (species level) ----
    
    # difference in fish community based on unit? 
    
        # site assumption: do groups have homogeneous variances? 
        dis = vegdist(vegan_fishNMDS_data[,5:ncol(vegan_fishNMDS_data)], method="bray")
        mod = betadisper(dis, reference_fishNMDS$Unit)
        anova(mod)      # p<0.05, violated ... but proceeding anyways
        plot(mod)
    
    adonis2(vegan_fishNMDS_data[,5:ncol(vegan_fishNMDS_data)] ~ Unit, 
            data = reference_fishNMDS, 
            permutations = 9999,
            method = "bray")                     # difference in community based on UNIT
    
    # difference in fish community based on substrate characterization? 
    
        # site assumption: do groups have homogeneous variances? 
        dis = vegdist(vegan_fishNMDS_data[,5:ncol(vegan_fishNMDS_data)], method="bray")
        mod = betadisper(dis, reference_fishNMDS$Substrate_Characterization)
        anova(mod)      # p<0.05, violated ... but proceeding anyways
        plot(mod)
    
    adonis2(vegan_fishNMDS_data[,5:ncol(vegan_fishNMDS_data)] ~ Substrate_Characterization, 
            data = reference_fishNMDS, 
            permutations = 9999,
            method = "bray")                     # difference in community based on SUBSTRATE CHARACTERIZATION
    
    # difference in coral community based on dominant benthic habitat type? 
    
        # site assumption: do groups have homogeneous variances? 
        dis = vegdist(vegan_fishNMDS_data[,5:ncol(vegan_fishNMDS_data)], method="bray")
        mod = betadisper(dis, reference_fishNMDS$Dominant_Benthic_Habitat_Type)
        anova(mod)      # p>0.05, proceed
        plot(mod)
    
    adonis2(vegan_fishNMDS_data[,5:ncol(vegan_fishNMDS_data)] ~ Dominant_Benthic_Habitat_Type, 
            data = reference_fishNMDS, 
            permutations = 9999,
            method = "bray")                     # difference in community based on BENTHIC HABITAT TYPE
    
    
    
    
        