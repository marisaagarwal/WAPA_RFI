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
        
    
    
    
    
    
        