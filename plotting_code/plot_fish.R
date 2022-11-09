# 2022-11-09


## 1. Set up ----

    # point to data locale
    data_locale = "analysis_code/"
    
    # load in the data
    source(paste0(data_locale, "analyze_fish.R"))
    
    
## 2. Plot species richness ----
    
    # substrate characterization
    fishsummary %>%
        group_by(Substrate_Characterization) %>%
        dplyr::summarise(mean_richness = mean(sp_richness), 
                         se_richness = std.error(sp_richness)) %>%
        ggplot(aes(x = Substrate_Characterization, y = mean_richness)) +
        geom_col() +
        geom_errorbar(aes(ymin = mean_richness-se_richness, 
                          ymax = mean_richness+se_richness))
    
    # dominant benthic habitat type
    fishsummary %>%
        group_by(Dominant_Benthic_Habitat_Type) %>%
        dplyr::summarise(mean_richness = mean(sp_richness), 
                         se_richness = std.error(sp_richness)) %>%
        ggplot(aes(x = Dominant_Benthic_Habitat_Type, y = mean_richness)) +
        geom_col() +
        geom_errorbar(aes(ymin = mean_richness-se_richness, 
                          ymax = mean_richness+se_richness))
                      
     # distance from shore/crest/freshwater
     fishsummary %>%
        ggplot() +
            geom_point(aes(x = Shore_Dist, y = sp_richness), color = "blue", alpha = 0.4) +
            geom_smooth(method = "lm", aes(x = Shore_Dist, y = sp_richness), color = "blue", fill = "blue") +
            geom_point(aes(x = Crest_Dist, y = sp_richness), color = "orange", alpha = 0.4) +
            geom_smooth(method = "lm", aes(x = Crest_Dist, y = sp_richness), color = "orange", fill = "orange") +
            geom_point(aes(x = Fresh_Dist, y = sp_richness), color = "pink", alpha = 0.4) +
            geom_smooth(method = "lm", aes(x = Fresh_Dist, y = sp_richness), color = "pink", fill = "pink") +
            labs(x = "Distance (m)", y = "Species Richness") +
            theme_light()
     
     
## 3. Plot density ----
     
     # substrate characterization
     fishsummary %>%
         # filter(Unit == "Asan") %>%
         group_by(Substrate_Characterization) %>%
         dplyr::summarise(mean_density = mean(fish_density), 
                          se_density = std.error(fish_density)) %>%
         ggplot(aes(x = Substrate_Characterization, y = mean_density)) +
             geom_col() +
             geom_errorbar(aes(ymin = mean_density-se_density, 
                               ymax = mean_density+se_density)) +
             theme_light()
     
     # dominant benthic habitat type
     fishsummary %>%
         group_by(Dominant_Benthic_Habitat_Type) %>%
         dplyr::summarise(mean_density = mean(fish_density), 
                          se_density = std.error(fish_density)) %>%
         ggplot(aes(x = Dominant_Benthic_Habitat_Type, y = mean_density)) +
         geom_col() +
         geom_errorbar(aes(ymin = mean_density-se_density, 
                           ymax = mean_density+se_density))
     
     # distance from shore/crest/freshwater
     fishsummary %>%
         ggplot() +
             geom_point(aes(x = Shore_Dist, y = fish_density), color = "blue", alpha = 0.4) +
             # geom_smooth(method = "lm", aes(x = Shore_Dist, y = fish_density), color = "blue", fill = "blue") +
             geom_point(aes(x = Crest_Dist, y = fish_density), color = "orange", alpha = 0.4) +
             # geom_smooth(method = "lm", aes(x = Crest_Dist, y = fish_density), color = "orange", fill = "orange") +
             geom_point(aes(x = Fresh_Dist, y = fish_density), color = "pink", alpha = 0.4) +
             geom_smooth(method = "lm", aes(x = Fresh_Dist, y = fish_density), color = "pink", fill = "pink") +
             labs(x = "Distance from Transect", y = "Fish Density") +
             theme_light()

     