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
     
     
## 4. Plot NMDS (species level) ----
     
     # NMDS plot of coral communities at each unit
     plotting_fishNMDS %>%
         ggplot(aes(x = NMDS1, y = NMDS2, color = Unit)) +
         geom_point(size = 3, alpha = 0.4) +
         geom_text(label = plotting_fishNMDS$Transect) +
         stat_ellipse(linetype = 2, size = 1) +
         theme_light()
     
     
     find_hull = function(plotting_fishNMDS) plotting_fishNMDS[chull(plotting_fishNMDS$NMDS1, plotting_fishNMDS$NMDS2), ]
     hulls = ddply(plotting_fishNMDS, "Unit", find_hull)
     
     ggplot() +
         geom_point(data = plotting_fishNMDS, 
                    aes(x = NMDS1, y = NMDS2, color = Unit)) +
         geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, 
                                        fill = Unit, color = Unit),
                      alpha = 0.3) +
         theme_light()
     
     
     # how do different species contribute to NMDS separation?
     ggplot() +
         geom_point(data = plotting_fishNMDS,
                    aes(x = NMDS1, y = NMDS2, 
                        color = Unit),
                    size = 3, 
                    alpha = 0.8) +
         stat_ellipse(data = plotting_fishNMDS, 
                      aes(x = NMDS1, y = NMDS2, 
                          color = Unit),
                      linetype = 2, size = 1) +
         geom_segment(data = significant_fish_species_scores,
                      aes(x = 0, xend=NMDS1, y=0, yend=NMDS2),
                      arrow = arrow(length = unit(0.25, "cm")),
                      colour = "grey10", 
                      lwd = 0.3) +                                               # add vector arrows of significant env variables
         ggrepel::geom_text_repel(data = significant_fish_species_scores, 
                                  aes(x=NMDS1, y=NMDS2, 
                                      label = abrev),
                                  cex = 3, 
                                  direction = "both", 
                                  segment.size = 0.25) +                          # add labels for species
         theme_light()
     
     # NMDS plot of coral communities for each substrate characterization
     plotting_fishNMDS %>%
         ggplot(aes(x = NMDS1, y = NMDS2, color = Substrate_Characterization)) +
         geom_point(size = 3, alpha = 0.4) +
         # geom_text(label = plotting_fishNMDS$Transect) +
         stat_ellipse(linetype = 2, size = 1) +
         theme_light()
     
     find_hull = function(plotting_fishNMDS) plotting_fishNMDS[chull(plotting_fishNMDS$NMDS1, plotting_fishNMDS$NMDS2), ]
     hulls = ddply(plotting_fishNMDS, "Substrate_Characterization", find_hull)
     
     ggplot() +
         geom_point(data = plotting_fishNMDS, 
                    aes(x = NMDS1, y = NMDS2, color = Substrate_Characterization)) +
         geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, 
                                        fill = Substrate_Characterization, 
                                        color = Substrate_Characterization),
                      alpha = 0.3) +
         theme_light()
     
     
     # NMDS plot of coral communities for each benthic habitat type
     plotting_fishNMDS %>%
         ggplot(aes(x = NMDS1, y = NMDS2, color = Dominant_Benthic_Habitat_Type)) +
         geom_point(size = 3, alpha = 0.4) +
         # geom_text(label = plotting_fishNMDS$Transect) +
         stat_ellipse(linetype = 2, size = 1) +
         theme_light()
     
     find_hull = function(plotting_fishNMDS) plotting_fishNMDS[chull(plotting_fishNMDS$NMDS1, plotting_fishNMDS$NMDS2), ]
     hulls = ddply(plotting_fishNMDS, "Dominant_Benthic_Habitat_Type", find_hull)
     
     ggplot() +
         geom_point(data = plotting_fishNMDS, 
                    aes(x = NMDS1, y = NMDS2, color = Dominant_Benthic_Habitat_Type)) +
         geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, 
                                        fill = Dominant_Benthic_Habitat_Type, 
                                        color = Dominant_Benthic_Habitat_Type),
                      alpha = 0.3) +
         theme_light()
     
     
## 5. Plot biomass and length ----
     
     fish_biomass %>%
         ggplot(aes(y = weight)) +
            geom_boxplot() +
            theme_light()
     
     fish_biomass %>%
         ggplot(aes(x = Unit, y = weight)) +
         # geom_boxplot() +
         # geom_jitter(alpha = 0.1) +
         geom_violin()+
         theme_light()
     
     
     # length vs. weight
     fish_biomass %>%
         ggplot(aes(x = Total_Length, y = weight)) +
            geom_point() +
            theme_light()
     
     # by species
     
        # weight
        fish_biomass %>%
            group_by(Unit, Species_Code) %>%
            dplyr::summarise(mean_tl = mean(Total_Length),
                            se_tl = std.error(Total_Length),
                            mean_weight = mean(weight), 
                            se_weight = std.error(weight)) %>%
            dplyr::filter(!Species_Code %in% unpaired_unit_fish_species) %>%
            ggplot(aes(x = Species_Code, y = mean_weight, fill = Unit)) +
                geom_col(position="dodge") +
                geom_errorbar(aes(ymin = mean_weight-se_weight, 
                                  ymax = mean_weight+se_weight), 
                              position = position_dodge(width = 0.9)) +
                theme_light()

        # substrate
        fish_biomass %>%
            group_by(Substrate_Characterization, Species_Code) %>%
            dplyr::summarise(mean_tl = mean(Total_Length),
                             se_tl = std.error(Total_Length),
                             mean_weight = mean(weight), 
                             se_weight = std.error(weight)) %>%
            dplyr::filter(Species_Code == "MUFL") %>%
            ggplot(aes(x = Species_Code, y = mean_weight, fill = Substrate_Characterization)) +
                geom_col(position="dodge") +
                geom_errorbar(aes(ymin = mean_weight-se_weight, 
                                  ymax = mean_weight+se_weight), 
                              position = position_dodge(width = 0.9)) +
                theme_light()
        
     
     # by substrate characterization
     
        # weight
        fish_biomass %>%
         group_by(Substrate_Characterization) %>%
         dplyr::summarise(mean_tl = mean(Total_Length),
                          se_tl = std.error(Total_Length),
                          mean_weight = mean(weight), 
                          se_weight = std.error(weight)) %>%
            ggplot(aes(x = Substrate_Characterization, y = mean_weight)) +
                geom_col() +
                geom_errorbar(aes(ymin = mean_weight-se_weight, 
                                  ymax = mean_weight+se_weight)) +
                theme_light()
        
        fish_biomass %>%
            group_by(Unit, Substrate_Characterization) %>%
            dplyr::summarise(mean_tl = mean(Total_Length),
                             se_tl = std.error(Total_Length),
                             mean_weight = mean(weight), 
                             se_weight = std.error(weight)) %>%
            ggplot(aes(x = Substrate_Characterization, y = mean_weight)) +
                geom_col() +
                geom_errorbar(aes(ymin = mean_weight-se_weight, 
                                  ymax = mean_weight+se_weight)) +
                facet_wrap(~Unit) +
                theme_light()
        
        # length
        fish_biomass %>%
            group_by(Substrate_Characterization) %>%
            dplyr::summarise(mean_tl = mean(Total_Length),
                             se_tl = std.error(Total_Length),
                             mean_weight = mean(weight), 
                             se_weight = std.error(weight)) %>%
            ggplot(aes(x = Substrate_Characterization, y = mean_tl)) +
                geom_col() +
                geom_errorbar(aes(ymin = mean_tl-se_tl, 
                                  ymax = mean_tl+se_tl)) +
                theme_light()
        
        fish_biomass %>%
            group_by(Unit, Substrate_Characterization) %>%
            dplyr::summarise(mean_tl = mean(Total_Length),
                             se_tl = std.error(Total_Length),
                             mean_weight = mean(weight), 
                             se_weight = std.error(weight)) %>%
            ggplot(aes(x = Substrate_Characterization, y = mean_tl)) +
                geom_col() +
                geom_errorbar(aes(ymin = mean_tl-se_tl, 
                                  ymax = mean_tl+se_tl)) +
                facet_wrap(~Unit) +
                theme_light()
        
    # by dominant benthic habitat type
        
        # weight
        fish_biomass %>%
            group_by(Dominant_Benthic_Habitat_Type) %>%
            dplyr::summarise(mean_tl = mean(Total_Length),
                             se_tl = std.error(Total_Length),
                             mean_weight = mean(weight), 
                             se_weight = std.error(weight)) %>%
            ggplot(aes(x = Dominant_Benthic_Habitat_Type, y = mean_weight)) +
                geom_col() +
                geom_errorbar(aes(ymin = mean_weight-se_weight, 
                                  ymax = mean_weight+se_weight)) +
                theme_light()
        
        fish_biomass %>%
            group_by(Unit, Dominant_Benthic_Habitat_Type) %>%
            dplyr::summarise(mean_tl = mean(Total_Length),
                             se_tl = std.error(Total_Length),
                             mean_weight = mean(weight), 
                             se_weight = std.error(weight)) %>%
            ggplot(aes(x = Dominant_Benthic_Habitat_Type, y = mean_weight)) +
                geom_col() +
                geom_errorbar(aes(ymin = mean_weight-se_weight, 
                                  ymax = mean_weight+se_weight)) +
                facet_wrap(~Unit) +
                theme_light()
        
        # length
        fish_biomass %>%
            group_by(Dominant_Benthic_Habitat_Type) %>%
            dplyr::summarise(mean_tl = mean(Total_Length),
                             se_tl = std.error(Total_Length),
                             mean_weight = mean(weight), 
                             se_weight = std.error(weight)) %>%
            ggplot(aes(x = Dominant_Benthic_Habitat_Type, y = mean_tl)) +
                geom_col() +
                geom_errorbar(aes(ymin = mean_tl-se_tl, 
                                  ymax = mean_tl+se_tl)) +
                theme_light()
        
        fish_biomass %>%
            group_by(Unit, Dominant_Benthic_Habitat_Type) %>%
            dplyr::summarise(mean_tl = mean(Total_Length),
                             se_tl = std.error(Total_Length),
                             mean_weight = mean(weight), 
                             se_weight = std.error(weight)) %>%
            ggplot(aes(x = Dominant_Benthic_Habitat_Type, y = mean_tl)) +
                geom_col() +
                geom_errorbar(aes(ymin = mean_tl-se_tl, 
                                  ymax = mean_tl+se_tl)) +
                facet_wrap(~Unit) +
                theme_light()
        
        
        
        
     