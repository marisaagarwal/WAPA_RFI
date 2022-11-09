# 2022-11-07


## 1. Set up ----

    # point to data locale
    data_locale = "analysis_code/"
    
    # load in the data
    source(paste0(data_locale, "analyze_coral.R"))


## 2. Plot NMDS (species level) ----

    # NMDS plot of coral communities at each unit
    plotting_coralNMDS %>%
        ggplot(aes(x = NMDS1, y = NMDS2, color = Unit)) +
        geom_point(size = 3, alpha = 0.4) +
        geom_text(label = plotting_coralNMDS$Transect) +
        stat_ellipse(linetype = 2, size = 1) +
        theme_light()
    
    
    find_hull = function(plotting_coralNMDS) plotting_coralNMDS[chull(plotting_coralNMDS$NMDS1, plotting_coralNMDS$NMDS2), ]
    hulls = ddply(plotting_coralNMDS, "Unit", find_hull)
    
    ggplot() +
        geom_point(data = plotting_coralNMDS, 
                   aes(x = NMDS1, y = NMDS2, color = Unit)) +
        geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, 
                                       fill = Unit, color = Unit),
                     alpha = 0.3) +
        theme_light()
    
    
    # how do different species contribute to NMDS separation?
    ggplot() +
        geom_point(data = plotting_coralNMDS,
                   aes(x = NMDS1, y = NMDS2, 
                       color = Unit),
                   size = 3, 
                   alpha = 0.8) +
        stat_ellipse(data = plotting_coralNMDS, 
                     aes(x = NMDS1, y = NMDS2, 
                         color = Unit),
                     linetype = 2, size = 1) +
        geom_segment(data = significant_coral_species_scores,
                     aes(x = 0, xend=NMDS1, y=0, yend=NMDS2),
                     arrow = arrow(length = unit(0.25, "cm")),
                     colour = "grey10", 
                     lwd = 0.3) +                                               # add vector arrows of significant env variables
        ggrepel::geom_text_repel(data = significant_coral_species_scores, 
                                 aes(x=NMDS1, y=NMDS2, 
                                     label = abrev),
                                 cex = 3, 
                                 direction = "both", 
                                 segment.size = 0.25) +                          # add labels for species
        theme_light()
    
    # NMDS plot of coral communities for each substrate characterization
    plotting_coralNMDS %>%
        ggplot(aes(x = NMDS1, y = NMDS2, color = Substrate_Characterization)) +
        geom_point(size = 3, alpha = 0.4) +
        # geom_text(label = plotting_coralNMDS$Transect) +
        stat_ellipse(linetype = 2, size = 1) +
        theme_light()
    
    find_hull = function(plotting_coralNMDS) plotting_coralNMDS[chull(plotting_coralNMDS$NMDS1, plotting_coralNMDS$NMDS2), ]
    hulls = ddply(plotting_coralNMDS, "Substrate_Characterization", find_hull)
    
    ggplot() +
        geom_point(data = plotting_coralNMDS, 
                   aes(x = NMDS1, y = NMDS2, color = Substrate_Characterization)) +
        geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, 
                                       fill = Substrate_Characterization, 
                                       color = Substrate_Characterization),
                     alpha = 0.3) +
        theme_light()
    
    
    # NMDS plot of coral communities for each benthic habitat type
    plotting_coralNMDS %>%
        ggplot(aes(x = NMDS1, y = NMDS2, color = Dominant_Benthic_Habitat_Type)) +
        geom_point(size = 3, alpha = 0.4) +
        # geom_text(label = plotting_coralNMDS$Transect) +
        stat_ellipse(linetype = 2, size = 1) +
        theme_light()
    
    find_hull = function(plotting_coralNMDS) plotting_coralNMDS[chull(plotting_coralNMDS$NMDS1, plotting_coralNMDS$NMDS2), ]
    hulls = ddply(plotting_coralNMDS, "Dominant_Benthic_Habitat_Type", find_hull)
    
    ggplot() +
        geom_point(data = plotting_coralNMDS, 
                   aes(x = NMDS1, y = NMDS2, color = Dominant_Benthic_Habitat_Type)) +
        geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, 
                                       fill = Dominant_Benthic_Habitat_Type, 
                                       color = Dominant_Benthic_Habitat_Type),
                     alpha = 0.3) +
        theme_light()
    

## 3. Plot species richness ----
    
    # substrate characterization
    coralsummary %>%
        group_by(Substrate_Characterization) %>%
        dplyr::summarise(mean_richness = mean(sp_richness), 
                         se_richness = std.error(sp_richness)) %>%
        ggplot(aes(x = Substrate_Characterization, y = mean_richness)) +
            geom_col() +
            geom_errorbar(aes(ymin = mean_richness-se_richness, 
                              ymax = mean_richness+se_richness))
    
    # dominant benthic habitat type
    coralsummary %>%
        group_by(Dominant_Benthic_Habitat_Type) %>%
        dplyr::summarise(mean_richness = mean(sp_richness), 
                         se_richness = std.error(sp_richness)) %>%
        ggplot(aes(x = Dominant_Benthic_Habitat_Type, y = mean_richness)) +
        geom_col() +
        geom_errorbar(aes(ymin = mean_richness-se_richness, 
                          ymax = mean_richness+se_richness)
    
    # distance from shore/crest/freshwater
    coralsummary %>%
        ggplot() +
            geom_point(aes(x = Shore_Dist, y = sp_richness), color = "blue", alpha = 0.4) +
            geom_smooth(method = "lm", aes(x = Shore_Dist, y = sp_richness), color = "blue", fill = "blue") +
            geom_point(aes(x = Crest_Dist, y = sp_richness), color = "orange", alpha = 0.4) +
            # geom_smooth(method = "lm", aes(x = Crest_Dist, y = sp_richness), color = "orange", fill = "orange") +
            geom_point(aes(x = Fresh_Dist, y = sp_richness), color = "pink", alpha = 0.4) +
            geom_smooth(method = "lm", aes(x = Fresh_Dist, y = sp_richness), color = "pink", fill = "pink") +
            labs(x = "Distance (m)", y = "Species Richness") +
            theme_light()
    
                     
## 4. Plot coral density ----

    # substrate characterization
    coralsummary %>%
        filter(Unit == "Asan") %>%
        group_by(Substrate_Characterization) %>%
        dplyr::summarise(mean_density = mean(coral_density), 
                         se_density = std.error(coral_density)) %>%
        ggplot(aes(x = Substrate_Characterization, y = mean_density)) +
        geom_col() +
        geom_errorbar(aes(ymin = mean_density-se_density, 
                          ymax = mean_density+se_density)) +
        theme_light()
    
    # dominant benthic habitat type
    coralsummary %>%
        filter(Unit == "Asan") %>%
        group_by(Dominant_Benthic_Habitat_Type) %>%
        dplyr::summarise(mean_density = mean(coral_density), 
                         se_density = std.error(coral_density)) %>%
        ggplot(aes(x = Dominant_Benthic_Habitat_Type, y = mean_density)) +
        geom_col() +
        geom_errorbar(aes(ymin = mean_density-se_density, 
                          ymax = mean_density+se_density))
    
    # distance from shore/crest/freshwater
    coralsummary %>%
        ggplot() +
            geom_point(aes(x = Shore_Dist, y = coral_density), color = "blue", alpha = 0.4) +
            # geom_smooth(method = "lm", aes(x = Shore_Dist, y = coral_density), color = "blue", fill = "blue") +
            geom_point(aes(x = Crest_Dist, y = coral_density), color = "orange", alpha = 0.4) +
            geom_smooth(method = "lm", aes(x = Crest_Dist, y = coral_density), color = "orange", fill = "orange") +
            geom_point(aes(x = Fresh_Dist, y = coral_density), color = "pink", alpha = 0.4) +
            # geom_smooth(method = "lm", aes(x = Fresh_Dist, y = coral_density), color = "pink", fill = "pink") +
            labs(x = "Distance from Transect", y = "Coral Density") +
            theme_light()
