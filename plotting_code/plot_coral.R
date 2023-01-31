# 2022-11-07


## 1. Set up ----

    # point to data locale
    data_locale = "analysis_code/"
    
    # load in the data
    source(paste0(data_locale, "analyze_coral.R"))
    source(paste0(data_locale, "analyze_CoralNet.R"))


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
                     lwd = 0.3) +                                               # add vector arrows of significant species
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
                     alpha = 0.15) +
        scale_fill_flat_d() +
        scale_color_flat_d() +
        # geom_segment(data = significant_coral_species_scores,
        #              aes(x = 0, xend=NMDS1, y=0, yend=NMDS2),
        #              arrow = arrow(length = unit(0.25, "cm")),
        #              colour = "grey10", 
        #              lwd = 0.3) +                                               # add vector arrows of significant species
        # ggrepel::geom_text_repel(data = significant_coral_species_scores, 
        #                          aes(x=NMDS1, y=NMDS2, 
        #                              label = abrev),
        #                          cex = 3, 
        #                          direction = "both", 
        #                          segment.size = 0.25) +                          # add labels for species
        labs(color= "Dominant Benthic Habitat Type", fill = "Dominant Benthic Habitat Type") +
        theme_pubr(legend = "right") 
    

## 3. Plot species richness ----
    
    # substrate characterization
    coralsummary %>%
        group_by(Substrate_Characterization) %>%
        dplyr::summarise(mean_richness = mean(sp_richness), 
                         se_richness = std.error(sp_richness)) %>%
        mutate(Substrate_Characterization = recode(Substrate_Characterization, 
                                                   AggregatePatchReef = "Aggregate Patch \n Reef",
                                                   AggregateReef = "Aggregate Reef",
                                                   ReefRubble = "Reef Rubble",
                                                   RockBoulder = "Rock Boulder",
                                                   SandScatteredCoral = "Sand Scattered \n Coral",
                                                   SandScatteredRock = "Sand Scattered \n Rock" )) %>%
        ggplot(aes(x = reorder(Substrate_Characterization, mean_richness), y = mean_richness, 
                   fill = Substrate_Characterization)) +
            geom_col() +
            scale_fill_flat_d() +
            geom_errorbar(aes(ymin = mean_richness-se_richness, 
                              ymax = mean_richness+se_richness, , width = 0.5)) +
            geom_signif(comparisons=list(c("Aggregate Patch \n Reef", "Sand")), annotations="**",
                        y_position = 7.9, tip_length = 0.02, vjust=0.4) +
            labs(x="Substrate Characterization", y="Mean Coral Richness (± Standard Error)") +
            theme_pubr(legend = "none")
        
    
    # dominant benthic habitat type
    coralsummary %>%
        group_by(Dominant_Benthic_Habitat_Type) %>%
        dplyr::summarise(mean_richness = mean(sp_richness), 
                         se_richness = std.error(sp_richness)) %>%
        ggplot(aes(x = reorder(Dominant_Benthic_Habitat_Type, mean_richness), y = mean_richness,
                   fill = Dominant_Benthic_Habitat_Type)) +
            geom_col() +
            scale_fill_flat_d() +
            geom_errorbar(aes(ymin = mean_richness-se_richness, 
                              ymax = mean_richness+se_richness, width = 0.5)) +
            geom_signif(comparisons=list(c("Seagrass", "Coral")), annotations="***",
                        y_position = 6.5, tip_length = 0.02, vjust=0.4) +
            geom_signif(comparisons=list(c("Uncolonized", "Coral")), annotations="*",
                        y_position = 7.2, tip_length = 0.02, vjust=0.4) +
            labs(x="Dominant Benthic Habitat Type", y="Mean Coral Richness (± Standard Error)") +
            theme_pubr(legend = "none")
    
    # distance from shore/crest/freshwater
    coralsummary %>% 
        pivot_longer(cols = c(Shore_Dist, Crest_Dist, Fresh_Dist), 
                     names_to = "Type", 
                     values_to = "Distance") %>%
        dplyr::mutate(Type = recode(Type, 
                             Fresh_Dist = "Freshwater Source", 
                             Shore_Dist = "Shore", 
                             Crest_Dist = "Reef Crest")) %>%
        ggplot(aes(x = Distance, y = sp_richness, color = Type, fill = Type)) +
            geom_point(alpha = 0.4) +
            geom_smooth(data = . %>% filter(!Type == "Reef Crest" ),
                        method = "lm") +
            scale_color_manual(values = c("pink",  "orange", "blue")) +
            scale_fill_manual(values = c("pink", "orange", "blue")) +
            labs(x = "Distance (m)", y = "Coral Species Richness", 
                 color = "Distance from:", fill = "Distance from:") +
            scale_x_continuous(limits = c(0, 1250)) +
            theme_pubr()
    
    
    
                     
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
                          ymax = mean_density+se_density, width = 0.5)) +
        theme_light()
    
    # dominant benthic habitat type
    coralsummary %>%
        # filter(Unit == "Asan") %>%
        group_by(Dominant_Benthic_Habitat_Type) %>%
        dplyr::summarise(mean_density = mean(coral_density), 
                         se_density = std.error(coral_density)) %>%
        ggplot(aes(x = reorder(Dominant_Benthic_Habitat_Type, mean_density), y = mean_density,
                   fill = Dominant_Benthic_Habitat_Type)) +
            geom_col() +
            scale_fill_flat_d() +
            geom_errorbar(aes(ymin = mean_density-se_density, 
                              ymax = mean_density+se_density, width = 0.5)) +
            geom_signif(comparisons=list(c("Seagrass", "Coral")), annotations="*",
                        y_position = 9.7, tip_length = 0.02, vjust=0.4) +
            labs(x="Dominant Benthic Habitat Type", y="Mean Coral Density (± Standard Error)") +
            theme_pubr(legend = "none")
    
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
    
    coralsummary %>% 
        pivot_longer(cols = c(Shore_Dist, Crest_Dist, Fresh_Dist), 
                     names_to = "Type", 
                     values_to = "Distance") %>%
        dplyr::mutate(Type = recode(Type, 
                                    Fresh_Dist = "Freshwater Source", 
                                    Shore_Dist = "Shore", 
                                    Crest_Dist = "Reef Crest")) %>%
        ggplot(aes(x = Distance, y = coral_density, color = Type, fill = Type)) +
            geom_point(alpha = 0.4) +
            geom_smooth(data = . %>% filter(!Type %in% c("Freshwater Source", "Shore")),
                        method = "lm") +
            scale_color_manual(values = c("pink",  "orange", "blue")) +
            scale_fill_manual(values = c("pink", "orange", "blue")) +
            labs(x = "Distance (m)", y = expression(paste("Coral Density (colonies/",m^2,")")), 
                 color = "Distance from:", fill = "Distance from:") +
            scale_x_continuous(limits = c(0, 1000)) +
            theme_pubr()
    
    expression(paste("x axis ", ring(A)^2))
    
    
## 5. Plot NMDS (genus level) ----
    
    # NMDS plot of coral communities at each unit
    plotting_coralNMDS_genus %>%
        ggplot(aes(x = NMDS1, y = NMDS2, color = Unit)) +
        geom_point(size = 3, alpha = 0.4) +
        geom_text(label = plotting_coralNMDS_genus$Transect) +
        stat_ellipse(linetype = 2, size = 1) +
        theme_light()
    
    find_hull = function(plotting_coralNMDS_genus) plotting_coralNMDS_genus[chull(plotting_coralNMDS_genus$NMDS1, plotting_coralNMDS_genus$NMDS2), ]
    hulls = ddply(plotting_coralNMDS_genus, "Unit", find_hull)
    
    ggplot() +
        geom_point(data = plotting_coralNMDS_genus, 
                   aes(x = NMDS1, y = NMDS2, color = Unit)) +
        geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, 
                                       fill = Unit, color = Unit),
                     alpha = 0.3) +
        theme_light()
    
    
    # how do different species contribute to NMDS separation?
    ggplot() +
        geom_point(data = plotting_coralNMDS_genus,
                   aes(x = NMDS1, y = NMDS2, 
                       color = Unit),
                   size = 3, 
                   alpha = 0.8) +
        stat_ellipse(data = plotting_coralNMDS_genus, 
                     aes(x = NMDS1, y = NMDS2, 
                         color = Unit),
                     linetype = 2, size = 1) +
        geom_segment(data = significant_coral_species_scores_genus,
                     aes(x = 0, xend=NMDS1, y=0, yend=NMDS2),
                     arrow = arrow(length = unit(0.25, "cm")),
                     colour = "grey10", 
                     lwd = 0.3) +                                               # add vector arrows of significant env variables
        ggrepel::geom_text_repel(data = significant_coral_species_scores_genus, 
                                 aes(x=NMDS1, y=NMDS2, 
                                     label = abrev),
                                 cex = 3, 
                                 direction = "both", 
                                 segment.size = 0.25) +                          # add labels for species
        theme_light()
    
    # NMDS plot of coral communities for each substrate characterization
    plotting_coralNMDS_genus %>%
        ggplot(aes(x = NMDS1, y = NMDS2, color = Substrate_Characterization)) +
        geom_point(size = 3, alpha = 0.4) +
        # geom_text(label = plotting_coralNMDS$Transect) +
        stat_ellipse(linetype = 2, size = 1) +
        theme_light()
    
    find_hull = function(plotting_coralNMDS_genus) plotting_coralNMDS_genus[chull(plotting_coralNMDS_genus$NMDS1, plotting_coralNMDS_genus$NMDS2), ]
    hulls = ddply(plotting_coralNMDS_genus, "Substrate_Characterization", find_hull)
    
    ggplot() +
        geom_point(data = plotting_coralNMDS_genus %>% mutate(Substrate_Characterization = recode(Substrate_Characterization, 
                                                                                              AggregatePatchReef = "Aggregate Patch Reef",
                                                                                              AggregateReef = "Aggregate Reef",
                                                                                              ReefRubble = "Reef Rubble",
                                                                                              RockBoulder = "Rock Boulder",
                                                                                              SandScatteredCoral = "Sand Scattered Coral",
                                                                                              SandScatteredRock = "Sand Scattered Rock")),  
                   aes(x = NMDS1, y = NMDS2, color = Substrate_Characterization)) +
        geom_polygon(data = hulls %>% mutate(Substrate_Characterization = recode(Substrate_Characterization, 
                                                                                AggregatePatchReef = "Aggregate Patch Reef",
                                                                                AggregateReef = "Aggregate Reef",
                                                                                ReefRubble = "Reef Rubble",
                                                                                RockBoulder = "Rock Boulder",
                                                                                SandScatteredCoral = "Sand Scattered Coral",
                                                                                SandScatteredRock = "Sand Scattered Rock")),
                     aes(x = NMDS1, y = NMDS2, 
                        fill = Substrate_Characterization, 
                        color = Substrate_Characterization), alpha = 0.15) +
        scale_fill_flat_d() +
        scale_color_flat_d() +
        geom_segment(data = significant_coral_species_scores_genus,
                     aes(x = 0, xend=NMDS1, y=0, yend=NMDS2),
                     arrow = arrow(length = unit(0.25, "cm")),
                     colour = "grey10", 
                     lwd = 0.3) +                                               # add vector arrows of significant env variables
        # geom_text(data = significant_coral_species_scores_genus,
        #            aes(x=NMDS1*1.3, y=NMDS2*1.08,label = abrev)) +
        ggrepel::geom_label_repel(data = significant_coral_species_scores_genus,
                                 aes(x=NMDS1, y=NMDS2,label = Genus),
                                 # min.segment.length = 10,
                                 # cex = 3,
                                 direction = "both",
                                 # force = 5, 
                                 point.size = 2
                                 ) +                          # add labels for species

        labs(color = "Substrate Characterization", fill = "Substrate Characterization") +
        theme_pubr(legend = "right") 
    
    
    # NMDS plot of coral communities for each benthic habitat type
    plotting_coralNMDS_genus %>%
        ggplot(aes(x = NMDS1, y = NMDS2, color = Dominant_Benthic_Habitat_Type)) +
        geom_point(size = 3, alpha = 0.4) +
        # geom_text(label = plotting_coralNMDS$Transect) +
        stat_ellipse(linetype = 2, size = 1) +
        theme_light()
    
    find_hull = function(plotting_coralNMDS_genus) plotting_coralNMDS_genus[chull(plotting_coralNMDS_genus$NMDS1, plotting_coralNMDS_genus$NMDS2), ]
    hulls = ddply(plotting_coralNMDS_genus, "Dominant_Benthic_Habitat_Type", find_hull)
    
    ggplot() +
        geom_point(data = plotting_coralNMDS_genus, 
                   aes(x = NMDS1, y = NMDS2, color = Dominant_Benthic_Habitat_Type)) +
        geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, 
                                       fill = Dominant_Benthic_Habitat_Type, 
                                       color = Dominant_Benthic_Habitat_Type),
                     alpha = 0.3) +
        geom_segment(data = significant_coral_species_scores_genus,
                     aes(x = 0, xend=NMDS1, y=0, yend=NMDS2),
                     arrow = arrow(length = unit(0.25, "cm")),
                     colour = "grey10", 
                     lwd = 0.3) +                                               # add vector arrows of significant genuses
        ggrepel::geom_text_repel(data = significant_coral_species_scores_genus, 
                                 aes(x=NMDS1, y=NMDS2, 
                                     label = abrev),
                                 cex = 3, 
                                 direction = "both", 
                                 segment.size = 0.25) +                          # add labels for genus
        theme_light()
    
    
## 6. Plot coral cover ---- 
    
    # by substrate
    coral_cover %>%
        group_by(Substrate_Characterization) %>%
        summarise(mean_cover = mean(percent_cover),
                  se_cover = std.error(percent_cover)) %>%
        mutate(Substrate_Characterization = recode(Substrate_Characterization, 
                                                   AggregatePatchReef = "Aggregate Patch \n Reef",
                                                   AggregateReef = "Aggregate Reef",
                                                   ReefRubble = "Reef Rubble",
                                                   RockBoulder = "Rock Boulder",
                                                   SandScatteredCoral = "Sand Scattered \n Coral",
                                                   SandScatteredRock = "Sand Scattered \n Rock" )) %>%
        ggplot(aes(x = reorder(Substrate_Characterization, mean_cover), y = mean_cover, 
                   fill = Substrate_Characterization)) +
        geom_col() +
        scale_fill_flat_d() +
        geom_errorbar(aes(ymin = mean_cover-se_cover, 
                          ymax = mean_cover+se_cover , width = 0.5)) +
        geom_signif(comparisons=list(c("Aggregate Patch \n Reef", "Sand")), annotations="**",
                    y_position = 57, tip_length = 0.02, vjust=0.4) +
        geom_signif(comparisons=list(c("Aggregate Reef", "Sand")), annotations="*",
                    y_position = 61, tip_length = 0.02, vjust=0.4) +
        labs(x="Substrate Characterization", y="Mean Coral Cover (± Standard Error)") +
        theme_pubr(legend = "none")

    # by benthic habitat type
    coral_cover %>%
        group_by(Dominant_Benthic_Habitat_Type) %>%
        summarise(mean_cover = mean(percent_cover),
                  se_cover = std.error(percent_cover)) %>%
        ggplot(aes(x = reorder(Dominant_Benthic_Habitat_Type, mean_cover), y = mean_cover,
                   fill = Dominant_Benthic_Habitat_Type)) +
        geom_col() +
        scale_fill_flat_d() +
        geom_errorbar(aes(ymin = mean_cover-se_cover, 
                          ymax = mean_cover+se_cover, width = 0.5)) +
        geom_signif(comparisons=list(c("Seagrass", "Coral")), annotations="***",
                    y_position = 29, tip_length = 0.02, vjust=0.4) +
        geom_signif(comparisons=list(c("Uncolonized", "Coral")), annotations="**",
                    y_position = 35, tip_length = 0.02, vjust=0.4) +
        geom_signif(comparisons=list(c("Macroalgae", "Coral")), annotations="*",
                    y_position = 38, tip_length = 0.02, vjust=0.4) +
        geom_signif(comparisons=list(c("Seagrass", "Turf")), annotations="*",
                    y_position = 32, tip_length = 0.02, vjust=0.4) +
        labs(x="Dominant Benthic Habitat Type", y="Mean Coral Cover (± Standard Error)") +
        theme_pubr(legend = "none")
    
    
    
    
    
    
    
