# 2022-11-09


## 1. Set up ----

    # point to data locale
    data_locale = "analysis_code/"
    
    # load in the data
    source(paste0(data_locale, "analyze_fish.R"))
    
    
## 2. Plot species richness ----
    
    # unit
    fishsummary %>%
        group_by(Substrate_Characterization) %>%
        dplyr::summarise(mean_richness = mean(sp_richness), 
                         se_richness = std.error(sp_richness)) %>%
        ggplot(aes(x = Unit, y = mean_density,
                   fill = Unit)) +
            geom_col(alpha = 0.8) +
            scale_fill_viridis_d() +
            geom_errorbar(aes(ymin = mean_density-se_density,
                              ymax = mean_density+se_density, width = 0.5)) +
            labs(x="WAPA Management Unit", y="Mean Fish Richness (± Standard Error)") +
            theme_pubr(legend = "none")
    
    # substrate characterization
    fishsummary %>%
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
                              ymax = mean_richness+se_richness, width = 0.5)) +
            geom_signif(comparisons=list(c("Aggregate Patch \n Reef", "Reef Rubble")), annotations="*",
                         y_position = 35, tip_length = 0.02, vjust=0.4) +
            geom_signif(comparisons=list(c("Aggregate Patch \n Reef", "Sand")), annotations="***",
                        y_position = 32, tip_length = 0.02, vjust=0.4) +
            geom_signif(comparisons=list(c("Sand Scattered \n Rock", "Sand")), annotations="*",
                        y_position = 29, tip_length = 0.02, vjust=0.4) +
            labs(x="Substrate Characterization", y="Mean Fish Richness (± Standard Error)") +
            theme_pubr(legend = "none")
    
    # dominant benthic habitat type
    fishsummary %>%
        group_by(Dominant_Benthic_Habitat_Type) %>%
        dplyr::summarise(mean_richness = mean(sp_richness), 
                         se_richness = std.error(sp_richness)) %>%
        mutate(Dominant_Benthic_Habitat_Type = recode(Dominant_Benthic_Habitat_Type, 
                                                   AggregatePatchReef = "Aggregate Patch \n Reef",
                                                   AggregateReef = "Aggregate Reef",
                                                   ReefRubble = "Reef Rubble",
                                                   RockBoulder = "Rock Boulder",
                                                   SandScatteredCoral = "Sand Scattered \n Coral",
                                                   SandScatteredRock = "Sand Scattered \n Rock" )) %>%
        ggplot(aes(x = reorder(Dominant_Benthic_Habitat_Type, mean_richness), y = mean_richness, 
                   fill = Dominant_Benthic_Habitat_Type)) +
            geom_col() +
            scale_fill_flat_d() +
            geom_errorbar(aes(ymin = mean_richness-se_richness, 
                              ymax = mean_richness+se_richness, width = 0.5)) +
            geom_signif(comparisons=list(c("Seagrass", "Turf")), annotations="*",
                        y_position = 25, tip_length = 0.02, vjust=0.4) +
            geom_signif(comparisons=list(c("Coral", "Seagrass")), annotations="***",
                        y_position = 27, tip_length = 0.02, vjust=0.4) +
            geom_signif(comparisons=list(c("Coral", "Uncolonized")), annotations="*",
                        y_position = 29, tip_length = 0.02, vjust=0.4) +
            labs(x="Dominant Benthic Habitat Type", y="Mean Fish Richness (± Standard Error)") +
            theme_pubr(legend = "none")
                      
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
     
     fishsummary %>% 
         pivot_longer(cols = c(Shore_Dist, Crest_Dist, Fresh_Dist), 
                      names_to = "Type", 
                      values_to = "Distance") %>%
         dplyr::mutate(Type = recode(Type, 
                                     Fresh_Dist = "Freshwater Source", 
                                     Shore_Dist = "Shore", 
                                     Crest_Dist = "Reef Crest")) %>%
         ggplot(aes(x = Distance, y = sp_richness, color = Type, fill = Type)) +
             geom_point(alpha = 0.4) +
             geom_smooth(data = . %>% filter(!Type %in% c("Reef Crest", "Shore")),
                         method = "lm") +
             scale_color_manual(values = c("pink",  "orange", "blue")) +
             scale_fill_manual(values = c("pink", "orange", "blue")) +
             labs(x = "Distance (m)", y = "Fish Species Richness", 
                  color = "Distance from:", fill = "Distance from:") +
             scale_x_continuous(limits = c(0, 1250)) +
             theme_pubr()
     
     
## 3. Plot density ----
     
     # by unit
     fish_density %>%
         group_by(Unit) %>%
         summarise(mean_density = mean(fish_density), 
                   se_density = std.error(fish_density)) %>%
         ggplot(aes(x = Unit, y = mean_density, 
                    fill = Unit)) +
         geom_col(alpha = 0.8) +
         scale_fill_viridis_d() +
         geom_errorbar(aes(ymin = mean_density-se_density, 
                           ymax = mean_density+se_density, width = 0.5)) +
         geom_signif(comparisons=list(c("Asan", "Agat")), annotations="*",
                     y_position = 2.3, tip_length = 0.02, vjust=0.4) +
         # scale_y_continuous(limits = c(0, 14), n.breaks = 6) + 
         labs(x="WAPA Management Unit", y="Mean Density (fish/m2)") +
         theme_pubr(legend = "none")
     
     # substrate characterization
     fishsummary %>%
         group_by(Substrate_Characterization) %>%
         dplyr::summarise(mean_density = mean(fish_density), 
                          se_density = std.error(fish_density)) %>%
         mutate(Substrate_Characterization = recode(Substrate_Characterization, 
                                                    AggregatePatchReef = "Aggregate Patch \n Reef",
                                                    AggregateReef = "Aggregate Reef",
                                                    ReefRubble = "Reef Rubble",
                                                    RockBoulder = "Rock Boulder",
                                                    SandScatteredCoral = "Sand Scattered \n Coral",
                                                    SandScatteredRock = "Sand Scattered \n Rock" )) %>%
         ggplot(aes(x = reorder(Substrate_Characterization, mean_density), y = mean_density, 
                    fill = Substrate_Characterization)) +
             geom_col() +
             scale_fill_flat_d() +
             geom_errorbar(aes(ymin = mean_density - se_density, 
                               ymax = mean_density + se_density, width = 0.5)) +
             labs(x="Substrate Characterization", y="Mean Fish Density (± Standard Error)") +
             theme_pubr(legend = "none")
     
    # dominant benthic habitat type
    fishsummary %>%
        group_by(Dominant_Benthic_Habitat_Type) %>%
        dplyr::summarise(mean_density = mean(fish_density), 
                          se_density = std.error(fish_density)) %>%
        ggplot(aes(x = reorder(Dominant_Benthic_Habitat_Type, mean_density), y = mean_density,
                   fill = Dominant_Benthic_Habitat_Type)) +
            geom_col() +
            scale_fill_flat_d() +
            geom_errorbar(aes(ymin = mean_density - se_density, 
                              ymax = mean_density + se_density, width = 0.5)) +
            geom_signif(comparisons=list(c("Seagrass", "Coral")), annotations="**",
                         y_position = 3.1, tip_length = 0.02, vjust=0.4) +
            geom_signif(comparisons=list(c("Macroalgae", "Seagrass")), annotations="*",
                         y_position = 3.3, tip_length = 0.02, vjust=0.4) +
            labs(x="Dominant Benthic Habitat Type", y="Mean Fish Density (± Standard Error)") +
            theme_pubr(legend = "none")
         
     # distance from shore/crest/freshwater
        fishsummary %>% 
            pivot_longer(cols = c(Shore_Dist, Crest_Dist, Fresh_Dist), 
                         names_to = "Type", 
                         values_to = "Distance") %>%
            dplyr::mutate(Type = recode(Type, 
                                        Fresh_Dist = "Freshwater Source", 
                                        Shore_Dist = "Shore", 
                                        Crest_Dist = "Reef Crest")) %>%
            ggplot(aes(x = Distance, y = fish_density, color = Type, fill = Type)) +
                geom_point(alpha = 0.4) +
                geom_smooth(data = . %>% filter(!Type %in% c("Reef Crest", "Shore")),
                            method = "lm") +
                scale_color_manual(values = c("pink",  "orange", "blue")) +
                scale_fill_manual(values = c("pink", "orange", "blue")) +
                labs(x = "Distance (m)", y = expression(paste("Fish Density (fish/",m^2,")")), 
                     color = "Distance from:", fill = "Distance from:") +
                scale_x_continuous(limits = c(0, 1250)) +
                theme_pubr()
     
     
## 4. Plot NMDS (species level) ----
     
     # NMDS plot of fish communities at each unit
     plotting_fishNMDS %>%
         ggplot(aes(x = NMDS1, y = NMDS2, color = Unit)) +
         geom_point(size = 3, alpha = 0.4) +
         geom_text(label = plotting_fishNMDS$Transect) +
         stat_ellipse(linetype = 2, size = 1) +
         theme_light()
     
     
     find_hull = function(plotting_fishNMDS) plotting_fishNMDS[chull(plotting_fishNMDS$NMDS1, plotting_fishNMDS$NMDS2), ]
     hulls = ddply(plotting_fishNMDS, "Unit", find_hull)
     
     ggplot() +
         geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, 
                                        fill = Unit, color = Unit),
                      alpha = 0.3) +
         geom_point(data = plotting_fishNMDS, 
                    aes(x = NMDS1, y = NMDS2, color = Unit)) +
         scale_color_viridis_d() +
         scale_fill_viridis_d() +
         geom_label(aes(label = c("Agat", "Asan"), x = c(1.8, -0.5), y = c(0.85, 0.65)),
                    color = "black", fill = c("#440154", "#fde725"), size = 3, alpha = 0.3) +
         theme_bw() +
         labs_pubr() +
         rremove("grid") +
         rremove("legend")
     
     
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
         
         geom_polygon(data = hulls %>%
                          mutate(Substrate_Characterization = recode(Substrate_Characterization, 
                                                                     AggregatePatchReef = "Aggregate Patch Reef",
                                                                     AggregateReef = "Aggregate Reef",
                                                                     ReefRubble = "Reef Rubble",
                                                                     RockBoulder = "Rock Boulder",
                                                                     SandScatteredCoral = "Sand Scattered Coral",
                                                                     SandScatteredRock = "Sand Scattered Rock" )),
                      aes(x = NMDS1, y = NMDS2, 
                                        fill = Substrate_Characterization, 
                                        color = Substrate_Characterization),
                      alpha = 0.3) +
         geom_point(data = plotting_fishNMDS %>%
                        mutate(Substrate_Characterization = recode(Substrate_Characterization, 
                                                                   AggregatePatchReef = "Aggregate Patch Reef",
                                                                   AggregateReef = "Aggregate Reef",
                                                                   ReefRubble = "Reef Rubble",
                                                                   RockBoulder = "Rock Boulder",
                                                                   SandScatteredCoral = "Sand Scattered Coral",
                                                                   SandScatteredRock = "Sand Scattered Rock")), 
                    aes(x = NMDS1, y = NMDS2, color = Substrate_Characterization)) +
         scale_color_flat_d() +
         scale_fill_flat_d() +
         theme_bw() +
         labs_pubr() +
         rremove("grid") +
         labs(fill = "Substrate Characterization", color = "Substrate Characterization")
     
     
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
         scale_color_flat_d() +
         scale_fill_flat_d() +
         theme_bw() +
         labs_pubr() +
         rremove("grid") +
         labs(fill = "Dominant Benthic Habitat Type", color = "Dominant Benthic Habitat Type")
     
     
## 5. Plot biomass and length ----
     
     # overall 
     
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
     
     fish_biomass %>%                                           # length vs. weight relationship
         ggplot(aes(x = Total_Length, y = weight, color = Family)) +
         geom_point(alpha = 0.5) +
         # geom_smooth(formula = y ~ -x^2) +
         scale_color_viridis_d() +
         facet_wrap(~Family) +
         theme_light()
    
     LW_relationship = 
         merge(fish_biomass,fishcodes) %>%
         dplyr::select(c(Family, Taxon_Name, Functional_Trophic_Group, Unit, Transect, 
                         Total_Length, weight, Substrate_Characterization, 
                         Dominant_Benthic_Habitat_Type, A_value, B_value))
     
             # LW_relationship %>%                                           # length vs. weight relationship (FAMILIES WITH 3+ OBSERVATIONS)
             #     group_by(Family) %>%
             #     filter(n() >= 3) %>%
             #         ggplot(aes(x = Total_Length, y = weight, color = Family)) +
             #             geom_point(alpha = 0.5) +
             #             geom_smooth(formula = y ~ A_value*x^B_value, 
             #                         method="nls", 
             #                         se = F) +
             #             scale_color_viridis_d() +
             #             facet_wrap(~Family) +
             #             theme_light()
     
     average_parameters = 
         LW_relationship %>%
             group_by(Family) %>%
             summarise(mean_A_value = mean(A_value), 
                       mean_B_value = mean(B_value)) %>%
             mutate(log_mean_A_value = log(mean_A_value))
     
     LW_relationship = merge(LW_relationship, average_parameters)
     
     LW_relationship %>%                                             # log-log length-weight relationship (FAMILIES WITH 3+ OBSERVATIONS)
         group_by(Family) %>%
         filter(n() >= 3) %>%
         filter(!Family == "Carangidae") %>%
            ggplot(aes(x = log(Total_Length), y = log(weight), color = Family)) +
                 geom_abline(aes(intercept = log_mean_A_value, slope = mean_B_value, 
                                 color = Family), 
                             linetype = "dashed") +
                 geom_point(alpha = 0.7) +
                 scale_color_viridis_d() +
                 # facet_wrap(~Family, ncol = 3) + 
                 facet_wrap_paginate(~Family, ncol = 3, nrow = 3, page = 1) +
                 scale_x_continuous(limits = c(1, 3)) +
                 scale_y_continuous(limits = c(0, 4.5)) +
                 geom_text(data = test %>%
                                   filter(!Family %in% c("Carangidae", "Fistulariidae", 
                                                         "Lethrinidae", "Lutjanidae", 
                                                         "Ptereleotridae", "Scorpaenidae", 
                                                         "Synodontidae")), 
                           aes(label = paste('log(W) =', round(log_mean_A_value, 2), "+", round(mean_B_value, 2), "(log(TL))")),
                           x = 1, y = 4, vjust = 0, hjust = 0,
                           show.legend = F) +
                 geom_text(data = LW_relationship %>% 
                               group_by(Family) %>% 
                               tally() %>%
                               filter(n >= 3) %>%
                               filter(!Family == "Carangidae"), 
                           aes(label=paste('n =', n)), x = 1, y = 3.2, vjust = 0, hjust = 0, 
                           show.legend = F) +
                labs(x = "log(Total Length)  (cm)", y = "log(Weight)  (g)") +
                theme_pubr(legend = "none")
    
     

    # by unit 
        
        # biomass
         fish_biomass %>%
             group_by(Unit) %>%
             summarise(mean_biomass = mean(weight), 
                       se_biomass = std.error(weight)) %>%
             ggplot(aes(x = Unit, y = mean_biomass, 
                        fill = Unit)) +
                 geom_col(alpha = 0.8) +
                 scale_fill_viridis_d() +
                 geom_errorbar(aes(ymin = mean_biomass-se_biomass, 
                                   ymax = mean_biomass+se_biomass, width = 0.5)) +
                 geom_signif(comparisons=list(c("Asan", "Agat")), annotations="***",
                             y_position = 13.7, tip_length = 0.02, vjust=0.4) +
                scale_y_continuous(limits = c(0, 14), n.breaks = 6) + 
                 labs(x="WAPA Management Unit", y="Mean Individual Biomass (g)") +
                 theme_pubr(legend = "none")
     
         # length
         fish_biomass %>%
             group_by(Unit) %>%
             summarise(mean_TL = mean(Total_Length), 
                       se_TL = std.error(Total_Length)) %>%
             ggplot(aes(x = Unit, y = mean_TL, 
                        fill = Unit)) +
             geom_col(alpha = 0.8) +
             scale_fill_viridis_d() +
             geom_errorbar(aes(ymin = mean_TL-se_TL, 
                               ymax = mean_TL+se_TL, width = 0.5)) +
             geom_signif(comparisons=list(c("Asan", "Agat")), annotations="***",
                         y_position = 8, tip_length = 0.02, vjust=0.4) +
             labs(x="WAPA Management Unit", y="Mean Total Length (cm)") +
             theme_pubr(legend = "none")
     
         
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
            # dplyr::filter(Species_Code == "MUFL") %>%
            ggplot(aes(x = Species_Code, y = mean_weight, fill = Substrate_Characterization)) +
                geom_col(position="dodge") +
                geom_errorbar(aes(ymin = mean_weight-se_weight, 
                                  ymax = mean_weight+se_weight), 
                              position = position_dodge(width = 0.9)) +
                theme_light()
        
        # benthic habitat
        fish_biomass %>%
            group_by(Dominant_Benthic_Habitat_Type, Species_Code) %>%
            dplyr::summarise(mean_tl = mean(Total_Length),
                             se_tl = std.error(Total_Length),
                             mean_weight = mean(weight), 
                             se_weight = std.error(weight)) %>%
            # dplyr::filter(Species_Code == "SIAR") %>%
            ggplot(aes(x = Species_Code, y = mean_weight, fill = Dominant_Benthic_Habitat_Type)) +
                geom_col(position="dodge") +
                geom_errorbar(aes(ymin = mean_weight-se_weight, 
                                  ymax = mean_weight+se_weight), 
                              position = position_dodge(width = 0.9)) +
                theme_light()
        
     
     # by substrate characterization
     
        # weight
        fish_biomass %>%
         group_by(Substrate_Characterization) %>%
         summarise(mean_tl = mean(Total_Length),
                          se_tl = std.error(Total_Length),
                          mean_weight = mean(weight), 
                          se_weight = std.error(weight)) %>%
         mutate(Substrate_Characterization = recode(Substrate_Characterization, 
                                                       AggregatePatchReef = "Aggregate Patch \n Reef",
                                                       AggregateReef = "Aggregate Reef",
                                                       ReefRubble = "Reef Rubble",
                                                       RockBoulder = "Rock Boulder",
                                                       SandScatteredCoral = "Sand Scattered \n Coral",
                                                       SandScatteredRock = "Sand Scattered \n Rock" )) %>%
            ggplot(aes(x = reorder(Substrate_Characterization, mean_weight), y = mean_weight, 
                       fill = Substrate_Characterization)) +
                geom_col() +
                scale_fill_flat_d() +
                geom_errorbar(aes(ymin = mean_weight-se_weight, 
                                  ymax = mean_weight+se_weight), width = 0.5) +
                labs(x="Substrate Characterization", y="Mean Individual Biomass (g) (± Standard Error)") +
                theme_pubr(legend = "none")
        
        # length
        fish_biomass %>%
            group_by(Substrate_Characterization) %>%
            dplyr::summarise(mean_tl = mean(Total_Length),
                             se_tl = std.error(Total_Length),
                             mean_weight = mean(weight), 
                             se_weight = std.error(weight)) %>%
            mutate(Substrate_Characterization = recode(Substrate_Characterization, 
                                                       AggregatePatchReef = "Aggregate Patch \n Reef",
                                                       AggregateReef = "Aggregate Reef",
                                                       ReefRubble = "Reef Rubble",
                                                       RockBoulder = "Rock Boulder",
                                                       SandScatteredCoral = "Sand Scattered \n Coral",
                                                       SandScatteredRock = "Sand Scattered \n Rock" )) %>%
            ggplot(aes(x = reorder(Substrate_Characterization, mean_tl), y = mean_tl,
                       fill = Substrate_Characterization)) +
                geom_col() +
                scale_fill_flat_d() +
                geom_errorbar(aes(ymin = mean_tl-se_tl, 
                                  ymax = mean_tl+se_tl), width = 0.5) +
                labs(x="Substrate Characterization", y="Mean Individual Total Length (cm) (± Standard Error)") +
                theme_pubr(legend = "none")
        
        
    # by dominant benthic habitat type
        
        # weight
        fish_biomass %>%
            group_by(Dominant_Benthic_Habitat_Type) %>%
            summarise(mean_tl = mean(Total_Length),
                      se_tl = std.error(Total_Length),
                      mean_weight = mean(weight), 
                      se_weight = std.error(weight)) %>%
            ggplot(aes(x = reorder(Dominant_Benthic_Habitat_Type, mean_weight), y = mean_weight, 
                       fill = Dominant_Benthic_Habitat_Type)) +
                geom_col() +
                scale_fill_flat_d() +
                geom_errorbar(aes(ymin = mean_weight-se_weight, 
                                  ymax = mean_weight+se_weight), width = 0.5) +
                geom_signif(comparisons=list(c("Coral", "Macroalgae")), annotations="***",
                            y_position = 25, tip_length = 0.02, vjust=0.4) +
                geom_signif(comparisons=list(c("Coral", "Turf")), annotations="***",
                            y_position = 28, tip_length = 0.02, vjust=0.4) +
                geom_signif(comparisons=list(c("Turf", "Macroalgae")), annotations="***",
                            y_position = 22, tip_length = 0.02, vjust=0.4) +
                geom_signif(comparisons=list(c("Uncolonized", "Macroalgae")), annotations="**",
                            y_position = 31, tip_length = 0.02, vjust=0.4) +
                labs(x="Dominant Benthic Habitat Type", y="Mean Individual Biomass (g) (± Standard Error)") +
                theme_pubr(legend = "none")
        
        # length
        fish_biomass %>%
            group_by(Dominant_Benthic_Habitat_Type) %>%
            dplyr::summarise(mean_tl = mean(Total_Length),
                             se_tl = std.error(Total_Length),
                             mean_weight = mean(weight), 
                             se_weight = std.error(weight)) %>%
            ggplot(aes(x = reorder(Dominant_Benthic_Habitat_Type, mean_tl), y = mean_tl,
                       fill = Dominant_Benthic_Habitat_Type)) +
                geom_col() +
                scale_fill_flat_d() +
                geom_errorbar(aes(ymin = mean_tl-se_tl, 
                                  ymax = mean_tl+se_tl), width = 0.5) +
                geom_signif(comparisons=list(c("Uncolonized", "Macroalgae")), annotations="**",
                            y_position = 12, tip_length = 0.02, vjust=0.4) +
                geom_signif(comparisons=list(c("Coral", "Turf")), annotations="***",
                            y_position = 11, tip_length = 0.02, vjust=0.4) +
                geom_signif(comparisons=list(c("Turf", "Macroalgae")), annotations="***",
                            y_position = 10, tip_length = 0.02, vjust=0.4) +
                labs(x="Dominant Benthic Habitat Type", y="Mean Individual Total Length (cm) (± Standard Error)") +
                theme_pubr(legend = "none")
        
        
    # by distance from shore/reef crest/freshwater
        
        # weight
        fish_biomass %>%
            group_by(Unit, Transect, Crest_Dist, Shore_Dist, Fresh_Dist) %>%
            summarise(sum_total_length = sum(Total_Length),
                      sum_biomass = sum(weight))  %>% 
            pivot_longer(cols = c(Shore_Dist, Crest_Dist, Fresh_Dist), 
                         names_to = "Type", 
                         values_to = "Distance") %>%
            dplyr::mutate(Type = recode(Type, 
                                        Fresh_Dist = "Freshwater Source", 
                                        Shore_Dist = "Shore", 
                                        Crest_Dist = "Reef Crest")) %>%
            ggplot(aes(x = Distance, y = sum_biomass, color = Type, fill = Type)) +
                geom_point(alpha = 0.4) +
                geom_smooth(data = . %>% filter(!Type %in% c("Reef Crest", "Shore")),
                            method = "lm") +
                scale_color_manual(values = c("pink",  "orange", "blue")) +
                scale_fill_manual(values = c("pink", "orange", "blue")) +
                labs(x = "Distance (m)", y = "Total Fish Biomass per Transect (g)", 
                     color = "Distance from:", fill = "Distance from:") +
                scale_x_continuous(limits = c(0, 1250)) +
                theme_pubr()
        
        # length
        fish_biomass %>%
            group_by(Unit, Transect, Crest_Dist, Shore_Dist, Fresh_Dist) %>%
            summarise(sum_total_length = sum(Total_Length),
                      sum_biomass = sum(weight))  %>% 
            pivot_longer(cols = c(Shore_Dist, Crest_Dist, Fresh_Dist), 
                         names_to = "Type", 
                         values_to = "Distance") %>%
            dplyr::mutate(Type = recode(Type, 
                                        Fresh_Dist = "Freshwater Source", 
                                        Shore_Dist = "Shore", 
                                        Crest_Dist = "Reef Crest")) %>%
            ggplot(aes(x = Distance, y = sum_total_length, color = Type, fill = Type)) +
                geom_point(alpha = 0.4) +
                geom_smooth(data = . %>% filter(!Type %in% c("Reef Crest", "Shore")),
                            method = "lm") +
                scale_color_manual(values = c("pink",  "orange", "blue")) +
                scale_fill_manual(values = c("pink", "orange", "blue")) +
                labs(x = "Distance (m)", y = "Total Fish Length per Transect (cm)", 
                     color = "Distance from:", fill = "Distance from:") +
                scale_x_continuous(limits = c(0, 1250)) +
                theme_pubr()
        
        
## 6. Plot NMDS (family level) ----
        
        # NMDS plot of fish communities at each unit
        plotting_fishNMDS_family %>%
            ggplot(aes(x = NMDS1, y = NMDS2, color = Unit)) +
            geom_point(size = 3, alpha = 0.4) +
            geom_text(label = plotting_fishNMDS_family$Transect) +
            stat_ellipse(linetype = 2, size = 1) +
            theme_light()
        
        find_hull = function(plotting_fishNMDS_family) plotting_fishNMDS_family[chull(plotting_fishNMDS_family$NMDS1, plotting_fishNMDS_family$NMDS2), ]
        hulls = ddply(plotting_fishNMDS_family, "Unit", find_hull)
        
        ggplot() +
            geom_point(data = plotting_fishNMDS_family, 
                       aes(x = NMDS1, y = NMDS2, color = Unit)) +
            geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, 
                                           fill = Unit, color = Unit),
                         alpha = 0.3) +
            # geom_segment(data = significant_fish_species_scores_family,
            #              aes(x = 0, xend=NMDS1, y=0, yend=NMDS2),
            #              arrow = arrow(length = unit(0.25, "cm")),
            #              colour = "grey10", 
            #              lwd = 0.3) +                                               # add vector arrows of significant env variables
            # ggrepel::geom_text_repel(data = significant_fish_species_scores_family, 
            #                          aes(x=NMDS1, y=NMDS2, 
            #                              label = abrev),
            #                          cex = 3, 
            #                          direction = "both", 
            #                          segment.size = 0.25) +                          # add labels for species
            theme_light()
        
        
        # how do different species contribute to NMDS separation?
        ggplot() +
            geom_point(data = plotting_fishNMDS_family,
                       aes(x = NMDS1, y = NMDS2, 
                           color = Unit),
                       size = 3, 
                       alpha = 0.8) +
            stat_ellipse(data = plotting_fishNMDS_family, 
                         aes(x = NMDS1, y = NMDS2, 
                             color = Unit),
                         linetype = 2, size = 1) +
            geom_segment(data = significant_fish_species_scores_family,
                         aes(x = 0, xend=NMDS1, y=0, yend=NMDS2),
                         arrow = arrow(length = unit(0.25, "cm")),
                         colour = "grey10", 
                         lwd = 0.3) +                                               # add vector arrows of significant env variables
            ggrepel::geom_text_repel(data = significant_fish_species_scores_family, 
                                     aes(x=NMDS1, y=NMDS2, 
                                         label = abrev),
                                     cex = 3, 
                                     direction = "both", 
                                     segment.size = 0.25) +                          # add labels for species
            theme_light()
        
        # NMDS plot of coral communities for each substrate characterization
        plotting_fishNMDS_family %>%
            ggplot(aes(x = NMDS1, y = NMDS2, color = Substrate_Characterization)) +
            geom_point(size = 3, alpha = 0.4) +
            # geom_text(label = plotting_fishNMDS_family$Transect) +
            stat_ellipse(linetype = 2, size = 1) +
            theme_light()
        
        find_hull = function(plotting_fishNMDS_family) plotting_fishNMDS_family[chull(plotting_fishNMDS_family$NMDS1, plotting_fishNMDS_family$NMDS2), ]
        hulls = ddply(plotting_fishNMDS_family, "Substrate_Characterization", find_hull)
        
        ggplot() +
            geom_point(data = plotting_fishNMDS_family, 
                       aes(x = NMDS1, y = NMDS2, color = Substrate_Characterization)) +
            geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, 
                                           fill = Substrate_Characterization, 
                                           color = Substrate_Characterization),
                         alpha = 0.3) +
            # geom_segment(data = significant_fish_species_scores_family,
            #              aes(x = 0, xend=NMDS1, y=0, yend=NMDS2),
            #              arrow = arrow(length = unit(0.25, "cm")),
            #              colour = "grey10", 
            #              lwd = 0.3) +                                               # add vector arrows of significant env variables
            # ggrepel::geom_text_repel(data = significant_fish_species_scores_family, 
            #                          aes(x=NMDS1, y=NMDS2, 
            #                              label = abrev),
            #                          cex = 3, 
            #                          direction = "both", 
            #                          segment.size = 0.25) +                          # add labels for species
            theme_light()
        
        
        # NMDS plot of coral communities for each benthic habitat type
        plotting_fishNMDS_family %>%
            ggplot(aes(x = NMDS1, y = NMDS2, color = Dominant_Benthic_Habitat_Type)) +
            geom_point(size = 3, alpha = 0.4) +
            # geom_text(label = plotting_fishNMDS$Transect) +
            stat_ellipse(linetype = 2, size = 1) +
            theme_light()
        
        find_hull = function(plotting_fishNMDS_family) plotting_fishNMDS_family[chull(plotting_fishNMDS_family$NMDS1, plotting_fishNMDS_family$NMDS2), ]
        hulls = ddply(plotting_fishNMDS_family, "Dominant_Benthic_Habitat_Type", find_hull)
        
        ggplot() +
            geom_point(data = plotting_fishNMDS_family, 
                       aes(x = NMDS1, y = NMDS2, color = Dominant_Benthic_Habitat_Type)) +
            geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, 
                                           fill = Dominant_Benthic_Habitat_Type, 
                                           color = Dominant_Benthic_Habitat_Type),
                         alpha = 0.3) +
            # geom_segment(data = significant_fish_species_scores_family,
            #              aes(x = 0, xend=NMDS1, y=0, yend=NMDS2),
            #              arrow = arrow(length = unit(0.25, "cm")),
            #              colour = "grey10", 
            #              lwd = 0.3) +                                               # add vector arrows of significant env variables
            # ggrepel::geom_text_repel(data = significant_fish_species_scores_family, 
            #                          aes(x=NMDS1, y=NMDS2, 
            #                              label = abrev),
            #                          cex = 3, 
            #                          direction = "both", 
            #                          segment.size = 0.25) +                          # add labels for species
            theme_light()       
        
        
## 7. Trophic positions ----
        
    # by site
    fishdiets %>%
        group_by(Unit, Transect, Functional_Trophic_Group) %>%
        summarise(n_fish = n()) %>%
        ungroup() %>%
        complete(nesting(Unit, Transect),
                     nesting(Functional_Trophic_Group),
                     fill = list(n_fish = 0)) %>%
            mutate(Functional_Trophic_Group = recode(Functional_Trophic_Group, 
                                                     piscivore = "Piscivore",
                                                     planktivore = "Planktivore",
                                                     primary_consumer = "Primary Consumer",
                                                     secondary_consumer = "Secondary Consumer"),
                   Functional_Trophic_Group = reorder(Functional_Trophic_Group, n_fish)) %>%
            ggplot(aes(x = Unit, y = n_fish, fill = Functional_Trophic_Group)) +
                geom_col(position = "fill") +
                scale_fill_viridis_d() +
                labs(x="WAPA Management Unit", y="Proportion of Total Fish Observed", fill = "Functional Trophic Group") +
                theme_pubr(legend = "right")    
        
    fishdiets %>%
        group_by(Unit, Transect, Functional_Trophic_Group) %>%
        summarise(n_fish = n()) %>%
        ungroup() %>%
        complete(nesting(Unit, Transect),
                 nesting(Functional_Trophic_Group),
                 fill = list(n_fish = 0)) %>%
            group_by(Unit, Functional_Trophic_Group) %>%
            summarize(mean_fish = mean(n_fish), 
                      se_fish = std.error(n_fish)) %>%
            ungroup() %>%
            mutate(Functional_Trophic_Group = recode(Functional_Trophic_Group, 
                                                       piscivore = "Piscivore",
                                                       planktivore = "Planktivore",
                                                       primary_consumer = "Primary Consumer",
                                                       secondary_consumer = "Secondary Consumer"),
                   Functional_Trophic_Group = reorder(Functional_Trophic_Group, mean_fish)) %>%
                ggplot(aes(x = Unit, y = mean_fish, fill = Functional_Trophic_Group)) + 
                    geom_col(position = "dodge") +
                    geom_errorbar(aes(ymin = mean_fish-se_fish, 
                                      ymax = mean_fish+se_fish, width = 0.5), 
                                  position = position_dodge(0.9)) +
                    geom_signif(annotation="***",      # Agat primary and piscivores
                                y_position=50, xmin=0.65, xmax=1.35, tip_length = 0.02) +
                    geom_signif(annotation="***",      # Agat secondary and piscivores
                                y_position=62, xmin=0.65, xmax=1.1, tip_length = 0.02) +
                    geom_signif(annotation="*",        # Agat primary and planktivores
                                y_position=56, xmin=0.88, xmax=1.35, tip_length = 0.02) +
                    geom_signif(annotation="***",      # Asan primary and piscivores
                                y_position=68, xmin=1.65, xmax=2.35, tip_length = 0.02) +
                    geom_signif(annotation="***",      # Asan secondary and piscivores
                                y_position=74, xmin=1.65, xmax=2.1, tip_length = 0.02) +
                    geom_signif(annotation="*",        # Asan planktivores and piscivores
                                y_position=81, xmin=1.65, xmax=1.85, tip_length = 0.02) +
                    geom_signif(annotation="**",      # Asan primary and planktivores
                                y_position=87, xmin=1.85, xmax=2.35, tip_length = 0.02) +
                    geom_signif(annotation="*",        # Asan secondary and planktivores
                                y_position=94, xmin=1.85, xmax=2.1, tip_length = 0.02) +
                    scale_fill_viridis_d() +
                    labs(x="WAPA Management Unit", y="Mean Number of Fish", fill = "Functional Trophic Group") +
                    theme_pubr(legend = "right")
         
        
    # by substrate 
    fishdiets %>%
        group_by(Functional_Trophic_Group, Substrate_Characterization) %>%
        summarise(n_group_fish = n()) %>%
        ungroup() %>%
        complete(nesting(Substrate_Characterization),
                 nesting(Functional_Trophic_Group),
                 fill = list(n_group_fish = 0)) %>%
        group_by(Substrate_Characterization) %>%
        mutate(total_fish = sum(n_group_fish),
               n_nongroup_fish = total_fish - n_group_fish,
               prop_group_fish = n_group_fish / total_fish) %>%
        ungroup() %>%
        mutate(Functional_Trophic_Group = recode(Functional_Trophic_Group, 
                                                 piscivore = "Piscivore",
                                                 planktivore = "Planktivore",
                                                 primary_consumer = "Primary Consumer",
                                                 secondary_consumer = "Secondary Consumer"),
               Functional_Trophic_Group = reorder(Functional_Trophic_Group, n_group_fish)) %>%
        mutate(Substrate_Characterization = recode(Substrate_Characterization, 
                                                   AggregatePatchReef = "Aggregate Patch \n Reef",
                                                   AggregateReef = "Aggregate Reef",
                                                   ReefRubble = "Reef Rubble",
                                                   RockBoulder = "Rock Boulder",
                                                   SandScatteredCoral = "Sand Scattered \n Coral",
                                                   SandScatteredRock = "Sand Scattered \n Rock" )) %>%
        ggplot(aes(x = reorder(Substrate_Characterization, n_group_fish), y = n_group_fish, fill = Functional_Trophic_Group)) +
            geom_col(position = "fill") +
            scale_fill_viridis_d() +
            geom_label(data = . %>% filter(n_group_fish > 0), 
                      aes(label = paste0("n = ", n_group_fish)),
                      position=position_fill(vjust=0.5), 
                      color = "black", show.legend = F) +
            labs(x="Substrate Characterization", y="Proportion of Fish Observed", fill = "Functional Trophic Group") +
            theme_pubr(legend = "right")
        
     # by benthic habitat type
     fishdiets %>%
        group_by(Functional_Trophic_Group, Dominant_Benthic_Habitat_Type) %>%
        summarise(n_group_fish = n()) %>%
        ungroup() %>%
        complete(nesting(Dominant_Benthic_Habitat_Type),
                 nesting(Functional_Trophic_Group),
                 fill = list(n_group_fish = 0)) %>%
        group_by(Dominant_Benthic_Habitat_Type) %>%
        mutate(total_fish = sum(n_group_fish),
               n_nongroup_fish = total_fish - n_group_fish,
               prop_group_fish = n_group_fish / total_fish) %>%
        ungroup() %>%
        mutate(Functional_Trophic_Group = recode(Functional_Trophic_Group, 
                                                 piscivore = "Piscivore",
                                                 planktivore = "Planktivore",
                                                 primary_consumer = "Primary Consumer",
                                                 secondary_consumer = "Secondary Consumer"),
               Functional_Trophic_Group = reorder(Functional_Trophic_Group, n_group_fish)) %>%
        ggplot(aes(x = reorder(Dominant_Benthic_Habitat_Type, n_group_fish), y = n_group_fish, fill = Functional_Trophic_Group)) +
        geom_col(position = "fill") +
        scale_fill_viridis_d() +
        geom_label(data = . %>% filter(n_group_fish > 0), 
                   aes(label = paste0("n = ", n_group_fish)),
                   position=position_fill(vjust=0.5), 
                   color = "black", show.legend = F) +
        labs(x="Dominant Benthic Habitat Type", y="Proportion of Fish Observed", fill = "Functional Trophic Group") +
        theme_pubr(legend = "right")
   
      
    # by distance from reef crest/shore/freshwater
        
        # by proportion of community per transect
        fish_props %>%
            pivot_longer(cols = c(Shore_Dist, Crest_Dist, Fresh_Dist), 
                         names_to = "Type", 
                         values_to = "Distance") %>%
            mutate(Type = recode(Type, 
                                 Fresh_Dist = "Freshwater Source", 
                                 Shore_Dist = "Shore", 
                                 Crest_Dist = "Reef Crest"),
                   Functional_Trophic_Group = recode(Functional_Trophic_Group, 
                                                     piscivore = "Piscivore",
                                                     planktivore = "Planktivore",
                                                     primary_consumer = "Primary Consumer",
                                                     secondary_consumer = "Secondary Consumer")) %>%
            ggplot(aes(x = Distance, y = prop_group_fish, color = Type, fill = Type)) +
                geom_point(alpha = 0.4) +
                geom_smooth(data = . %>% filter(!Type %in% c("Reef Crest", "Shore", "Freshwater Source")),
                            method = "lm") +
                scale_color_manual(values = c("pink",  "orange", "blue")) +
                scale_fill_manual(values = c("pink", "orange", "blue")) +
                facet_wrap(~Functional_Trophic_Group, scales = "free") +
                labs(x = "Distance (m)", y = "Proportion of Fish Community per Transect", 
                     color = "Distance from:", fill = "Distance from:") +
                theme_pubr()
            
        # by number of fish found per transect
        fish_props %>%
            pivot_longer(cols = c(Shore_Dist, Crest_Dist, Fresh_Dist), 
                         names_to = "Type", 
                         values_to = "Distance") %>%
            mutate(Type = recode(Type, 
                                 Fresh_Dist = "Freshwater Source", 
                                 Shore_Dist = "Shore", 
                                 Crest_Dist = "Reef Crest"),
                   Functional_Trophic_Group = recode(Functional_Trophic_Group, 
                                                     piscivore = "Piscivore",
                                                     planktivore = "Planktivore",
                                                     primary_consumer = "Primary Consumer",
                                                     secondary_consumer = "Secondary Consumer")) %>%
            ggplot(aes(x = Distance, y = n_group_fish, color = Type, fill = Type)) +
                geom_point(alpha = 0.6) +
                geom_smooth(data = . %>% filter(!Type %in% c("Reef Crest", "Shore", "Freshwater Source")),
                            method = "lm") +
                scale_color_manual(values = c("pink",  "orange", "blue")) +
                scale_fill_manual(values = c("pink", "orange", "blue")) +
                facet_wrap(~Functional_Trophic_Group, scales = "free") +
                labs(x = "Distance (m)", y = "Number of Fish per Transect", 
                     color = "Distance from:", fill = "Distance from:") +
                theme_pubr(legend = "right") 
                
        
        
     