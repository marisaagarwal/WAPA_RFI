# 2023-02-14 <3

## 1. Set up ----

    # point to data locale
    data_locale = "analysis_code/"
    
    # load in the data
    source(paste0(data_locale, "analyze_inverts.R"))

    
## 2. Species Richness ----
    
    # sea cucumbers
    invert_data %>%
        filter(Group == "Sea Cucumbers") %>%
        group_by(Unit, Transect, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
        summarise(sp_richness = length(unique(Species))) %>%
        ungroup() %>%
        group_by(Unit) %>%
        summarise(mean_richness = mean(sp_richness),
                  se_richness = std.error(sp_richness)) %>%
        ggplot(aes(x = Unit, y = mean_richness, fill = Unit)) +
            geom_col(alpha = 0.8) +
            scale_fill_viridis_d() +
            geom_errorbar(aes(ymin = mean_richness-se_richness, 
                              ymax = mean_richness+se_richness, width = 0.5)) +
            geom_signif(comparisons=list(c("Asan", "Agat")), annotations="*",
                        y_position = 2.6, tip_length = 0.02, vjust=0.4) +
            labs(x="WAPA Management Unit", y="Mean Holothurian Richness") +
            theme_pubr(legend = "none")
    
    # sea urchins --> not applicable bc only four species so not very informative
    
    # sea stars --> not applicable bc only two species so not very informative
    

## 3. Density ----
    
    # sea cucumbers 
    invert_data %>%
        filter(Group == "Sea Cucumbers") %>%
        group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
        summarise(cucumber_density = n() / 50) %>%
        ungroup() %>%
        group_by(Unit) %>%
        summarise(mean_density = mean(cucumber_density),
                  se_density = std.error(cucumber_density)) %>%
        ggplot(aes(x = Unit, y = mean_density, fill = Unit)) +
            geom_col(alpha = 0.8) +
            scale_fill_viridis_d() +
            geom_errorbar(aes(ymin = mean_density-se_density, 
                              ymax = mean_density+se_density, width = 0.5)) +
            geom_signif(comparisons=list(c("Asan", "Agat")), annotations="*",
                        y_position = 0.55, tip_length = 0.02, vjust=0.4) +
            labs(x="WAPA Management Unit", y= "Mean Holothurian Density") +
            theme_pubr(legend = "none")
    
    invert_data %>%
        filter(Group == "Sea Cucumbers") %>%
        group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type, Shore_Dist, Crest_Dist, Fresh_Dist) %>%
        summarise(cucumber_density = n() / 50) %>%
        ungroup() %>%
        pivot_longer(cols = c(Shore_Dist, Crest_Dist, Fresh_Dist), 
                     names_to = "Type", 
                     values_to = "Distance") %>%
        dplyr::mutate(Type = recode(Type, 
                                    Fresh_Dist = "Freshwater Source", 
                                    Shore_Dist = "Shore", 
                                    Crest_Dist = "Reef Crest")) %>%
        ggplot(aes(x = Distance, y = cucumber_density, color = Type, fill = Type)) +
            geom_point(alpha = 0.4) +
            geom_smooth(data = . %>% filter(!Type %in% c("Freshwater Source", "Shore")),
                        method = "lm") +
            scale_color_manual(values = c("pink",  "orange", "blue")) +
            scale_fill_manual(values = c("pink", "orange", "blue")) +
            labs(x = "Distance (m)", y = "Holothurian Density", 
                 color = "Distance from:", fill = "Distance from:") +
            scale_x_continuous(limits = c(0, 1250)) +
            theme_pubr()
    
    
    # sea urchins
    invert_data %>%
        filter(Group == "Sea Urchins") %>%
        group_by(Unit, Transect, Group, Substrate_Characterization, Dominant_Benthic_Habitat_Type) %>%
        summarise(urchin_density = n() / 50) %>%
        ungroup() %>%
        group_by(Substrate_Characterization) %>%
        summarise(mean_density = mean(urchin_density), 
                  std_error_density = std.error(urchin_density))
    

## 4. Size ----
    
    ## sea cucumbers
    invert_data %>%
        filter(Group == "Sea Cucumbers") %>%
        ggplot(aes(x = Size, fill = Species)) +
            geom_histogram(bins = 30, alpha = 0.5) +
            scale_fill_viridis_d() +
            geom_vline(data = invert_data %>%
                           filter(Group == "Sea Cucumbers") %>%
                           group_by(Species) %>%
                           summarise(mean_length = mean(Size), 
                                     se_length = std.error(Size)),  
                       aes(xintercept = mean_length, color = Species), 
                       size = 1) +
            geom_label(data = invert_data %>%
                           filter(Group == "Sea Cucumbers") %>%
                           group_by(Species) %>%
                           summarise(mean_length = mean(Size),
                                     se_length = std.error(Size)),
                       aes(x = mean_length, y = Inf ,
                           label = round(mean_length, 2)),
                       color = "white", 
                       vjust = 2.75) +
            scale_color_viridis_d() +
            facet_wrap(~Species, scales = "free_y") +
            labs(x="Holothurian Length (cm)", y="Count") +
            theme_pubr(legend = "none")
    
    invert_data %>%
        filter(Group == "Sea Cucumbers") %>%
        group_by(Unit) %>%
        summarise(mean_length = mean(Size),
                  se_length = std.error(Size)) %>%
        ggplot(aes(x = Unit, y = mean_length, fill = Unit)) +
            geom_col(alpha = 0.8) +
            scale_fill_viridis_d() +
            geom_errorbar(aes(ymin = mean_length-se_length, 
                              ymax = mean_length+se_length, width = 0.5)) +
            geom_signif(comparisons=list(c("Asan", "Agat")), annotations="**",
                         y_position = 19, tip_length = 0.02, vjust=0.4) +
            labs(x="WAPA Management Unit", y="Mean Holothurian Length (cm)") +
            theme_pubr(legend = "none")
    
    invert_data %>%
        filter(Group == "Sea Cucumbers") %>%
        group_by(Substrate_Characterization) %>%
        summarise(mean_length = mean(Size),
                  se_length = std.error(Size)) %>%
        mutate(Substrate_Characterization = recode(Substrate_Characterization, 
                                                   AggregatePatchReef = "Aggregate Patch \n Reef",
                                                   AggregateReef = "Aggregate Reef",
                                                   ReefRubble = "Reef Rubble",
                                                   RockBoulder = "Rock Boulder",
                                                   SandScatteredCoral = "Sand Scattered \n Coral",
                                                   SandScatteredRock = "Sand Scattered \n Rock" )) %>%
        ggplot(aes(x = reorder(Substrate_Characterization, mean_length), y = mean_length, 
                   fill = Substrate_Characterization)) +
            geom_col() +
            scale_fill_flat_d() +
            geom_errorbar(aes(ymin = mean_length-se_length, 
                              ymax = mean_length+se_length, width = 0.5)) +
            geom_signif(comparisons=list(c("Aggregate Patch \n Reef", "Pavement")), annotations="***",
                         y_position = 18, tip_length = 0.02, vjust=0.4) +
            geom_signif(comparisons=list(c("Aggregate Patch \n Reef", "Reef Rubble")), annotations="***",
                        y_position = 22, tip_length = 0.02, vjust=0.4) +
            geom_signif(comparisons=list(c("Aggregate Patch \n Reef", "Rock Boulder")), annotations="**",
                        y_position = 26, tip_length = 0.02, vjust=0.4) +
            geom_signif(comparisons=list(c("Aggregate Patch \n Reef", "Sand Scattered \n Coral")), annotations="***",
                        y_position = 30, tip_length = 0.02, vjust=0.4) +
            geom_signif(comparisons=list(c("Pavement", "Sand Scattered \n Coral")), annotations="**",
                        y_position = 38, tip_length = 0.02, vjust=0.4) +
            geom_signif(comparisons=list(c("Sand", "Sand Scattered \n Coral")), annotations="***",
                        y_position = 34, tip_length = 0.02, vjust=0.4) +
            labs(x="Substrate Characterization", y="Mean Holothurian Length (cm)") +
            theme_pubr(legend = "none")
    
    invert_data %>%
        filter(Group == "Sea Cucumbers") %>%
        group_by(Dominant_Benthic_Habitat_Type) %>%
        summarise(mean_length = mean(Size),
                  se_length = std.error(Size)) %>% 
        ggplot(aes(x = reorder(Dominant_Benthic_Habitat_Type, mean_length), y = mean_length, 
                   fill = Dominant_Benthic_Habitat_Type)) +
            geom_col() +
            scale_fill_flat_d() +
            geom_errorbar(aes(ymin = mean_length-se_length, 
                              ymax = mean_length+se_length, width = 0.5)) +
            geom_signif(comparisons=list(c("Macroalgae", "Uncolonized")), annotations="*",
                         y_position = 21, tip_length = 0.02, vjust=0.4) +
            geom_signif(comparisons=list(c("Macroalgae", "Seagrass")), annotations="*",
                        y_position = 23, tip_length = 0.02, vjust=0.4) +
            geom_signif(comparisons=list(c("Macroalgae", "Turf")), annotations="**",
                        y_position = 29, tip_length = 0.02, vjust=0.4) +
            geom_signif(comparisons=list(c("Macroalgae", "Coral")), annotations="***",
                        y_position = 25, tip_length = 0.02, vjust=0.4) +
            geom_signif(comparisons=list(c("Coral", "Turf")), annotations="***",
                        y_position = 27, tip_length = 0.02, vjust=0.4) +
            labs(x = "Dominant Benthic Habitat Type", y = "Mean Holothurian Length (cm)") +
            theme_pubr(legend = "none")
    
    invert_data %>%
        filter(Group == "Sea Cucumbers") %>%
        pivot_longer(cols = c(Shore_Dist, Crest_Dist, Fresh_Dist), 
                     names_to = "Type", 
                     values_to = "Distance") %>%
        dplyr::mutate(Type = recode(Type, 
                                    Fresh_Dist = "Freshwater Source", 
                                    Shore_Dist = "Shore", 
                                    Crest_Dist = "Reef Crest")) %>%
        ggplot(aes(x = Distance, y = Size, color = Type, fill = Type)) +
            geom_point(alpha = 0.4) +
            geom_smooth(data = . %>% filter(!Type %in% c("Reef Crest", "Shore")),
                        method = "lm") +
            scale_color_manual(values = c("pink",  "orange", "blue")) +
            scale_fill_manual(values = c("pink", "orange", "blue")) +
            labs(x = "Distance (m)", y = "Holothurian Length (cm)", 
                 color = "Distance from:", fill = "Distance from:") +
            scale_x_continuous(limits = c(0, 1250)) +
            theme_pubr()

    ## sea urchins
    invert_data %>%
        filter(Group == "Sea Urchins") %>%
        group_by(Substrate_Characterization) %>%
        summarise(mean_radius = mean(Size), 
                  se_radius = std.error(Size)) %>%
        mutate(Substrate_Characterization = recode(Substrate_Characterization, 
                                                   AggregatePatchReef = "Aggregate Patch \n Reef",
                                                   AggregateReef = "Aggregate Reef",
                                                   ReefRubble = "Reef Rubble",
                                                   RockBoulder = "Rock Boulder",
                                                   SandScatteredCoral = "Sand Scattered \n Coral",
                                                   SandScatteredRock = "Sand Scattered \n Rock" )) %>%
        ggplot(aes(x = reorder(Substrate_Characterization, mean_radius), y = mean_radius, 
                   fill = Substrate_Characterization)) +
            geom_col() +
            scale_fill_flat_d() +
            geom_errorbar(aes(ymin = mean_radius-se_radius, 
                              ymax = mean_radius+se_radius, width = 0.5)) +
            geom_signif(comparisons=list(c("Aggregate Patch \n Reef", "Reef Rubble")), annotations="***",
                        y_position = 8, tip_length = 0.02, vjust=0.4) +
            geom_signif(comparisons=list(c("Sand Scattered \n Coral", "Reef Rubble")), annotations="***",
                        y_position = 9, tip_length = 0.02, vjust=0.4) +
            geom_signif(comparisons=list(c("Sand Scattered \n Rock", "Reef Rubble")), annotations="*",
                        y_position = 11, tip_length = 0.02, vjust=0.4) +
            geom_signif(comparisons=list(c("Pavement", "Reef Rubble")), annotations="*",
                        y_position = 10, tip_length = 0.02, vjust=0.4) +
            labs(x="Substrate Characterization", y="Mean Echinoidean Radial Length (cm)") +
            theme_pubr(legend = "none")
    
    invert_data %>%
        filter(Group == "Sea Urchins") %>%
        pivot_longer(cols = c(Shore_Dist, Crest_Dist, Fresh_Dist), 
                     names_to = "Type", 
                     values_to = "Distance") %>%
        dplyr::mutate(Type = recode(Type, 
                                    Fresh_Dist = "Freshwater Source", 
                                    Shore_Dist = "Shore", 
                                    Crest_Dist = "Reef Crest")) %>%
        ggplot(aes(x = Distance, y = Size, color = Type, fill = Type)) +
        geom_point(alpha = 0.4) +
        geom_smooth(data = . %>% filter(!Type %in% c("Shore")),
                    method = "lm") +
        scale_color_manual(values = c("pink",  "orange", "blue")) +
        scale_fill_manual(values = c("pink", "orange", "blue")) +
        labs(x = "Distance (m)", y = "Echinoidean Radial Length (cm)", 
             color = "Distance from:", fill = "Distance from:") +
        scale_x_continuous(limits = c(0, 1250)) +
        theme_pubr()
    
    
    ## sea stars
    invert_data %>%
        filter(Group == "Sea Stars") %>%
        group_by(Substrate_Characterization) %>%
        summarise(mean_length = mean(Size),
                  se_length = std.error(Size)) %>%
        mutate(Substrate_Characterization = recode(Substrate_Characterization, 
                                                   AggregatePatchReef = "Aggregate Patch \n Reef",
                                                   AggregateReef = "Aggregate Reef",
                                                   ReefRubble = "Reef Rubble",
                                                   RockBoulder = "Rock Boulder",
                                                   SandScatteredCoral = "Sand Scattered \n Coral",
                                                   SandScatteredRock = "Sand Scattered \n Rock" )) %>%
        ggplot(aes(x = reorder(Substrate_Characterization, mean_length), y = mean_length, 
                   fill = Substrate_Characterization)) +
            geom_col() +
            scale_fill_flat_d() +
            geom_errorbar(aes(ymin = mean_length-se_length, 
                              ymax = mean_length+se_length, width = 0.5)) +
            geom_signif(comparisons=list(c("Pavement", "Reef Rubble")), annotations="*",
                        y_position = 11.5, tip_length = 0.02, vjust=0.4) +
            labs(x="Substrate Characterization", y="Mean Asteroidean Radial Length (cm)") +
            theme_pubr(legend = "none")
    
    invert_data %>%
        filter(Group == "Sea Stars") %>%
        pivot_longer(cols = c(Shore_Dist, Crest_Dist, Fresh_Dist), 
                     names_to = "Type", 
                     values_to = "Distance") %>%
        dplyr::mutate(Type = recode(Type, 
                                    Fresh_Dist = "Freshwater Source", 
                                    Shore_Dist = "Shore", 
                                    Crest_Dist = "Reef Crest")) %>%
        ggplot(aes(x = Distance, y = Size, color = Type, fill = Type)) +
            geom_point(alpha = 0.4) +
            geom_smooth(data = . %>% filter(!Type %in% c("Reef Crest", "Shore")),
                        method = "lm") +
            scale_color_manual(values = c("pink",  "orange", "blue")) +
            scale_fill_manual(values = c("pink", "orange", "blue")) +
            labs(x = "Distance (m)", y = "Asteroidean Radial Length (cm)", 
                 color = "Distance from:", fill = "Distance from:") +
            scale_x_continuous(limits = c(0, 1250)) +
            theme_pubr()
    
    
    