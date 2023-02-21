# 2022-12-06


## 1. Set up ----

    # point to data locale
    data_locale = "analysis_code/"
    
    # load in the data
    source(paste0(data_locale, "analyze_CoralNet.R"))
    

## 2. Percent Cover (all labels) ---- 
    
    # by species
    species_cover %>%
        ggplot(aes(Transect, percent_cover, fill = Species_Code)) +
            geom_bar(position = "fill", stat = "identity") +
            facet_wrap(~Site, ncol = 1) +
            theme_light()
    
    species_cover %>%
        ggplot(aes(Site, percent_cover, fill = Species_Code)) +
        geom_bar(position = "fill", stat = "identity") +
        theme_light()
    
    # by detailed functional group
    detailed_func_cover %>%
        ggplot(aes(Transect, percent_cover, fill = Detailed_Func_Group)) +
        geom_bar(position = "fill", stat = "identity") +
        facet_wrap(~Site, ncol = 1) +
        theme_light()
    
    detailed_func_cover %>%
        ggplot(aes(Site, percent_cover, fill = Detailed_Func_Group)) +
        geom_bar(position = "fill", stat = "identity") +
        theme_light()
    
    # by broad functional group
    broad_func_cover %>%
        ggplot(aes(Transect, percent_cover, fill = Functional_Group)) +
        geom_bar(position = "fill", stat = "identity") +
        facet_wrap(~Site, ncol = 1) +
        theme_light()
    
        # site
        broad_func_cover %>%
            ggplot(aes(Site, percent_cover, fill = Functional_Group)) +
            geom_bar(position = "fill", stat = "identity") +
            scale_fill_flat_d() +
            labs(y = "Proportion of Benthic Cover", fill = "Functional Group") +
            theme_pubr(legend = "right")
        
        # dominant benthic habitat
        common_labels_benthic %>%
            ggplot(aes(Dominant_Benthic_Habitat_Type, percent_annotations, fill = Functional_Group)) +
            geom_bar(position = "fill", stat = "identity") +
            scale_fill_flat_d() +
            labs(y = "Proportion of Benthic Cover", fill = "Functional Group") +
            theme_pubr(legend = "right")
    
    
## 3. SEAGRASS percent cover ----

    # by unit
        algae_seagrass_data %>%
            group_by(Unit, Transect, Functional_Group, n_annotations) %>%
            summarise(cover = sum(count)) %>%
            mutate(percent_cover = (cover / n_annotations) * 100) %>%
            filter(Functional_Group == "Seagrass") %>%
            group_by(Unit) %>%
            summarise(mean_cover = mean(percent_cover), 
                      se_cover = std.error(percent_cover)) %>%
        ggplot(aes(Unit, mean_cover, fill = Unit)) +
            geom_col(alpha = 0.8) +
            scale_fill_viridis_d() +
            geom_errorbar(aes(ymin = mean_cover-se_cover, 
                              ymax = mean_cover+se_cover, width = 0.5)) +
            geom_signif(comparisons=list(c("Asan", "Agat")), annotations="*",
                        y_position = 16.9, tip_length = 0.02, vjust=0.4) +
            labs(x="WAPA Management Unit", y="Mean Seagrass Percent Cover") +
            theme_pubr(legend = "none")
    
    # by substrate
    algae_seagrass_data %>%
        group_by(Unit, Transect, Functional_Group, Substrate_Characterization, n_annotations) %>%
        summarise(cover = sum(count)) %>%
        mutate(percent_cover = (cover / n_annotations) * 100) %>%                    filter(Functional_Group == "Seagrass") %>%
        ungroup() %>% 
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
                              ymax = mean_cover+se_cover, width = 0.5)) +
            labs(x="Substrate Characterization", y="Mean Seagrass Percent Cover") +
            theme_pubr(legend = "none")
        
    # by benthic habitat
    algae_seagrass_data %>%
        group_by(Unit, Transect, Functional_Group, Dominant_Benthic_Habitat_Type, n_annotations) %>%
        summarise(cover = sum(count)) %>%
        mutate(percent_cover = (cover / n_annotations) * 100) %>%
        filter(Functional_Group == "Seagrass") %>%
        ungroup() %>%
        group_by(Dominant_Benthic_Habitat_Type) %>%
        summarise(mean_cover = mean(percent_cover), 
                  se_cover = std.error(percent_cover)) %>%
        ggplot(aes(x = reorder(Dominant_Benthic_Habitat_Type, mean_cover), y = mean_cover, 
                   fill = Dominant_Benthic_Habitat_Type)) +
            geom_col() +
            scale_fill_flat_d() +
            geom_errorbar(aes(ymin = mean_cover-se_cover, 
                              ymax = mean_cover+se_cover, width = 0.5)) +
            labs(x="Dominant Benthic Habitat Type", y="Mean Seagrass Percent Cover") +
            theme_pubr(legend = "none")
    
    
## 4. ALGAE OVERALL percent cover   ----
        
    # by unit
    algae_seagrass_data %>%
        group_by(Unit, Transect, Functional_Group, n_annotations) %>%
        summarise(cover = sum(count)) %>%
        mutate(percent_cover = (cover / n_annotations) * 100) %>%
        filter(Functional_Group == "Algae") %>%
        group_by(Unit) %>%
        summarise(mean_cover = mean(percent_cover), 
                  se_cover = std.error(percent_cover)) %>%
        ggplot(aes(Unit, mean_cover, fill = Unit)) +
            geom_col(alpha = 0.8) +
            scale_fill_viridis_d() +
            geom_errorbar(aes(ymin = mean_cover-se_cover, 
                              ymax = mean_cover+se_cover, width = 0.5)) +
            labs(x="WAPA Management Unit", y="Mean Algae Percent Cover") +
            theme_pubr(legend = "none")
    
    # by substrate
    algae_seagrass_data %>%
        group_by(Unit, Transect, Functional_Group, Substrate_Characterization, n_annotations) %>%
        summarise(cover = sum(count)) %>%
        mutate(percent_cover = (cover / n_annotations) * 100) %>% 
        filter(Functional_Group == "Algae") %>%
        ungroup() %>% 
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
                          ymax = mean_cover+se_cover, width = 0.5)) +
        labs(x="Substrate Characterization", y="Mean Algae Percent Cover") +
        theme_pubr(legend = "none")
    
    # by benthic habitat
    algae_seagrass_data %>%
        group_by(Unit, Transect, Functional_Group, Dominant_Benthic_Habitat_Type, n_annotations) %>%
        summarise(cover = sum(count)) %>%
        mutate(percent_cover = (cover / n_annotations) * 100) %>%
        filter(Functional_Group == "Algae") %>%
        ungroup() %>%
        group_by(Dominant_Benthic_Habitat_Type) %>%
        summarise(mean_cover = mean(percent_cover), 
                  se_cover = std.error(percent_cover)) %>%
        ggplot(aes(x = reorder(Dominant_Benthic_Habitat_Type, mean_cover), y = mean_cover, 
                   fill = Dominant_Benthic_Habitat_Type)) +
            geom_col() +
            scale_fill_flat_d() +
            geom_errorbar(aes(ymin = mean_cover-se_cover, 
                              ymax = mean_cover+se_cover, width = 0.5)) +
            labs(x="Dominant Benthic Habitat Type", y="Mean Algae Percent Cover") +
            theme_pubr(legend = "none")
        

## 4. ALGAE FUNCITONAL GROUPS percent cover   ----
    
    # by unit
    algae_seagrass_data %>%
        group_by(Unit, Transect, Functional_Group, Detailed_Func_Group, n_annotations) %>%
        summarise(cover = sum(count)) %>%
        mutate(percent_cover = (cover / n_annotations) * 100) %>%
        filter(Functional_Group == "Algae") %>%
        group_by(Unit, Detailed_Func_Group) %>%
        summarise(mean_cover = mean(percent_cover), 
                  se_cover = std.error(percent_cover)) %>% 
        mutate(Detailed_Func_Group = reorder(Detailed_Func_Group, mean_cover)) %>%
        ggplot(aes(Unit, mean_cover, fill = Detailed_Func_Group)) +
            geom_col(position = "dodge") +
            scale_fill_manual(values = c("gold", "red3", "darkgreen", "tan4")) +
            geom_errorbar(aes(ymin = mean_cover-se_cover, 
                              ymax = mean_cover+se_cover, 
                              width = 0.5), 
                          position = position_dodge(0.9)) +
            geom_signif(annotation="**",      # Agat brown and green
                        y_position = 14.5, xmin= 1.13, xmax=1.35, tip_length = 0.02) +
            geom_signif(annotation="*",      # Agat brown and red
                        y_position = 13, xmin=0.9, xmax=1.35, tip_length = 0.02) +
            geom_signif(annotation="**",      # Asan brown and green
                        y_position = 16.8, xmin= 2.13, xmax=2.35, tip_length = 0.02) +
            geom_signif(annotation="***",      # Asan brown and red
                        y_position = 15.3, xmin=1.9, xmax=2.35, tip_length = 0.02) +
            labs(x="WAPA Management Unit", y="Mean Algal Percent Cover", fill = "Algal Functional Group") +
            theme_pubr(legend = "right")
    
    algae_seagrass_data %>%
        group_by(Unit, Transect, Functional_Group, Detailed_Func_Group, n_annotations) %>%
        summarise(cover = sum(count)) %>%
        mutate(percent_cover = (cover / n_annotations) * 100) %>%
        filter(Functional_Group == "Algae") %>%
        group_by(Unit, Detailed_Func_Group) %>%
        summarise(mean_cover = mean(percent_cover), 
                  se_cover = std.error(percent_cover)) %>% 
        mutate(Detailed_Func_Group = reorder(Detailed_Func_Group, mean_cover)) %>%
        ggplot(aes(x = Unit, y = mean_cover, fill = Detailed_Func_Group)) +
            geom_col(position = "fill") +
            scale_fill_manual(values = c("gold", "red3", "darkgreen", "tan4")) +
            labs(x="WAPA Management Unit", y="Mean Proportion Cover of Algae", fill = "Algal Functional Group") +
            theme_pubr(legend = "right")
    
    # by substrate 
    algae_seagrass_data %>%
        filter(Functional_Group == "Algae") %>%
        group_by(Substrate_Characterization, Detailed_Func_Group) %>%
        summarise(n_points = sum(count)) %>%
        ungroup() %>%
        group_by(Substrate_Characterization) %>%
        mutate(total_points = sum(n_points),
               prop_cover = n_points / total_points,
               percent_cover = prop_cover * 100) %>% 
        mutate(Detailed_Func_Group = reorder(Detailed_Func_Group, n_points),
               Substrate_Characterization = recode(Substrate_Characterization, 
                                                   AggregatePatchReef = "Aggregate Patch \n Reef",
                                                   AggregateReef = "Aggregate Reef",
                                                   ReefRubble = "Reef Rubble",
                                                   RockBoulder = "Rock Boulder",
                                                   SandScatteredCoral = "Sand Scattered \n Coral",
                                                   SandScatteredRock = "Sand Scattered \n Rock" )) %>%
        ggplot(aes(x = reorder(Substrate_Characterization, n_points), y = n_points, 
                   fill = Detailed_Func_Group)) +
        geom_col(position = "fill") +
        scale_fill_manual(values = c("red3", "darkgreen", "tan4", "gold")) +
        geom_label(data = . %>% filter(percent_cover > 1), 
                   aes(label = paste(round(percent_cover, 2),"%")),
                   position=position_fill(vjust=0.5), 
                   color = "black", show.legend = F) +
        labs(x="Substrate Characterization", y="Proportion of Algae Observed", 
             fill = "Algal Functional Group", label = "Mean Percent Cover") +
        theme_pubr(legend = "right")

    # by benthic habitat type
    algae_seagrass_data %>%
        filter(Functional_Group == "Algae") %>%
        group_by(Dominant_Benthic_Habitat_Type, Detailed_Func_Group) %>%
        summarise(n_points = sum(count)) %>%
        ungroup() %>%
        group_by(Dominant_Benthic_Habitat_Type) %>%
        mutate(total_points = sum(n_points),
               prop_cover = n_points / total_points,
               percent_cover = prop_cover * 100) %>% 
        mutate(Detailed_Func_Group = reorder(Detailed_Func_Group, n_points)) %>%
        ggplot(aes(x = reorder(Dominant_Benthic_Habitat_Type, n_points), y = n_points, 
                   fill = Detailed_Func_Group)) +
            geom_col(position = "fill") +
            scale_fill_manual(values = c("red3", "darkgreen", "tan4", "gold")) +
            geom_label(data = . %>% filter(percent_cover > 0.2), 
                       aes(label = paste(round(percent_cover, 2),"%")),
                       position = position_fill(vjust=0.5), 
                       color = "black", show.legend = F) +
            labs(x="Dominant Benthic Habitat Type", y="Proportion of Algae Observed", 
                 fill = "Algal Functional Group", label = "Mean Percent Cover") +
            theme_pubr(legend = "right")
    
    # by distance from shore / reef crest / freshwater input - overall 
    algae_seagrass_data %>%
        filter(Functional_Group == "Algae") %>%
        group_by(Unit, Transect, Crest_Dist, Shore_Dist, Fresh_Dist, n_annotations) %>%
        summarise(n_points = sum(count)) %>%
        mutate(percent_cover = (n_points / n_annotations) * 100)  %>%
        pivot_longer(cols = c(Shore_Dist, Crest_Dist, Fresh_Dist), 
                     names_to = "Type", 
                     values_to = "Distance") %>%
        mutate(Type = recode(Type, 
                             Fresh_Dist = "Freshwater Source", 
                             Shore_Dist = "Shore", 
                             Crest_Dist = "Reef Crest")) %>%
        ggplot(aes(x = Distance, y = percent_cover, color = Type, fill = Type)) +
            geom_point(alpha = 0.6) +
            geom_smooth(data = . %>% filter(!Type %in% c("Shore")),
                        method = "lm") +
            scale_color_manual(values = c("pink",  "orange", "blue")) +
            scale_fill_manual(values = c("pink", "orange", "blue")) +
            coord_cartesian(ylim = c(0, NA)) + 
            labs(x = "Distance (m)", y = "Percent Algal Cover", 
                 color = "Distance from:", fill = "Distance from:") +
            theme_pubr(legend = "right")
    
    # by distance from shore / reef crest / freshwater input - by functional group 
    algae_seagrass_data %>%
        filter(Functional_Group == "Algae") %>%
        group_by(Unit, Transect, Detailed_Func_Group, Crest_Dist, Shore_Dist, Fresh_Dist, n_annotations) %>%
        summarise(n_points = sum(count)) %>%
        mutate(percent_cover = (n_points / n_annotations) * 100)  %>%
        pivot_longer(cols = c(Shore_Dist, Crest_Dist, Fresh_Dist), 
                     names_to = "Type", 
                     values_to = "Distance") %>%
        mutate(Type = recode(Type, 
                             Fresh_Dist = "Freshwater Source", 
                             Shore_Dist = "Shore", 
                             Crest_Dist = "Reef Crest")) %>%
        filter(!Detailed_Func_Group == "Golden Algae") %>%
        ggplot(aes(x = Distance, y = percent_cover, color = Type, fill = Type)) +
            geom_point(alpha = 0.6) +
            # geom_smooth(data = ~subset( . ,
            #                            Detailed_Func_Group == "Golden Algae" &
            #                            !Type %in% c("Shore", "Freshwater Source")),
            #             method = "lm") +
            geom_smooth(data = ~subset( . , 
                                        Detailed_Func_Group == "Green Algae" & 
                                            !Type %in% c("Freshwater Source")),
                        method = "lm", fullrange = F) +
            geom_smooth(data = ~subset( . , 
                                        Detailed_Func_Group == "Red Algae" & 
                                            !Type %in% c("Shore", "Freshwater Source")),
                        method = "lm") +
            # geom_smooth(data = ~subset( . , 
            #                             Detailed_Func_Group == "Brown Algae" & 
            #                                 !Type %in% c("Shore", "Freshwater Source", "Reef Crest")),
            #             method = "lm") +
            scale_color_manual(values = c("pink",  "orange", "blue")) +
            scale_fill_manual(values = c("pink", "orange", "blue")) +
            facet_wrap(~Detailed_Func_Group, scales = "free") +
            coord_cartesian(ylim = c(0, NA)) + 
            labs(x = "Distance (m)", y = "Percent Algal Cover", 
                 color = "Distance from:", fill = "Distance from:") +
            theme_pubr(legend = "right")
    