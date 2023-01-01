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
        
        
## 3. Fish Density ----
        
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
    
    
    
## 6. Fish Biomass and Length ----
    
    # create object
    fish_biomass = merge(fishdata %>% 
                             dplyr::rename(Species_Code = Species), 
                         fishcodes)
    
    # calculations
    fish_biomass %<>%
        mutate(weight = A_value*(Total_Length^B_value))

    # clean data
    
        # identify outliers
        fish_extremeoutliers = 
            fish_biomass %>%
                identify_outliers(weight) %>%
                filter(is.extreme == T)
        
        fish_extremeoutliers %>%
            group_by(Species_Code) %>%
            dplyr::summarise(count = n())
        
        # remove outliers 
        fish_biomass = 
            merge(fish_biomass, fish_extremeoutliers, all.x = T) %>%
            mutate(is.outlier = replace_na(is.outlier, FALSE),
                   is.extreme = replace_na(is.extreme, FALSE)) %>%
            filter(is.extreme != TRUE)
        
        # beautify
        fish_biomass %<>%
            dplyr::select(-c(Date, Notes, A_value, B_value, is.outlier, is.extreme))
        
        # add in metadata   
        fish_biomass = merge(fish_biomass, metadata)
        
    #  ANALYSIS
        
        # summary stats
            
            # average weight by species
            fish_biomass %>%
                group_by(Unit, Species_Code) %>%
                dplyr::summarise(mean_tl = mean(Total_Length),
                                 se_tl = std.error(Total_Length),
                                 mean_weight = mean(weight), 
                                 se_weight = std.error(weight)) %>%
                dplyr::select(c(Unit, Species_Code, mean_weight, se_weight)) %>%
                pivot_wider(names_from = Unit, values_from = c(mean_weight,se_weight))
            
            unpaired_unit_fish_species = c("ABSX", "CAME", "CHEP", "CHMA", "CHTR", 
                                      "EPIN", "EPME", "FICO", "FOLO", "GOBI", 
                                      "GOVA", "LUFU", "PABA", "PAMU", "PLDI",
                                      "PLLA", "PLLD", "POPA", "PTHE", "SCSC", 
                                      "STPI", "ZEFL", "ZEVE", "ACNC", "CHIN",
                                      "CHOR", "CHTL", "GNCA", "LABR", "LEOL", 
                                      "MAME", "MEAT", "MOGR", "MUFL", "MYBE",
                                      "MYMU", "NEOP", "PACL", "PTAN", "SADI",
                                      "STFA","SYBI", "VAST", "CHUL")
            
            # insufficient_obs_fish_species = c("CHLT", "CHRE", "HEFA", "SIAR")
            
            # average weight by unit
            fish_biomass %>%
                group_by(Unit) %>%
                dplyr::summarise(mean_tl = mean(Total_Length),
                                 se_tl = std.error(Total_Length),
                                 mean_weight = mean(weight), 
                                 se_weight = std.error(weight))
            
            # average weight by substrate characterization
            fish_biomass %>%
                group_by(Substrate_Characterization) %>%
                dplyr::summarise(mean_tl = mean(Total_Length),
                                 se_tl = std.error(Total_Length),
                                 mean_weight = mean(weight), 
                                 se_weight = std.error(weight))
            
        # difference in weight
            
            # between units
            fish_biomass %>%
                t_test(weight ~ Unit)
            
            # between units, species specific (i.e., which species weighed diff across units)
            fish_biomass %>%
                dplyr::filter(!Species_Code %in% unpaired_unit_fish_species) %>%
                group_by(Species_Code) %>%
                pairwise_t_test(weight ~ Unit)
            
            # between substrates, species specific (i.e., which species weighed diff across substrate types)
            fish_biomass_substrate_models = 
                fish_biomass %>%
                    group_by(Species_Code) %>%
                    dplyr::filter(n_distinct(Substrate_Characterization) >= 2) %>%
                    group_by(Species_Code) %>%
                    nest() %>%
                    mutate(aov = map(data, ~aov(weight ~ Substrate_Characterization, 
                                                data = .x)),
                           tukey = map(data, ~TukeyHSD(aov(weight ~ Substrate_Characterization, 
                                                           data = .x))))
    
                fish_biomass_substrate_models %>%
                    mutate(tidy_aov = map(aov, tidy), 
                           glance_aov = map(aov, glance),
                           augment_aov = map(aov, augment)) %>%
                    unnest(tidy_aov) %>%
                    dplyr::filter(p.value <= 0.05)
                
                fish_biomass_substrate_models %>%
                    mutate(coefs = purrr::map(tukey, tidy, conf.int = F)) %>% 
                    unnest(coefs) %>%
                    dplyr::filter(adj.p.value <= 0.05) %>%
                    dplyr::select(c(Species_Code, contrast, adj.p.value))

            # between benthic habitats, species specific (i.e., which species weighed diff across benthic habitat types)
                fish_biomass_benthic_models = 
                    fish_biomass %>%
                    group_by(Species_Code) %>%
                    dplyr::filter(n_distinct(Dominant_Benthic_Habitat_Type) >= 2) %>%
                    group_by(Species_Code) %>%
                    nest() %>%
                    mutate(aov = map(data, ~aov(weight ~ Dominant_Benthic_Habitat_Type, 
                                                data = .x)),
                           tukey = map(data, ~TukeyHSD(aov(weight ~ Dominant_Benthic_Habitat_Type, 
                                                           data = .x))))
               
                fish_biomass_benthic_models %>%
                    mutate(tidy_aov = map(aov, tidy), 
                           glance_aov = map(aov, glance),
                           augment_aov = map(aov, augment)) %>%
                    unnest(tidy_aov) %>%
                    dplyr::filter(p.value <= 0.05)
                
                fish_biomass_benthic_models %>%
                    mutate(coefs = purrr::map(tukey, tidy, conf.int = F)) %>% 
                    unnest(coefs) %>%
                    dplyr::filter(adj.p.value <= 0.05) %>%
                    dplyr::select(c(Species_Code, contrast, adj.p.value))
                
     
            # by substrate characterization
                
                # overall 
                fish_biomass %>%
                    anova_test(weight ~ Substrate_Characterization)
                
                fish_biomass %>%
                    tukey_hsd(weight ~ Substrate_Characterization)
            
                    # Agat 
                    fish_biomass %>%
                        filter(Unit == "Agat") %>%
                        anova_test(weight ~ Substrate_Characterization)
                    
                    fish_biomass %>%
                        filter(Unit == "Agat") %>%
                        tukey_hsd(weight ~ Substrate_Characterization)
                    
                    # Asan
                    fish_biomass %>%
                        filter(Unit == "Asan") %>%
                        anova_test(weight ~ Substrate_Characterization)
                    
                    fish_biomass %>%
                        filter(Unit == "Asan") %>%
                        tukey_hsd(weight ~ Substrate_Characterization)
           
            # by benthic habitat
                
                # overall 
                fish_biomass %>%
                    anova_test(weight ~ Dominant_Benthic_Habitat_Type)
                
                fish_biomass %>%
                    tukey_hsd(weight ~ Dominant_Benthic_Habitat_Type)
                
                # Agat 
                fish_biomass %>%
                    filter(Unit == "Agat") %>%
                    anova_test(weight ~ Dominant_Benthic_Habitat_Type)
                
                fish_biomass %>%
                    filter(Unit == "Agat") %>%
                    tukey_hsd(weight ~ Dominant_Benthic_Habitat_Type)
                
                # Asan
                fish_biomass %>%
                    filter(Unit == "Asan") %>%
                    anova_test(weight ~ Dominant_Benthic_Habitat_Type)
                
                fish_biomass %>%
                    filter(Unit == "Asan") %>%
                    tukey_hsd(weight ~ Dominant_Benthic_Habitat_Type)
                
        # difference in total lengths
            
            # between units
            fish_biomass %>%
                t_test(Total_Length ~ Unit)
            
            # by species
            fish_biomass %>%
                anova_test(Total_Length ~ Species_Code)
            
            
    # difference in biomass by distance from shore/crest/freshwater? 
            # to shore --> yes
            summary(lm(weight ~ Shore_Dist, data = fish_biomass))
            # to crest --> no
            summary(lm(weight ~ Crest_Dist, data = fish_biomass))
            # to freshwater output --> no
            summary(lm(weight ~ Fresh_Dist, data = fish_biomass))
            
    # difference in total length by distance from shore/crest/freshwater? 
            # to shore --> yes
            summary(lm(Total_Length ~ Shore_Dist, data = fish_biomass))
            # to crest --> no
            summary(lm(Total_Length ~ Crest_Dist, data = fish_biomass))
            # to freshwater output --> no
            summary(lm(Total_Length ~ Fresh_Dist, data = fish_biomass))
            
        
## 7. Summary items ----
            
    # number of different species & families
    fishdata %<>%
        dplyr::rename(Species_Code = Species) 
            
    output = merge(fishdata, fishcodes)    
    
    length(unique(output$Species_Code))
    length(unique(output$Family))
    
    # frequency of different families
    output %>%
        group_by(Family) %>%
        tally(sort = T) %>%
        mutate(prop = n/3190)
    
    # frequency of different species
    output %>%
        group_by(Unit, Family, Taxon_Name) %>%
        tally(sort = T)
    
    viewer = 
    output %>%
        filter(Unit == "Agat") %>%
        group_by(Family, Taxon_Name) %>%
        tally(sort = T)
    
    
## 8. NMDS (family level) ----
    
    # set up data       
    fishNMDSdata_family = 
        fishNMDSdata %>%
        pivot_longer(cols = c(3:106), names_to = "Species_Code", values_to = "value")
    
    fishNMDSdata_family = merge(fishNMDSdata_family, fishcodes)
    
    fishNMDSdata_family %<>%
        dplyr::select(c(Unit, Transect, value, Family)) %>%
        pivot_wider(names_from = "Family", values_from = "value", values_fn = "sum", values_fill = 0)
    
    vegan_fishNMDS_data_family = 
        merge(fishsummary %>%
                  dplyr::select(c(Unit, Transect, Substrate_Characterization, Dominant_Benthic_Habitat_Type)),
              fishNMDSdata_family)
    
    
    vegan_fishNMDS_data_family = 
        merge(fishsummary %>%
                  dplyr::select(c(Unit, Transect, Substrate_Characterization, Dominant_Benthic_Habitat_Type)),
              fishNMDSdata_family) 
    
    # conduct NMDS 
    fishNMDS_object_family = metaMDS(vegan_fishNMDS_data_family[,5:ncol(vegan_fishNMDS_data_family)], 
                              k = 2,
                              distance = "bray", 
                              trymax = 100)
    
    # examine stressplot & baseplot
    stressplot(fishNMDS_object_family)
    plot(fishNMDS_object_family)
    
    # create parsed down grouping dataframe and add row_ID column
    reference_fishNMDS_family = 
        vegan_fishNMDS_data_family %>%
        dplyr::select(c(Unit, Transect, Substrate_Characterization, Dominant_Benthic_Habitat_Type)) %>%
        ungroup() %>%
        dplyr::mutate(row_ID = row_number())
    
    # extract data for plotting
    plotting_fishNMDS_family = 
        scores(fishNMDS_object_family, display = "sites") %>% 
        as.data.frame() %>% 
        rownames_to_column("row_ID")
    
    plotting_fishNMDS_family = merge(reference_fishNMDS_family, plotting_fishNMDS_family)
    
    # fit environmental and species vectors
    fishNMDS_envfit_family =
        envfit(fishNMDS_object_family, 
               reference_fishNMDS_family, 
               permutations = 999,
               na.rm = TRUE) # this fits environmental vectors
    
    fishNMDS_speciesfit_family =
        envfit(fishNMDS_object_family, 
               vegan_fishNMDS_data_family[,5:ncol(vegan_fishNMDS_data_family)], 
               permutations = 999,
               na.rm = T) # this fits species vectors      
    
    # which species contribute to differences in NMDS plots?
    fish_species_scores_family =
        as.data.frame(scores(fishNMDS_speciesfit_family,
                             display = "vectors"))                                      #save species intrinsic values into dataframe
    
    fish_species_scores_family = cbind(fish_species_scores_family, 
                                Family = rownames(fish_species_scores_family))        #add species names to dataframe
    
    fish_species_scores_family = cbind(fish_species_scores_family,
                                pval = fishNMDS_speciesfit_family$vectors$pvals)      #add pvalues to dataframe so you can select species which are significant
    
    
    fish_species_scores_family = cbind(fish_species_scores_family,
                                abrev = abbreviate(fish_species_scores_family$Family,
                                                   minlength = 4, 
                                                   method = "both"))                #abbreviate species names
    
    significant_fish_species_scores_family = subset(fish_species_scores_family,
                                             pval <= 0.05)                          #subset data to show species significant at 0.05
    
    # which environmental factors contribute to differences in NMDS plots?
    fish_env_scores_family =
        as.data.frame(scores(fishNMDS_envfit_family,
                             display = "vectors"))                        # save species intrinsic values into dataframe
    
    fish_env_scores_family = cbind(fish_env_scores_family, 
                            Species = rownames(fish_env_scores_family))        # add species names to dataframe
    
    fish_env_scores_family = cbind(fish_env_scores_family,
                            pval = fishNMDS_envfit_family$vectors$pvals)       # add pvalues to dataframe so you can select species which are significant
    
    
    # current_env_scores = cbind(current_env_scores,
    #                                abrev = abbreviate(current_env_scores$Species,
    #                                                   minlength = 4, 
    #                                                   method = "both"))                #abbreviate environmental factor names
    
    significant_fish_env_scores_family = subset(fish_env_scores_family,
                                         pval <= 0.05)     #subset data to show environmental factors significant at 0.05
    
    
## 9. PERMANOVA of NMDS (family level) ----
    
    # difference in fish community based on unit? 
    
    # site assumption: do groups have homogeneous variances? 
    dis = vegdist(vegan_fishNMDS_data_family[,5:ncol(vegan_fishNMDS_data_family)], method="bray")
    mod = betadisper(dis, reference_fishNMDS_family$Unit)
    anova(mod)      # p<0.05, violated ... but proceeding anyways
    plot(mod)
    
    adonis2(vegan_fishNMDS_data_family[,5:ncol(vegan_fishNMDS_data_family)] ~ Unit, 
            data = reference_fishNMDS_family, 
            permutations = 9999,
            method = "bray")                     # difference in community based on UNIT
    
    # difference in fish community based on substrate characterization? 
    
    # site assumption: do groups have homogeneous variances? 
    dis = vegdist(vegan_fishNMDS_data_family[,5:ncol(vegan_fishNMDS_data_family)], method="bray")
    mod = betadisper(dis, reference_fishNMDS_family$Substrate_Characterization)
    anova(mod)      # p<0.05, violated ... but proceeding anyways
    plot(mod)
    
    adonis2(vegan_fishNMDS_data_family[,5:ncol(vegan_fishNMDS_data_family)] ~ Substrate_Characterization, 
            data = reference_fishNMDS_family, 
            permutations = 9999,
            method = "bray")                     # difference in community based on SUBSTRATE CHARACTERIZATION
    
    # difference in coral community based on dominant benthic habitat type? 
    
    # site assumption: do groups have homogeneous variances? 
    dis = vegdist(vegan_fishNMDS_data_family[,5:ncol(vegan_fishNMDS_data_family)], method="bray")
    mod = betadisper(dis, reference_fishNMDS_family$Dominant_Benthic_Habitat_Type)
    anova(mod)      # p<0.05, violated ... but proceeding anyways
    plot(mod)
    
    adonis2(vegan_fishNMDS_data_family[,5:ncol(vegan_fishNMDS_data_family)] ~ Dominant_Benthic_Habitat_Type, 
            data = reference_fishNMDS_family, 
            permutations = 9999,
            method = "bray")                     # difference in community based on BENTHIC HABITAT TYPE
    
     
        
    
        
        