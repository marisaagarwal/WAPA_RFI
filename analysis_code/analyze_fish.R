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
            summarise(sp_richness = length(unique(Species))) %>%
            ungroup() %>%
            add_row(Unit = "Asan", Transect = 19, sp_richness = 0) %>%
            mutate(unit_transect = paste(Unit, "_", Transect))
            

    #combining metadata and diversity
    fishsummary = merge(metadata, fish_diversity)
    
    #ANALYSIS
    
        # summary stats
        fishsummary %>%
            group_by(Unit) %>%
            summarise(mean_richness = mean(sp_richness),
                             se_richness = std.error(sp_richness))
        

        # difference in richness by unit? --> no
        fishsummary %>%
            t_test(sp_richness ~ Unit)
        
                # check assumptions
                    # extreme outliers? no. 
                    fishsummary %>%
                        group_by(Unit) %>%
                        identify_outliers(sp_richness)
                    # normal? yes.
                    fishsummary %>%
                        group_by(Unit) %>%
                        shapiro_test(sp_richness)
                    ggqqplot(fishsummary, x = "sp_richness", facet.by = "Unit")
                    # equal variances? yes.
                    fishsummary %>% 
                        levene_test(sp_richness ~ Unit)
                    
                # effect size
                effectsize(t.test(sp_richness ~ Unit, data = fishsummary))
                
                
        # difference in richness by distance from shore/crest/freshwater? 
                # to shore --> yes
                summary(lm(sp_richness ~ Shore_Dist, data = fishsummary))
                # to crest --> yes
                summary(lm(sp_richness ~ Crest_Dist, data = fishsummary))
                # to freshwater output --> yes
                summary(lm(sp_richness ~ Fresh_Dist, data = fishsummary))
            # multiple linear regression
            summary(lm(sp_richness ~ Shore_Dist + Fresh_Dist + Crest_Dist, data = fishsummary))
        
            
        # difference in richness by substrate characterization? -> yes
        fishsummary %>%
            anova_test(sp_richness ~ Substrate_Characterization)
        
                # check assumptions
                    # extreme outliers? There are 2. 
                    fishsummary %>% 
                        group_by(Substrate_Characterization) %>% 
                        identify_outliers(sp_richness) 
                    # homogeneity of variance --> p>0.05 is good
                    fishsummary %>%
                        levene_test(sp_richness ~ Substrate_Characterization)
                    # normality --> p>0.05 is good
                    fishsummary %>%
                        group_by(Substrate_Characterization) %>%
                        filter(!Substrate_Characterization %in% c("RockBoulder", "AggregateReef", "SandScatteredRock")) %>%
                        shapiro_test(sp_richness)
                    ggqqplot(fishsummary, x = "sp_richness", facet.by = "Substrate_Characterization")
                    
            # post-hoc testing
            fishsummary %>%
                tukey_hsd(sp_richness ~ Substrate_Characterization) %>%
                filter(!p.adj > 0.05)
        
                #     # Asan only? --> yes
                #     fishsummary %>%
                #         filter(Unit == "Asan") %>%
                #         anova_test(sp_richness ~ Substrate_Characterization)
                #     
                #     fishsummary %>%
                #         filter(Unit == "Asan") %>%
                #         tukey_hsd(sp_richness ~ Substrate_Characterization) %>%
                #         filter(!p.adj > 0.05)
                #     
                #     # Agat only? --> no
                #     fishsummary %>%
                #         filter(Unit == "Agat") %>%
                #         anova_test(sp_richness ~ Substrate_Characterization)
        
        # difference in richness by benthic habitat type (overall)? -> yes
        fishsummary %>%
            anova_test(sp_richness ~ Dominant_Benthic_Habitat_Type)

                # check assumptions
                    # extreme outliers? no. 
                    fishsummary %>% 
                        group_by(Dominant_Benthic_Habitat_Type) %>% 
                        identify_outliers(sp_richness) 
                    # homogeneity of variance --> p>0.05 is good
                    fishsummary %>%
                        levene_test(sp_richness ~ Dominant_Benthic_Habitat_Type)
                    # normality --> p>0.05 is good
                    fishsummary %>%
                        group_by(Dominant_Benthic_Habitat_Type) %>%
                        # filter(!Substrate_Characterization %in% c("RockBoulder", "AggregateReef", "SandScatteredRock")) %>%
                        shapiro_test(sp_richness)
                    ggqqplot(fishsummary, x = "sp_richness", facet.by = "Dominant_Benthic_Habitat_Type")
        
        # post-hoc testing
        fishsummary %>%
            tukey_hsd(sp_richness ~ Dominant_Benthic_Habitat_Type) %>%
            filter(!p.adj > 0.05)

                # # Asan only? --> no
                # fishsummary %>%
                #     filter(Unit == "Asan") %>%
                #     anova_test(sp_richness ~ Dominant_Benthic_Habitat_Type)
                # 
                # # Agat only? --> yes
                # fishsummary %>%
                #     filter(Unit == "Agat") %>%
                #     anova_test(sp_richness ~ Dominant_Benthic_Habitat_Type)
                # 
                # fishsummary %>%
                #     filter(Unit == "Agat") %>%
                #     tukey_hsd(sp_richness ~ Dominant_Benthic_Habitat_Type) %>%
                #     filter(!p.adj > 0.05)
        
        
## 3. Fish Density ----
        
    # calculations
    fish_density = 
        fishdata %>%
            group_by(Unit, Transect) %>%
            summarise(total_fish = length(Transect)) %>%
            ungroup() %>%
            add_row(Unit = "Asan", Transect = 19, total_fish = 0) %>%
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
            summarise(mean_density = mean(fish_density),
                             se_density = std.error(fish_density))
        
        # difference in density by site? -> yes
        fishsummary %>%
            mutate(transform_fish_density = fish_density^(1/3)) %>%
            t_test(transform_fish_density ~ Unit, var.equal = F)
        
                # check assumptions
                    # extreme outliers? no. 
                    fishsummary %>%
                        mutate(transform_fish_density = fish_density^(1/3)) %>%
                        group_by(Unit) %>%
                        identify_outliers(fish_density) %>%
                        select(c(Unit, Transect, is.outlier, is.extreme))
                    # normal? almost with cube root transform
                    fishsummary %>%
                        mutate(transform_fish_density = fish_density^(1/3)) %>%
                        group_by(Unit) %>%
                        shapiro_test(transform_fish_density)
                    ggqqplot(fishsummary %>% mutate(transform_fish_density = fish_density^(1/3)), x = "transform_fish_density", facet.by = "Unit")
                    # equal variances? no.
                    fishsummary %>% 
                        mutate(transform_fish_density = fish_density^(1/3)) %>%
                        levene_test(transform_fish_density ~ Unit)
                
                # effect size
                effectsize(t.test(transform_fish_density ~ Unit, data = fishsummary %>% mutate(transform_fish_density = fish_density^(1/3))))
        
    
        # difference in density by substrate characterization (overall)? -> no
        fishsummary %>% anova_test(fish_density ~ Substrate_Characterization)
        
                # check assumptions
                    # extreme outliers? no. 
                    fishsummary %>%
                        group_by(Substrate_Characterization) %>%
                        identify_outliers(fish_density) %>%
                        select(c(Unit, Transect, is.outlier, is.extreme))
                    # normal? basically
                    fishsummary %>%
                        group_by(Substrate_Characterization) %>%
                        filter(!Substrate_Characterization %in% c("AggregateReef", "RockBoulder")) %>%
                        shapiro_test(fish_density)
                    ggqqplot(data = fishsummary %>% filter(!Substrate_Characterization %in% c("AggregateReef", "RockBoulder")), x = "fish_density", facet.by = "Substrate_Characterization")
                    # equal variances? yes.
                    fishsummary %>% 
                        levene_test(fish_density ~ Substrate_Characterization)

        fishsummary %>%
            tukey_hsd(fish_density ~ Substrate_Characterization) %>%
            filter(!p.adj > 0.05)
        
            # # Asan only? --> yes
            # fishsummary %>%
            #     filter(Unit == "Asan") %>%
            #     anova_test(fish_density ~ Substrate_Characterization)
            # 
            # fishsummary %>%
            #     filter(Unit == "Asan") %>%
            #     tukey_hsd(fish_density ~ Substrate_Characterization) %>%
            #     filter(!p.adj > 0.05)
            # 
            # #Agat only? --> no
            # fishsummary %>%
            #     filter(Unit == "Agat") %>%
            #     anova_test(fish_density ~ Substrate_Characterization)
        
        # difference in richness by benthic habitat type (overall)? -> yes
        fishsummary %>% 
            anova_test(fish_density ~ Dominant_Benthic_Habitat_Type)
        
            # check assumptions
                # extreme outliers? some. 
                fishsummary %>%
                    group_by(Dominant_Benthic_Habitat_Type) %>%
                    identify_outliers(fish_density) %>%
                    select(c(Dominant_Benthic_Habitat_Type, Unit, Transect, is.outlier, is.extreme))
                # normal? yes.
                fishsummary %>%
                    group_by(Dominant_Benthic_Habitat_Type) %>%
                    shapiro_test(fish_density)
                ggqqplot(data = fishsummary, x = "fish_density", facet.by = "Dominant_Benthic_Habitat_Type")
                # equal variances? yes.
                fishsummary %>% 
                    levene_test(fish_density ~ Dominant_Benthic_Habitat_Type)
        
        fishsummary %>%
            tukey_hsd(fish_density ~ Dominant_Benthic_Habitat_Type) %>%
            filter(!p.adj > 0.05)
        
            # # Asan only? --> no
            # fishsummary %>%
            #     filter(Unit == "Asan") %>%
            #     anova_test(fish_density ~ Dominant_Benthic_Habitat_Type)
            # 
            # #Agat only? --> yes
            # fishsummary %>%
            #     filter(Unit == "Agat") %>%
            #     anova_test(fish_density ~ Dominant_Benthic_Habitat_Type)
            # 
            # fishsummary %>%
            #     filter(Unit == "Agat") %>%
            #     tukey_hsd(fish_density ~ Dominant_Benthic_Habitat_Type) %>%
            #     filter(!p.adj > 0.05)
            # 
        # difference in density by distance from shore/crest/freshwater? 
                # to shore
                summary(lm(fish_density ~ Shore_Dist, data = fishsummary))
                # to crest
                summary(lm(fish_density ~ Crest_Dist, data = fishsummary))
                # to freshwater output
                summary(lm(fish_density ~ Fresh_Dist, data = fishsummary)) 
            # multiple linear regression
            summary(lm(fish_density ~ Shore_Dist + Fresh_Dist + Crest_Dist, data = fishsummary))
        
    
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
                             rename(Species_Code = Species), 
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
            # write_xlsx(
                fish_biomass %>%
                           group_by(Unit, Family, Taxon_Name, Species_Code) %>%
                           dplyr::summarise(mean_tl = mean(Total_Length),
                                            se_tl = std.error(Total_Length),
                                            mean_weight = mean(weight), 
                                            se_weight = std.error(weight)) %>%
                           dplyr::select(c(Unit, Family, Taxon_Name, Species_Code, mean_weight, se_weight)) %>%
                           pivot_wider(names_from = Unit, values_from = c(mean_weight,se_weight))
                       # path = "average fish weight by species and unit4")
        
            fish_biomass %>%
                group_by(Unit, Species_Code) %>%
                summarise(mean_tl = mean(Total_Length),
                          se_tl = std.error(Total_Length),
                          mean_weight = mean(weight), 
                          se_weight = std.error(weight)) %>%
                dplyr::select(c(Unit, Species_Code, mean_weight, se_weight)) %>%
                pivot_wider(names_from = Unit, values_from = c(mean_weight, se_weight))
                
                
            unpaired_unit_fish_species = c("ABSX", "CAME", "CHEP", "CHMA", "CHTR",
                                      "EPIN", "EPME", "FICO", "FOLO", "GOBI",
                                      "GOVA", "LUFU", "PABA", "PAMU", "PLDI",
                                      "PLLA", "PLLD", "POPA", "PTHE", "SCSC",
                                      "STPI", "ZEFL", "ZEVE", "ACNC", "CHIN",
                                      "CHOR", "CHTL", "GNCA", "LABR", "LEOL",
                                      "MAME", "MEAT", "MOGR", "MUFL", "MYBE",
                                      "MYMU", "NEOP", "PACL", "PTAN", "SADI",
                                      "STFA","SYBI", "VAST", "CHUL", "LEHA")
            
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
            
            # overall between units --> did not satisfy assumptions of parametric tests, so wilcoxon
            fish_biomass %>% wilcox_test(weight ~ Unit)
            fish_biomass %>% wilcox_effsize(weight ~ Unit)
            
                # which species weighed different across units?
                fish_biomass %>%
                    filter(!Species_Code %in% unpaired_unit_fish_species) %>%
                    group_by(Species_Code) %>%
                    nest() %>%
                    mutate(wilcox_model = map(data, ~wilcox.test(weight ~ Unit, 
                                                data = .x))) %>%
                    mutate(tidy_wilcox = map(wilcox_model, tidy)) %>%
                    unnest(tidy_wilcox) %>%
                    dplyr::filter(p.value <= 0.05)
     
            # by substrate characterization
                
                # overall 
                fish_biomass %>% kruskal_test(weight ~ Substrate_Characterization)
                fish_biomass %>% kruskal_effsize(weight ~ Substrate_Characterization)
                
                # post-hoc test
                fish_biomass %>% dunn_test(weight ~ Substrate_Characterization) %>%
                    filter(p.adj <= 0.05)
                
                    # # Agat 
                    # fish_biomass %>%
                    #     filter(Unit == "Agat") %>%
                    #     anova_test(weight ~ Substrate_Characterization)
                    # 
                    # fish_biomass %>%
                    #     filter(Unit == "Agat") %>%
                    #     tukey_hsd(weight ~ Substrate_Characterization)
                    # 
                    # # Asan
                    # fish_biomass %>%
                    #     filter(Unit == "Asan") %>%
                    #     anova_test(weight ~ Substrate_Characterization)
                    # 
                    # fish_biomass %>%
                    #     filter(Unit == "Asan") %>%
                    #     tukey_hsd(weight ~ Substrate_Characterization)
                    
                    
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
           
            # by benthic habitat
                
                    # # overall 
                    # fish_biomass %>%
                    #     anova_test(weight ~ Dominant_Benthic_Habitat_Type)
                    # 
                    # fish_biomass %>%
                    #     tukey_hsd(weight ~ Dominant_Benthic_Habitat_Type)
                
                # overall 
                fish_biomass %>% kruskal_test(weight ~ Dominant_Benthic_Habitat_Type)
                fish_biomass %>% kruskal_effsize(weight ~ Dominant_Benthic_Habitat_Type)
                
                # post-hoc test
                fish_biomass %>% dunn_test(weight ~ Dominant_Benthic_Habitat_Type) %>%
                    filter(p.adj <= 0.05)
                
                
                    # # Agat 
                    # fish_biomass %>%
                    #     filter(Unit == "Agat") %>%
                    #     anova_test(weight ~ Dominant_Benthic_Habitat_Type)
                    # 
                    # fish_biomass %>%
                    #     filter(Unit == "Agat") %>%
                    #     tukey_hsd(weight ~ Dominant_Benthic_Habitat_Type)
                    # 
                    # # Asan
                    # fish_biomass %>%
                    #     filter(Unit == "Asan") %>%
                    #     anova_test(weight ~ Dominant_Benthic_Habitat_Type)
                    # 
                    # fish_biomass %>%
                    #     filter(Unit == "Asan") %>%
                    #     tukey_hsd(weight ~ Dominant_Benthic_Habitat_Type)
                    
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
                
                
        # difference in total lengths
            
            # between units
            fish_biomass %>% wilcox_test(Total_Length ~ Unit)
            fish_biomass %>% wilcox_effsize(Total_Length ~ Unit)
            
            # by species
            fish_biomass %>% anova_test(Total_Length ~ Species_Code)
            
            # by substrate type 
            
                # overall 
                fish_biomass %>% kruskal_test(Total_Length ~ Substrate_Characterization)
                fish_biomass %>% kruskal_effsize(Total_Length ~ Substrate_Characterization)
                
                # post-hoc test
                fish_biomass %>% dunn_test(Total_Length ~ Substrate_Characterization) %>%
                    filter(p.adj <= 0.05)
                
            # by benthic habitat type
                
                # overall 
                fish_biomass %>% kruskal_test(Total_Length ~ Dominant_Benthic_Habitat_Type)
                fish_biomass %>% kruskal_effsize(Total_Length ~ Dominant_Benthic_Habitat_Type)
                
                # post-hoc test
                fish_biomass %>% dunn_test(Total_Length ~ Dominant_Benthic_Habitat_Type) %>%
                    filter(p.adj <= 0.05)
            
    # difference in total biomass per transect by distance from shore/crest/freshwater? 
                # to shore 
                summary(lm(sum_biomass ~ Shore_Dist, 
                           data = fish_biomass %>%
                                       group_by(Unit, Transect, Crest_Dist, Shore_Dist, Fresh_Dist) %>%
                                       summarise(sum_total_length = sum(Total_Length),
                                                 sum_biomass = sum(weight))))
                # to crest
                summary(lm(sum_biomass ~ Crest_Dist, 
                           data = fish_biomass %>%
                                       group_by(Unit, Transect, Crest_Dist, Shore_Dist, Fresh_Dist) %>%
                                       summarise(sum_total_length = sum(Total_Length),
                                                 sum_biomass = sum(weight))))
                # to freshwater output 
                summary(lm(sum_biomass ~ Fresh_Dist, 
                           data = fish_biomass %>%
                                       group_by(Unit, Transect, Crest_Dist, Shore_Dist, Fresh_Dist) %>%
                                       summarise(sum_total_length = sum(Total_Length),
                                                 sum_biomass = sum(weight))))
            # multiple linear regression
            summary(lm(sum_biomass ~ Shore_Dist + Fresh_Dist + Crest_Dist, 
                       data = fish_biomass %>%
                                   group_by(Unit, Transect, Crest_Dist, Shore_Dist, Fresh_Dist) %>%
                                   summarise(sum_total_length = sum(Total_Length),
                                             sum_biomass = sum(weight))))     
            
            
    # difference in total length per transect by distance from shore/crest/freshwater? 
                # to shore 
                summary(lm(sum_total_length ~ Shore_Dist, 
                           data = fish_biomass %>%
                               group_by(Unit, Transect, Crest_Dist, Shore_Dist, Fresh_Dist) %>%
                               summarise(sum_total_length = sum(Total_Length),
                                         sum_biomass = sum(weight))))
                # to crest
                summary(lm(sum_total_length ~ Crest_Dist, 
                           data = fish_biomass %>%
                               group_by(Unit, Transect, Crest_Dist, Shore_Dist, Fresh_Dist) %>%
                               summarise(sum_total_length = sum(Total_Length),
                                         sum_biomass = sum(weight))))
                # to freshwater output 
                summary(lm(sum_total_length ~ Fresh_Dist, 
                           data = fish_biomass %>%
                               group_by(Unit, Transect, Crest_Dist, Shore_Dist, Fresh_Dist) %>%
                               summarise(sum_total_length = sum(Total_Length),
                                         sum_biomass = sum(weight))))
            # multiple linear regression
            summary(lm(sum_total_length ~ Shore_Dist + Fresh_Dist + Crest_Dist, 
                       data = fish_biomass %>%
                           group_by(Unit, Transect, Crest_Dist, Shore_Dist, Fresh_Dist) %>%
                           summarise(sum_total_length = sum(Total_Length),
                                     sum_biomass = sum(weight))))
            
        
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
        mutate(prop = n/3190, 
               percent = prop*100)
    
    # frequency of different species
    output %>%
        group_by(Unit, Family, Taxon_Name) %>%
        tally(sort = T)

    output %>%
        group_by(Family, Taxon_Name, Unit) %>%
        tally(sort = T) %>%
        pivot_wider(names_from = Unit, values_from = n)
    
    
    
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
    
     
## 10. Trophic positions of fish ----
     
    # difference in number of fish per trophic group within a site
    fishdiets %>%
        group_by(Unit, Functional_Trophic_Group) %>%
        summarise(n_fish = n()) %>%
        ungroup() %>%
        complete(nesting(Unit),
                 nesting(Functional_Trophic_Group),
                 fill = list(n_fish = 0))
    
    # difference in number of fish per trophic group per transect within a site
    fishdiets %>%
        group_by(Unit, Transect, Functional_Trophic_Group) %>%
        summarise(n_fish = n()) %>%
        ungroup() %>%
        complete(nesting(Unit, Transect),
                 nesting(Functional_Trophic_Group),
                 fill = list(n_fish = 0))

    # summary stats
    fishdiets %>%
        group_by(Unit, Transect, Functional_Trophic_Group) %>%
        summarise(n_fish = n()) %>%
        ungroup() %>%
        complete(nesting(Unit, Transect),
                 nesting(Functional_Trophic_Group),
                 fill = list(n_fish = 0)) %>%
        group_by(Unit, Functional_Trophic_Group) %>%
        summarize(mean_fish = mean(n_fish), 
                  se_fish = std.error(n_fish))
        
        # # test parametric assumptions --> NOT MET  
        #     # extreme outliers? yes.
        #     fishdiets %>%
        #         group_by(Unit, Transect, Functional_Trophic_Group) %>%
        #         summarise(n_fish = n()) %>%
        #         ungroup() %>%
        #         complete(nesting(Unit, Transect),
        #                  nesting(Functional_Trophic_Group),
        #                  fill = list(n_fish = 0)) %>%
        #         group_by(Unit) %>%
        #         identify_outliers(n_fish) %>%
        #         dplyr::select(c(Unit, Transect, Functional_Trophic_Group, is.outlier, is.extreme)) %>%
        #         filter(is.extreme == T)
        #     # normal? no.
        #     fishdiets %>%
        #         mutate(Unit = as.factor(Unit),
        #                Transect = as.factor(Transect),
        #                Functional_Trophic_Group = as.factor(Functional_Trophic_Group)) %>%
        #         group_by(Unit, Transect, Functional_Trophic_Group, .drop = F) %>%
        #         summarise(n_fish = n()) %>%
        #         ungroup() %>%
        #         group_by(Unit) %>%
        #         shapiro_test(n_fish)
        #     ggqqplot(fishdiets %>%
        #                  mutate(Unit = as.factor(Unit),
        #                         Transect = as.factor(Transect),
        #                         Functional_Trophic_Group = as.factor(Functional_Trophic_Group)) %>%
        #                  group_by(Unit, Transect, Functional_Trophic_Group, .drop = F) %>%
        #                  summarise(n_fish = n()) %>%
        #                  ungroup(),
        #              x = "n_fish", facet.by = "Unit")
        #     # equal variances? no.
        #     fishdiets %>%
        #         mutate(Unit = as.factor(Unit),
        #                Transect = as.factor(Transect),
        #                Functional_Trophic_Group = as.factor(Functional_Trophic_Group)) %>%
        #         group_by(Unit, Transect, Functional_Trophic_Group, .drop = F) %>%
        #         summarise(n_fish = n()) %>%
        #         ungroup() %>%
        #         group_by(Unit) %>%
        #         levene_test(n_fish ~ Functional_Trophic_Group)
            
        # perform non-parametric test within each unit
        fishdiets %>%
            group_by(Unit, Transect, Functional_Trophic_Group) %>%
            summarise(n_fish = n()) %>%
            ungroup() %>%
            complete(nesting(Unit, Transect),
                     nesting(Functional_Trophic_Group),
                     fill = list(n_fish = 0)) %>%
            group_by(Unit) %>%
            nest() %>%
                mutate(kruskal_test = map(data, 
                                          ~kruskal.test(n_fish ~ Functional_Trophic_Group, data = .x)),
                       kruskal_effsize = map(data, 
                                             ~kruskal_effsize(n_fish ~ Functional_Trophic_Group, data = .x))) %>%
                mutate(tidy_kruskal = map(kruskal_test, tidy)) %>%
                # unnest(tidy_kruskal) %>%
                mutate(dunn_test = map(data, 
                                       ~dunn_test(n_fish ~ Functional_Trophic_Group, data = .x)))
        
    # by unit 
        
        # perform non-parametric test for each functional group distributions between units
        fishdiets %>%
            group_by(Unit, Transect, Functional_Trophic_Group) %>%
            summarise(n_fish = n()) %>%
            ungroup() %>%
            complete(nesting(Unit, Transect),
                     nesting(Functional_Trophic_Group),
                     fill = list(n_fish = 0)) %>%
            group_by(Unit, Transect) %>%
            mutate(total_fish = sum(n_fish),
                   prop_fish = n_fish / total_fish) %>%
            ungroup() %>%
            group_by(Functional_Trophic_Group) %>%
            nest() %>%
            mutate(wilcox_test = map(data, 
                                      ~wilcox_test(prop_fish ~ Unit, data = .x)),
                   wilcox_effsize = map(data, 
                                         ~wilcox_effsize(prop_fish ~ Unit, data = .x)))
        
    # by substrate
        
        # piscivores   
        piscivore_substrate = 
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
                filter(Functional_Trophic_Group == "piscivore") %>%
                dplyr::select(c(n_group_fish, n_nongroup_fish))
        
        fisher.test(as.data.table(piscivore_substrate), simulate.p.value = T)
        pairwise_fisher_test(as.data.table(piscivore_substrate), simulate.p.value = T)
    
        # secondary consumers
        secondary_substrate = 
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
                filter(Functional_Trophic_Group == "secondary_consumer") %>%
                dplyr::select(c(n_group_fish, n_nongroup_fish))
        
        fisher.test(as.data.table(secondary_substrate), simulate.p.value = T)
        pairwise_fisher_test(as.data.table(secondary_substrate), simulate.p.value = T)
        
        # planktivores
        planktivore_substrate = 
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
            filter(Functional_Trophic_Group == "planktivore") %>%
            dplyr::select(c(n_group_fish, n_nongroup_fish))
        
        fisher.test(as.data.table(planktivore_substrate), simulate.p.value = T)
        view = pairwise_fisher_test(as.data.table(planktivore_substrate), simulate.p.value = T)
        
        # primary consumers
        primary_substrate = 
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
            filter(Functional_Trophic_Group == "primary_consumer") %>%
            dplyr::select(c(n_group_fish, n_nongroup_fish))
        
        fisher.test(as.data.table(primary_substrate), simulate.p.value = T)
        pairwise_fisher_test(as.data.table(primary_substrate), simulate.p.value = T)
        
        
   # by dominant benthic habitat type
        
        # piscivores   
        piscivore_benthic = 
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
            filter(Functional_Trophic_Group == "piscivore") %>%
            dplyr::select(c(n_group_fish, n_nongroup_fish))
        
        fisher.test(as.data.table(piscivore_benthic), simulate.p.value = T)
        pairwise_fisher_test(as.data.table(piscivore_benthic), simulate.p.value = T)
        
        # secondary consumers
        secondary_benthic = 
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
            filter(Functional_Trophic_Group == "secondary_consumer") %>%
            dplyr::select(c(n_group_fish, n_nongroup_fish))
        
        fisher.test(as.data.table(secondary_benthic), simulate.p.value = T)
        pairwise_fisher_test(as.data.table(secondary_benthic), simulate.p.value = T)
        
        # planktivores
        planktivore_benthic = 
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
            filter(Functional_Trophic_Group == "planktivore") %>%
            dplyr::select(c(n_group_fish, n_nongroup_fish))
        
        fisher.test(as.data.table(planktivore_benthic), simulate.p.value = T)
        pairwise_fisher_test(as.data.table(planktivore_benthic), simulate.p.value = T)
        
        # primary consumers
        primary_benthic = 
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
            filter(Functional_Trophic_Group == "primary_consumer") %>%
            dplyr::select(c(n_group_fish, n_nongroup_fish))
        
        fisher.test(as.data.table(primary_benthic), simulate.p.value = T)
        pairwise_fisher_test(as.data.table(primary_benthic), simulate.p.value = T)
        
    # by distance from crest/shore/freshwater
        
        fish_props = 
            fishdiets %>%
                group_by(Unit, Transect, Functional_Trophic_Group) %>%
                summarise(n_group_fish = n()) %>%
                ungroup() %>%
                complete(nesting(Unit, Transect),
                         nesting(Functional_Trophic_Group),
                         fill = list(n_fish = 0)) %>%
                group_by(Unit, Transect) %>%
                mutate(total_fish = sum(n_group_fish),
                       prop_group_fish = n_group_fish / total_fish) %>%
                ungroup() %>%
                # filter(Functional_Trophic_Group == "piscivore") %>%
                drop_na(prop_group_fish)
        
        fish_props = merge(metadata, fish_props) %>%
                        dplyr::select(c("Unit", "Transect","Functional_Trophic_Group", "Crest_Dist", "Shore_Dist", "Fresh_Dist",
                                        "n_group_fish", "prop_group_fish"))
        
        
        # PISCIVORES difference in number of fish & proportion group per transect
                # to shore 
                summary(lm(n_group_fish ~ Shore_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "piscivore")))
                summary(lm(prop_group_fish ~ Shore_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "piscivore")))
                # to crest
                summary(lm(n_group_fish ~ Crest_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "piscivore")))
                summary(lm(prop_group_fish ~ Crest_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "piscivore")))
                # to freshwater output 
                summary(lm(n_group_fish ~ Fresh_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "piscivore")))
                summary(lm(prop_group_fish ~ Fresh_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "piscivore")))
            # multiple linear regression
            summary(lm(n_group_fish ~ Shore_Dist + Fresh_Dist + Crest_Dist, 
                       data = fish_props %>% filter(Functional_Trophic_Group == "piscivore")))
            summary(lm(prop_group_fish ~ Shore_Dist + Fresh_Dist + Crest_Dist, 
                       data = fish_props %>% filter(Functional_Trophic_Group == "piscivore")))
            
        # PLANKTIVORES difference in number of fish & proportion group per transect
                # to shore 
                summary(lm(n_group_fish ~ Shore_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "planktivore")))
                summary(lm(prop_group_fish ~ Shore_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "planktivore")))
                # to crest
                summary(lm(n_group_fish ~ Crest_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "planktivore")))
                summary(lm(prop_group_fish ~ Crest_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "planktivore")))
                # to freshwater output 
                summary(lm(n_group_fish ~ Fresh_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "planktivore")))
                summary(lm(prop_group_fish ~ Fresh_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "planktivore")))
            # multiple linear regression
            summary(lm(n_group_fish ~ Shore_Dist + Fresh_Dist + Crest_Dist, 
                       data = fish_props %>% filter(Functional_Trophic_Group == "planktivore")))
            summary(lm(prop_group_fish ~ Shore_Dist + Fresh_Dist + Crest_Dist, 
                       data = fish_props %>% filter(Functional_Trophic_Group == "planktivore")))
        
        # SECONDARY CONSUMER difference in number of fish & proportion group per transect
                    # to shore 
                    summary(lm(n_group_fish ~ Shore_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "secondary_consumer")))
                    summary(lm(prop_group_fish ~ Shore_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "secondary_consumer")))
                    # to crest
                    summary(lm(n_group_fish ~ Crest_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "secondary_consumer")))
                    summary(lm(prop_group_fish ~ Crest_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "secondary_consumer")))
                    # to freshwater output 
                    summary(lm(n_group_fish ~ Fresh_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "secondary_consumer")))
                    summary(lm(prop_group_fish ~ Fresh_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "secondary_consumer")))
                # multiple linear regression
                summary(lm(n_group_fish ~ Shore_Dist + Fresh_Dist + Crest_Dist, 
                           data = fish_props %>% filter(Functional_Trophic_Group == "secondary_consumer")))
                summary(lm(prop_group_fish ~ Shore_Dist + Fresh_Dist + Crest_Dist, 
                           data = fish_props %>% filter(Functional_Trophic_Group == "secondary_consumer")))
                
        # PRIMARY CONSUMER difference in number of fish & proportion group per transect
                    # to shore 
                    summary(lm(n_group_fish ~ Shore_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "primary_consumer")))
                    summary(lm(prop_group_fish ~ Shore_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "primary_consumer")))
                    # to crest
                    summary(lm(n_group_fish ~ Crest_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "primary_consumer")))
                    summary(lm(prop_group_fish ~ Crest_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "primary_consumer")))
                    # to freshwater output 
                    summary(lm(n_group_fish ~ Fresh_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "primary_consumer")))
                    summary(lm(prop_group_fish ~ Fresh_Dist, data = fish_props %>% filter(Functional_Trophic_Group == "primary_consumer")))
                # multiple linear regression
                summary(lm(n_group_fish ~ Shore_Dist + Fresh_Dist + Crest_Dist, 
                           data = fish_props %>% filter(Functional_Trophic_Group == "primary_consumer")))
                summary(lm(prop_group_fish ~ Shore_Dist + Fresh_Dist + Crest_Dist, 
                           data = fish_props %>% filter(Functional_Trophic_Group == "primary_consumer")))          
        