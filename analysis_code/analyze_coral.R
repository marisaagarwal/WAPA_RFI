# 2022-11-07

## 1. Set up ----

    # point to data locale
    data_locale = "creation_code/"
    
    # load in the data
    source(paste0(data_locale, "create_coral.R"))


## 2. Alpha Diversity ----

    # calculation
    coral_diversity = 
        coralNMDSdata %>%
            pivot_longer(cols = c(3:23), 
                         names_to = "species_code", 
                         values_to = "count") %>%
            filter(!count == 0) %>%
            group_by(Unit, Transect) %>%
            dplyr::summarise(sp_richness = length(unique(species_code))) %>%
            ungroup() %>%
                add_row(Unit = "Asan", Transect = 22, sp_richness = 0) %>%
                add_row(Unit = "Agat", Transect = 2, sp_richness = 0) %>%
                add_row(Unit = "Agat", Transect = 11, sp_richness = 0) %>%
                add_row(Unit = "Agat", Transect = 15, sp_richness = 0) %>%
                mutate(unit_transect = paste(Unit, "_", Transect))
    
    # combining metadata & diversity
    coralsummary = merge(metadata, coral_diversity) 

    # ANALYSIS
    
        # summary stats for unit
        coralsummary %>%
            group_by(Unit) %>%
            dplyr::summarise(mean_richness = mean(sp_richness),
                             se_richness = std.error(sp_richness))
        
        
        # difference in richness by site? -> no
        coralsummary %>%
            t_test(sp_richness ~ Unit, var.equal = F)
                # testing assumptions
                    # extreme outliers? no. 
                    coralsummary %>% group_by(Unit) %>% identify_outliers(sp_richness)
                    # normal? almost. 
                    coralsummary %>% group_by(Unit) %>% shapiro_test(sp_richness)
                    # equal variances? no, specify in test. 
                    coralsummary %>% levene_test(sp_richness ~ Unit)
                    
        coralsummary %>% wilcox_test(sp_richness~Unit)
        coralsummary %>% wilcox_effsize(sp_richness~Unit)
        

        # difference in richness by distance from shore/crest/freshwater? 
                # to shore
                summary(lm(sp_richness ~ Shore_Dist, data = coralsummary))
                # to crest
                summary(lm(sp_richness ~ Crest_Dist, data = coralsummary))
                # to freshwater output
                summary(lm(sp_richness ~ Fresh_Dist, data = coralsummary))
            # multiple linear regression
            summary(lm(sp_richness ~ Shore_Dist + Fresh_Dist + Crest_Dist, data = coralsummary))
            
            
        # difference in richness by substrate characterization (overall)? -> yes
        
            coralsummary %>%
            anova_test(sp_richness ~ Substrate_Characterization)
        
                # testing assumptions
                    # extreme outliers? no. 
                    coralsummary %>% group_by(Substrate_Characterization) %>% identify_outliers(sp_richness)
                    # normal? almost. 
                    coralsummary %>% group_by(Substrate_Characterization) %>% shapiro_test(sp_richness)
                    ggqqplot(coralsummary, x = "coral_density", facet.by = "Substrate_Characterization")
                    # equal variances? no, specify in test. 
                    coralsummary %>% levene_test(sp_richness ~ Substrate_Characterization)
  
            coralsummary %>%
                tukey_hsd(sp_richness ~ Substrate_Characterization) %>%
                filter(!p.adj > 0.05)
            
            coralsummary %>% kruskal_test(sp_richness ~ Substrate_Characterization)
            coralsummary %>% kruskal_effsize(sp_richness ~ Substrate_Characterization)
            coralsummary %>% dunn_test(sp_richness ~ Substrate_Characterization) %>% filter(p.adj<= 0.05)
        
        # difference in richness by substrate characterization (for each unit)? -> yes, only Agat
            # Asan
            coralsummary %>%
                filter(Unit == "Asan") %>%
                anova_test(sp_richness ~ Substrate_Characterization)
            
            #Agat
            coralsummary %>%
                filter(Unit == "Agat") %>%
                anova_test(sp_richness ~ Substrate_Characterization)
            
            coralsummary %>%
                filter(Unit == "Agat") %>%
                tukey_hsd(sp_richness ~ Substrate_Characterization)
    
        # difference in richness by benthic habitat type (overall)? -> yes
            coralsummary %>%
                anova_test(sp_richness ~ Dominant_Benthic_Habitat_Type)
            
            # testing assumptions
                # extreme outliers? no. 
                coralsummary %>% group_by(Dominant_Benthic_Habitat_Type) %>% identify_outliers(sp_richness)
                # normal? no, low sample sizes. 
                coralsummary %>% group_by(Dominant_Benthic_Habitat_Type) %>% shapiro_test(sp_richness)
                ggqqplot(coralsummary, x = "sp_richness", facet.by = "Dominant_Benthic_Habitat_Type")
                # equal variances? no, specify in test. 
                coralsummary %>% levene_test(sp_richness ~ Dominant_Benthic_Habitat_Type)
            
            coralsummary %>%
                tukey_hsd(sp_richness ~ Dominant_Benthic_Habitat_Type)
            
            coralsummary %>% kruskal_test(sp_richness ~ Dominant_Benthic_Habitat_Type)
            coralsummary %>% kruskal_effsize(sp_richness ~ Dominant_Benthic_Habitat_Type)
            coralsummary %>% dunn_test(sp_richness ~ Dominant_Benthic_Habitat_Type) 
            
        # difference in richness by benthic habitat type (for each unit)? -> yes, only Asan
            # Asan
            coralsummary %>%
                filter(Unit == "Asan") %>%
                anova_test(sp_richness ~ Dominant_Benthic_Habitat_Type)
            
            coralsummary %>%
                filter(Unit == "Asan") %>%
                tukey_hsd(sp_richness ~ Dominant_Benthic_Habitat_Type)
            
            #Agat
            coralsummary %>%
                filter(Unit == "Agat") %>%
                anova_test(sp_richness ~ Dominant_Benthic_Habitat_Type)
        
    
## 4. NMDS (species level) ----
        
        # set up data  
            
            # remove transects that don't have any corals at all
            coralNMDSdata = coralNMDSdata[rowSums(coralNMDSdata[,3:ncol(coralNMDSdata)])>0,]
            
            # vegan-ify    
            vegan_coralNMDS_data = 
                merge(coralsummary %>%
                          dplyr::select(c(Unit, Transect, Substrate_Characterization, Dominant_Benthic_Habitat_Type)),
                      coralNMDSdata)
        
        # conduct NMDS 
        coralNMDS_object = metaMDS(vegan_coralNMDS_data[,5:ncol(vegan_coralNMDS_data)], 
                                    k = 2,
                                    distance = "bray", 
                                    trymax = 100)
        
        # examine stressplot & baseplot
        stressplot(coralNMDS_object)
        plot(coralNMDS_object)
        
        # create parsed down grouping dataframe and add row_ID column
        reference_coralNMDS = 
            vegan_coralNMDS_data %>%
            dplyr::select(c(Unit, Transect, Substrate_Characterization, Dominant_Benthic_Habitat_Type)) %>%
            ungroup() %>%
            dplyr::mutate(row_ID = row_number())
        
        # extract data for plotting
        plotting_coralNMDS = 
            scores(coralNMDS_object, display = "sites") %>% 
            as.data.frame() %>% 
            rownames_to_column("row_ID")
        
        plotting_coralNMDS = merge(reference_coralNMDS, plotting_coralNMDS)
        
        # fit environmental and species vectors
        coralNMDS_envfit =
            envfit(coralNMDS_object, 
                   reference_coralNMDS, 
                   permutations = 999,
                   na.rm = TRUE) # this fits environmental vectors
        
        coralNMDS_speciesfit =
            envfit(coralNMDS_object, 
                   vegan_coralNMDS_data[,5:ncol(vegan_coralNMDS_data)], 
                   permutations = 999,
                   na.rm = T) # this fits species vectors      
        
        # which species contribute to differences in NMDS plots?
        coral_species_scores =
            as.data.frame(scores(coralNMDS_speciesfit,
                                 display = "vectors"))                                      #save species intrinsic values into dataframe
        
        coral_species_scores = cbind(coral_species_scores, 
                                     Species = rownames(coral_species_scores))        #add species names to dataframe
        
        coral_species_scores = cbind(coral_species_scores,
                                      pval = coralNMDS_speciesfit$vectors$pvals)      #add pvalues to dataframe so you can select species which are significant
        
        
        coral_species_scores = cbind(coral_species_scores,
                                     abrev = abbreviate(coral_species_scores$Species,
                                                          minlength = 4, 
                                                          method = "both"))                #abbreviate species names
        
        significant_coral_species_scores = subset(coral_species_scores,
                                                  pval <= 0.05)                          #subset data to show species significant at 0.05
        
        # which environmental factors contribute to differences in NMDS plots?
        coral_env_scores =
            as.data.frame(scores(coralNMDS_envfit,
                                 display = "vectors"))                        # save species intrinsic values into dataframe
        
        coral_env_scores = cbind(coral_env_scores, 
                                 Species = rownames(coral_env_scores))        # add species names to dataframe
        
        coral_env_scores = cbind(coral_env_scores,
                                 pval = coralNMDS_envfit$vectors$pvals)       # add pvalues to dataframe so you can select species which are significant
        
        
        # current_env_scores = cbind(current_env_scores,
        #                                abrev = abbreviate(current_env_scores$Species,
        #                                                   minlength = 4, 
        #                                                   method = "both"))                #abbreviate environmental factor names
        
        significant_coral_env_scores = subset(coral_env_scores,
                                                pval <= 0.05)                          #subset data to show environmental factors significant at 0.05
        
        
## 5. perMANOVA of NMDS (species level) ----
        
    # difference in coral community based on: Unit? no. 
    #                                         Substrate characterization? no. 
    #                                         Benthic habitat? yes. 
        
        # assumptions: do groups have homogeneous variances? 
        dis = vegdist(vegan_coralNMDS_data[,5:ncol(vegan_coralNMDS_data)], method="bray")
        mod = betadisper(dis, reference_coralNMDS$Unit)
            anova(mod)      # p>0.05, proceed
            plot(mod)
        mod = betadisper(dis, reference_coralNMDS$Substrate_Characterization)
            anova(mod)      # p>0.05, proceed
            plot(mod)
        mod = betadisper(dis, reference_coralNMDS$Dominant_Benthic_Habitat_Type)
            anova(mod)      # p>0.05, proceed
            plot(mod)
        
        #test
        adonis2(vegan_coralNMDS_data[,5:ncol(vegan_coralNMDS_data)] ~ Unit,
                data = reference_coralNMDS, 
                permutations = 9999,
                method = "bray")
        adonis2(vegan_coralNMDS_data[,5:ncol(vegan_coralNMDS_data)] ~ Substrate_Characterization,
                data = reference_coralNMDS, 
                permutations = 9999,
                method = "bray")
        adonis2(vegan_coralNMDS_data[,5:ncol(vegan_coralNMDS_data)] ~ Dominant_Benthic_Habitat_Type,
                data = reference_coralNMDS, 
                permutations = 9999,
                method = "bray")
        
        
## 6. NMDS (genus level) ----
        
        # set up data
        coralNMDSdata_genus = 
            coralNMDSdata %>%
                pivot_longer(cols = c(3:23), names_to = "species_code", values_to = "value")
        
        coralNMDSdata_genus = merge(coralNMDSdata_genus, coralNMDScodes)
        
        coralNMDSdata_genus %<>%
            dplyr::select(c(Unit, Transect, value, Genus)) %>%
            pivot_wider(names_from = "Genus", values_from = "value", values_fn = "sum")
        
        vegan_coralNMDS_data_genus = 
            merge(coralsummary %>%
                      dplyr::select(c(Unit, Transect, Substrate_Characterization, Dominant_Benthic_Habitat_Type)),
                  coralNMDSdata_genus)
        
        # conduct NMDS 
        coralNMDS_genus_object = metaMDS(vegan_coralNMDS_data_genus[,5:ncol(vegan_coralNMDS_data_genus)], 
                                   k = 2,
                                   distance = "bray", 
                                   trymax = 100)
        
        # examine stressplot & baseplot
        stressplot(coralNMDS_genus_object)
        plot(coralNMDS_genus_object)
        
        # create parsed down grouping dataframe and add row_ID column
        reference_coralNMDS_genus = 
            vegan_coralNMDS_data_genus %>%
            dplyr::select(c(Unit, Transect, Substrate_Characterization, Dominant_Benthic_Habitat_Type)) %>%
            ungroup() %>%
            dplyr::mutate(row_ID = row_number())
        
        # extract data for plotting
        plotting_coralNMDS_genus = 
            scores(coralNMDS_genus_object, display = "sites") %>% 
            as.data.frame() %>% 
            rownames_to_column("row_ID")
        
        plotting_coralNMDS_genus = merge(reference_coralNMDS_genus, plotting_coralNMDS_genus)
        
        # fit environmental and species vectors
        coralNMDS_envfit_genus =
            envfit(coralNMDS_genus_object, 
                   reference_coralNMDS_genus, 
                   permutations = 999,
                   na.rm = TRUE) # this fits environmental vectors
        
        coralNMDS_speciesfit_genus =
            envfit(coralNMDS_genus_object, 
                   vegan_coralNMDS_data_genus[,5:ncol(vegan_coralNMDS_data_genus)], 
                   permutations = 999,
                   na.rm = T) # this fits species vectors      
        
        # which species contribute to differences in NMDS plots?
        coral_species_scores_genus =
            as.data.frame(scores(coralNMDS_speciesfit_genus,
                                 display = "vectors"))                                      #save species intrinsic values into dataframe
        
        coral_species_scores_genus = cbind(coral_species_scores_genus, 
                                     Genus = rownames(coral_species_scores_genus))        #add species names to dataframe
        
        coral_species_scores_genus = cbind(coral_species_scores_genus,
                                     pval = coralNMDS_speciesfit_genus$vectors$pvals)      #add pvalues to dataframe so you can select species which are significant
        
        
        coral_species_scores_genus = cbind(coral_species_scores_genus,
                                     abrev = abbreviate(coral_species_scores_genus$Genus,
                                                        minlength = 4, 
                                                        method = "both"))                #abbreviate species names
        
        significant_coral_species_scores_genus = subset(coral_species_scores_genus,
                                                  pval <= 0.05)                          #subset data to show species significant at 0.05
        
        # which environmental factors contribute to differences in NMDS plots?
        coral_env_scores_genus =
            as.data.frame(scores(coralNMDS_envfit_genus,
                                 display = "vectors"))                        # save species intrinsic values into dataframe
        
        coral_env_scores_genus = cbind(coral_env_scores_genus, 
                                 Species = rownames(coral_env_scores_genus))        # add species names to dataframe
        
        coral_env_scores_genus = cbind(coral_env_scores_genus,
                                 pval = coralNMDS_envfit_genus$vectors$pvals)       # add pvalues to dataframe so you can select species which are significant
        
        
        # current_env_scores = cbind(current_env_scores,
        #                                abrev = abbreviate(current_env_scores$Species,
        #                                                   minlength = 4, 
        #                                                   method = "both"))                #abbreviate environmental factor names
        
        significant_coral_env_scores_genus = subset(coral_env_scores_genus,
                                              pval <= 0.05)                          #subset data to show environmental factors significant at 0.05
        
        
## 7. perMANOVA of NMDS (genus level) ----   
        
    # difference in coral community based on Unit? no. Substrate? yes. Benthic Habitat? yes.  
        
        # site assumption: do groups have homogeneous variances? 
        dis = vegdist(vegan_coralNMDS_data_genus[,5:ncol(vegan_coralNMDS_data_genus)], method="bray")
        mod = betadisper(dis, reference_coralNMDS_genus$Unit)
            anova(mod)      # p>0.05, proceed
            plot(mod)
        mod = betadisper(dis, reference_coralNMDS_genus$Substrate_Characterization)
            anova(mod)      # p>0.05, proceed
            plot(mod)
        mod = betadisper(dis, reference_coralNMDS_genus$Dominant_Benthic_Habitat_Type)
            anova(mod)      # p>0.05, proceed
            plot(mod)
            
        # test 
        adonis2(vegan_coralNMDS_data_genus[,5:ncol(vegan_coralNMDS_data_genus)] ~ Unit, 
                data = reference_coralNMDS_genus, 
                permutations = 9999,
                method = "bray")
        adonis2(vegan_coralNMDS_data_genus[,5:ncol(vegan_coralNMDS_data_genus)] ~ Substrate_Characterization, 
                data = reference_coralNMDS_genus, 
                permutations = 9999,
                method = "bray")
        adonis2(vegan_coralNMDS_data_genus[,5:ncol(vegan_coralNMDS_data_genus)] ~ Dominant_Benthic_Habitat_Type, 
                data = reference_coralNMDS_genus, 
                permutations = 9999,
                method = "bray")
        
        
       
## 8. Coral Density ----
        
        # calculations
        coral_density = 
            coralNMDSdata %>%
            pivot_longer(cols = c(3:23), 
                         names_to = "species_code", 
                         values_to = "count") %>%
            group_by(Unit, Transect) %>%
            dplyr::summarise(total_corals = sum(count)) %>%
            mutate(coral_density = total_corals/10, 
                   unit_transect = paste(Unit, "_", Transect))
        
        # combining metadata & diversity
        coralsummary = merge(coralsummary, coral_density) 
        
        # prune
        coralsummary = 
            coralsummary %>%
            dplyr::select(c(Unit, Transect, unit_transect, Shore_Dist, Crest_Dist, Fresh_Dist,
                            Substrate_Characterization, Dominant_Benthic_Habitat_Type, 
                            sp_richness, coral_density))
        
     # ANALYSIS
        
        # summary stats for unit
        coralsummary %>%
            group_by(Unit) %>%
            dplyr::summarise(mean_density = mean(coral_density),
                             se_density = std.error(coral_density))
        
        # difference in density by site? -> yes
        coralsummary %>%
            t_test(coral_density ~ Unit, var.equal = F)
                # testing assumptions
                    # extreme outliers? no. 
                    coralsummary %>% group_by(Unit) %>% identify_outliers(coral_density)
                    # normal? not really. 
                    coralsummary %>% group_by(Unit) %>% shapiro_test(coral_density)
                    ggqqplot(coralsummary, x = "coral_density", facet.by = "Unit")
                    # equal variances? no, specify in test. 
                    coralsummary %>% levene_test(coral_density ~ Unit)
        
        coralsummary %>% wilcox_test(coral_density~Unit)
        coralsummary %>% wilcox_effsize(coral_density~Unit)
        

        # difference in density by substrate characterization (overall)? -> no
            
            # coralsummary %>% anova_test(coral_density ~ Substrate_Characterization)
        
        coralsummary %>% kruskal_test(coral_density ~ Substrate_Characterization)
        coralsummary %>% kruskal_effsize(coral_density ~ Substrate_Characterization)
        
        
        # difference in density by substrate characterization (for each unit)? -> yes, only Asan
        # Asan
        coralsummary %>%
            filter(Unit == "Asan") %>%
            anova_test(coral_density ~ Substrate_Characterization)
        
        coralsummary %>%
            filter(Unit == "Asan") %>%
            tukey_hsd(coral_density ~ Substrate_Characterization)
        
        #Agat
        coralsummary %>%
            filter(Unit == "Agat") %>%
            anova_test(coral_density ~ Substrate_Characterization)
        
        # difference in density by benthic habitat type (overall)? -> yes
        coralsummary %>%
            anova_test(coral_density ~ Dominant_Benthic_Habitat_Type)
        
        
        coralsummary %>% kruskal_test(coral_density ~ Dominant_Benthic_Habitat_Type)
        coralsummary %>% kruskal_effsize(coral_density ~ Dominant_Benthic_Habitat_Type)
        coralsummary %>% dunn_test(coral_density ~ Dominant_Benthic_Habitat_Type)
        
        
        # difference in density by benthic habitat type (for each unit)? -> yes, only Asan
        # Asan
        coralsummary %>%
            filter(Unit == "Asan") %>%
            anova_test(coral_density ~ Dominant_Benthic_Habitat_Type)
        
        coralsummary %>%
            filter(Unit == "Asan") %>%
            tukey_hsd(coral_density ~ Dominant_Benthic_Habitat_Type)
        
        #Agat
        coralsummary %>%
            filter(Unit == "Agat") %>%
            anova_test(coral_density ~ Dominant_Benthic_Habitat_Type)
        
        
    # difference in density by distance from shore/crest/freshwater? 
            # to shore
            summary(lm(coral_density ~ Shore_Dist, data = coralsummary))
            # to crest
            summary(lm(coral_density ~ Crest_Dist, data = coralsummary))
            # to freshwater output
            summary(lm(coral_density ~ Fresh_Dist, data = coralsummary))  
        # multiple linear regression
        summary(lm(coral_density ~ Shore_Dist + Fresh_Dist + Crest_Dist, data = coralsummary))
        
    
    
    
