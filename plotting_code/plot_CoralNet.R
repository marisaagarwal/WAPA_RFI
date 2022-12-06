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
    
    broad_func_cover %>%
        ggplot(aes(Site, percent_cover, fill = Functional_Group)) +
        geom_bar(position = "fill", stat = "identity") +
        scale_fill_flat_d() +
        labs(y = "Proportion of Benthic Cover", fill = "Functional Group") +
        theme_pubr(legend = "right")
    
    