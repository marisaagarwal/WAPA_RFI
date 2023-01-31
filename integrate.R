##  Project Name:  2022 Reef Flat Inventory Project at WAPA
##
##  Objective:     Assess ecological metrics of fish, coral, algae, and invertebrate
##                 communities at War in the Pacific National Historic Park in Guam, USA.
##
##  Approach:      1. Obtain data from surveys done along reef flats within WAPA.
##                 2. Analyze data to fulfill research objective with focus on:
##                      - diversity
##                      - community structure & variation (NMDS)
##                      - percent cover
##                      - biomass
##
##  Authors:       Marisa Agarwal
##
##  Start Date:    summer 2022, analysis started: 11/3/2022


##  Notes:         RUN THIS FILE BEFORE EVERY SESSION 



## 1. point to working directory

    setwd("research/WAPA_RFI")


## 2. Set up core functionality

    # clean up
    rm(list=ls())
    
    # call to core packages for data manipulation
    library(plyr)
    library(dplyr)
    library(tidyr)
    library(magrittr)      
    library(purrr)
    library(lubridate)
    library(stringr)
    library(forcats)      
    library(tidyverse)
    library(see)
    library(broom)
    
    # for importing different formats
    library(readr)
    library(readxl)
    library(data.table)
    library(writexl)
    
    # for easier tidy stats 
    library(rstatix)
    library(easystats)
    library(multcomp)
    library(vegan)
    # library(coin)
    
    # call to visualisation & output generation
    library(ggplot2)
    library(GGally)
    library(Cairo)
    library(extrafont)
    library(RColorBrewer)
    library(viridis)
    library(ggdist)
    library(ggsci)
    library(plotrix)
    library(multcompView)
    library(ggpubr)


## 3. Generate data objects
## if things need to be loaded in before every session, include them here
    
    