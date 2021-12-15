library(ggplot2)
library(readxl)
library(tidyverse)
#################################################################################################################
####   Get the data #####
input_data_file <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Murrays_sites.csv"

input_data_file_rain <- "X:/Therese_Jackie/Sandy_soils/Sands weather/met_file2021/GS_rain_deciles_Murray_sites.csv"



primary_murray <- read_csv(input_data_file)




#################################################################################################################
####   Create the descriptors #####


primary_murray$rip_depth <- as.double(primary_murray$rip_depth)
primary_murray <- primary_murray %>% 
  mutate(Rip_depth_jax = rip_depth/10)

#######################################################################################################################

#######################################################################################################################
# step 1 what depth was amendment applied?

primary_murray <- primary_murray %>% 
  mutate(placement_jax = case_when(
    placement  == "surface"  ~           "surface",
    placement  == "incorporated"  ~      "mix",
    placement  == "deep"  ~              "deep",
    TRUE                  ~            placement)
    )


#step 2 make a clm with what ameliorant was added if any
primary_2019_2020_imapct <- primary_2019_2020_imapct %>% 
  mutate(amendment = case_when(
    
    ## cl.with.depth
    organic  == "chicken_compost" | organic  ==  "chicken_manure" &  other_ameliorant ==  "none"    ~        paste0("Cl",".", placement_jax),
    organic  == "chicken_compost" | organic  ==  "chicken_manure" &  other_ameliorant ==  "gypsum"       ~   paste0("Cl.gypsum",".", placement_jax),
    organic  == "chicken_compost" | organic  ==  "chicken_manure" &  other_ameliorant ==  "clay"       ~     paste0("Cl.clay",".", placement_jax),
    
    # This is from Murray
    organic  == "chicken_litter"  &  other_ameliorant ==  "none"    ~        paste0("Cl",".", placement_jax),
    
    ## Lc.with.depth
    organic  == "lucerne"  &  other_ameliorant ==  "none"    ~           paste0("Lc",".", placement_jax),
    organic  == "lucerne"  &  other_ameliorant ==  "gypsum"       ~      paste0("Lc.gypsum",".", placement_jax),
    organic  == "lucerne"  &  other_ameliorant ==  "clay"       ~        paste0("Lc.clay",".", placement_jax),
    
    # This is from Murray
    organic  == "pelleted lucerne"  &  other_ameliorant ==  "none"    ~           paste0("Lc",".", placement_jax),
    
    #no amendment
    organic  == "none"  &  fertiliser == "none"   &    other_ameliorant ==  "none"    ~      "none",
    
    #other amendment
    organic  == "none"  &  fertiliser == "none"   &    other_ameliorant ==  "gypsum"    ~    paste0("gypsum",".", placement_jax),
    #organic  == "none"  &  fertiliser == "none"   &    other_ameliorant ==  "clay"    ~      paste0("clay",".", placement_jax), # this was wrong
    organic  == "none"  &  fertiliser == "none"   &    other_ameliorant ==  "clay"    ~      paste0("clay"),
    
    #fertiliser amendment
    organic  == "none"  &  fertiliser != "none"   &    other_ameliorant ==  "none"    ~      paste0("Fert",".", placement_jax),
    organic  == "none"  &  fertiliser != "none"   &    other_ameliorant ==  "gypsum"    ~    paste0("Fert.gypsum",".", placement_jax),
    organic  == "none"  &  fertiliser != "none"   &    other_ameliorant ==  "clay"    ~      paste0("Fert.clay",".", placement_jax),
    
    
    
    TRUE ~ as.character("check")
    
  ) )



### I am using pre_app_site_data as template