### merge all the data


library(readxl)
library(tidyverse)

#################################################################################################################
####   Get the data site trial data #####

current.folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/step2"




impact_sites_step1_2 <- 
  read_csv("X:/Therese_Jackie/Sandy_soils/Development_database/step2/impact_sites_step1_2_neat.csv", 
           col_types = cols(yield = col_double()))

research_sites_sites_step1_2 <-
  read.csv("X:/Therese_Jackie/Sandy_soils/Development_database/step2/research_sites_sites_step1_2_neat.csv")



sites_merged <- rbind(impact_sites_step1_2, research_sites_sites_step1_2)

  

                         
output_folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/completeDB/"

write.csv(sites_merged, paste0(output_folder,"sites_merged",".csv"))                           



#################################################################################################################
####   Get the data site trial CONTROL data #####

impact_sites_step1_2_control <- 
  read_csv("X:/Therese_Jackie/Sandy_soils/Development_database/step2/impact_sites_step1_2_control.csv", 
           col_types = cols(yield = col_double(),
                            control_yield = col_double()))

research_sites_sites_step1_2_control <-
  read.csv("X:/Therese_Jackie/Sandy_soils/Development_database/step2/research_sites_sites_step1_2_control.csv")

str(impact_sites_step1_2_control)
str(research_sites_sites_step1_2_control$yield)

sites_merged_control <- rbind(
  impact_sites_step1_2_control,
  research_sites_sites_step1_2_control
)



output_folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/completeDB/"

write.csv(sites_merged_control, paste0(output_folder,"sites_control_merged",".csv"))            

                        
#################################################################################################################
####   Get the data site trial data all of the data not just the neat stuff #####

current.folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/step2"




impact_sites_step1_2_all <- 
  read_csv("X:/Therese_Jackie/Sandy_soils/Development_database/step2/impact_sites_step1_2.csv", 
           col_types = cols(
             ID =  col_character(),
             site = col_character(),
             year = col_double(),
             plot = col_character(),
             rep_block = col_character(),
             crop = col_character(),
             rip = col_character(),
             rip_depth = col_character(),
             mix = col_character(),
             organic = col_character(),
             organic_rate = col_character(),
             fertiliser = col_character(),
             fertiliser_rate = col_character(),
             other_ameliorant = col_character(),
             other_rate = col_character(),
             establishment = col_double(),
             yield = col_double(),
             dry_biomass = col_double(),
             drill_depth = col_character(),
             timing = col_character(),
             sowing_strategy = col_character(),
             comments = col_character(),
             site_sub = col_character(),
             Rip_depth_jax = col_character(),
             amendment_organic = col_character(),
             amendment_fert = col_character(),
             amendment_other = col_character(),
             Descriptors = col_character(),
             Latitude = col_double(),
             Longitude = col_double(),
             Repellence = col_character(),
             Acidity = col_character(),
             Physical = col_character(),
             Nutrient = col_character(),
             other = col_character(),
             Amelioration_Date = col_date(format = "%Y-%m-%d"),
             Amelioration_Year = col_double(),
             yr_post_amelioration = col_double(),
             sowing_date = col_date(format = "%Y-%m-%d"),
             harvest_date = col_date(format = "%Y-%m-%d"),
             previous_crop = col_character(),
             site_year = col_character(),
             met_name_number = col_character(),
             rain = col_double(),
             decile = col_character()))
          



research_sites_sites_step1_2_all <-
  read_csv(
    "X:/Therese_Jackie/Sandy_soils/Development_database/step2/research_sites_sites_step1_2.csv",
    col_types = cols(
      ID =  col_character(),
      site = col_character(),
      year = col_double(),
      plot = col_character(),
      rep_block = col_character(),
      crop = col_character(),
      rip = col_character(),
      rip_depth = col_character(),
      mix = col_character(),
      organic = col_character(),
      organic_rate = col_character(),
      fertiliser = col_character(),
      fertiliser_content = col_character(),
      other_ameliorant = col_character(),
      other_rate = col_character(),
      establishment = col_double(),
      yield = col_double(),
      dry_biomass = col_double(),
      drill_depth = col_character(),
      timing = col_character(),
      sowing_strategy = col_character(),
      comments = col_character(),
      site_sub = col_character(),
      Rip_depth_jax = col_character(),
      amendment_organic = col_character(),
      amendment_fert = col_character(),
      amendment_other = col_character(),
      Descriptors = col_character(),
      Latitude = col_double(),
      Longitude = col_double(),
      Repellence = col_character(),
      Acidity = col_character(),
      Physical = col_character(),
      Nutrient = col_character(),
      other = col_character(),
      Amelioration_Date = col_date(format = "%Y-%m-%d"),
      Amelioration_Year = col_double(),
      yr_post_amelioration = col_double(),
      sowing_date = col_date(format = "%Y-%m-%d"),
      harvest_date = col_date(format = "%Y-%m-%d"),
      previous_crop = col_character(),
      site_year = col_character(),
      met_name_number = col_character(),
      rain = col_double(),
      decile = col_character()))


research_sites_sites_step1_2_all <- research_sites_sites_step1_2_all %>% 
  dplyr::select(-`...43`
                )
    
impact_sites_step1_2_all <- impact_sites_step1_2_all %>% 
  dplyr::rename(  fertiliser_content = fertiliser_rate)

names(impact_sites_step1_2_all)
names(research_sites_sites_step1_2_all)

dim(impact_sites_step1_2_all)
dim(research_sites_sites_step1_2_all)


sites_merged_all <- rbind(impact_sites_step1_2_all, research_sites_sites_step1_2_all)

output_folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/completeDB/"

write.csv(sites_merged_all, paste0(output_folder,"sites_merged_all_messy",".csv"))                           


                                 
                                 
                                 
                                
