### merge all the data


library(readxl)
library(tidyverse)

#################################################################################################################
####   Get the data site trial data #####

current.folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2"




impact_sites_step1_2 <- 
  read_csv("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2/impact_sites_step1_2_neat.csv", 
           col_types = cols(yield = col_double()))

research_sites_sites_step1_2 <-
  read.csv("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2/research_sites_sites_step1_2_neat.csv")



sites_merged <- rbind(impact_sites_step1_2, research_sites_sites_step1_2)

  

                         
output_folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/"

write.csv(sites_merged, paste0(output_folder,"sites_merged",".csv"))                           



#################################################################################################################
####   Get the data site trial CONTROL data #####

impact_sites_step1_2_control <- 
  read_csv("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2/impact_sites_step1_2_control.csv", 
           col_types = cols(yield = col_double(),
                            control_yield = col_double()))

research_sites_sites_step1_2_control <-
  read.csv("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2/research_sites_sites_step1_2_control.csv")

str(impact_sites_step1_2_control)
str(research_sites_sites_step1_2_control$yield)

sites_merged_control <- rbind(
  impact_sites_step1_2_control,
  research_sites_sites_step1_2_control
)



output_folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/"

write.csv(sites_merged_control, paste0(output_folder,"sites_control_merged",".csv"))            

                        
                              
                                 
                                 
                                 
                                
