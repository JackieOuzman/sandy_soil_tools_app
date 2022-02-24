### merge all the CONTROL data


library(readxl)
library(tidyverse)

#################################################################################################################
####   Get the data #####

current.folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2"


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


sites_merged_control %>% 
  distinct(Descriptors) %>% 
  arrange(desc(Descriptors))


## are there any reps in the ID clm?

duplicated_ID <- sites_merged_control %>% 
  group_by( ID  ) %>% 
  filter(n()>1)
                         
output_folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/"

write.csv(sites_merged_control, paste0(output_folder,"sites_control_merged",".csv"))                           
  

                        
                              
                                 
                                 
                                 
                                
