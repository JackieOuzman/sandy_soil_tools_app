### merge all the data


library(readxl)
library(tidyverse)

#################################################################################################################
####   Get the data #####

current.folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2"




impact_sites_step1_2 <- 
  read_csv("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2/impact_sites_step1_2_neat.csv", 
           col_types = cols(yield = col_double()))

research_sites_sites_step1_2 <-
  read.csv("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2/research_sites_sites_step1_2_neat.csv")



sites_merged <- rbind(impact_sites_step1_2, research_sites_sites_step1_2)

  
  
  sites_merged %>% 
  distinct(Descriptors) %>% 
  arrange(desc(Descriptors))

str(sites_merged$rep_block)
sites_merged %>%  distinct(rep_block)
                         
output_folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/"

write.csv(sites_merged, paste0(output_folder,"sites_merged",".csv"))                           
  

                        
                              
                                 
                                 
                                 
                                
