### merge all the data


library(readxl)
library(tidyverse)

#################################################################################################################
####   Get the data #####

current.folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2"
# find the files that you want

list.of.files <- list.files(current.folder, "neat.csv",full.names=T) 
list.of.files






list2env(
  lapply(setNames(list.of.files, 
                  make.names(gsub("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2/",  "", list.of.files))),
         read.csv), envir = .GlobalEnv)

# sites_merged <- rbind(Brimpton.Lake_sites_step1_2_neat.csv, 
#               Brooker_sites_step1_2_neat.csv,
#               Bute_sites_step1_2_neat.csv,
#               Cadgee_sites_step1_2_neat.csv,
#               Carwarp_sites_step1_2_neat.csv,
#               Karoonda_sites_step1_2_neat.csv,
#               Lowaldie_sites_step1_2_neat.csv ,
#               Murlong_sites_step1_2_neat.csv ,
#               Ouyen_spade_sites_step1_2_neat.csv ,
#               Waikerie_sites_step1_2_neat.csv  ,  
#               Yenda_sites_step1_2_neat.csv,
#               YoungHusband_sites_step1_2_neat.csv,
#               impact_sites_step1_2_neat.csv)

sites_merged <- rbind(impact_sites_step1_2_neat.csv, research_sites_sites_step1_2_neat.csv)

  
  
  sites_merged %>% 
  distinct(Descriptors) %>% 
  arrange(desc(Descriptors))

str(sites_merged$rep_block)
sites_merged %>%  distinct(rep_block)
                         
output_folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/"

write.csv(sites_merged, paste0(output_folder,"sites_merged",".csv"))                           
  

                        
                              
                                 
                                 
                                 
                                
