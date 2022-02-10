### merge all the CONTROL data


library(readxl)
library(tidyverse)

#################################################################################################################
####   Get the data #####

current.folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2"
# find the files that you want

list.of.files <- list.files(current.folder, "control.csv",full.names=T) 

list.of.files






list2env(
  lapply(setNames(list.of.files, 
                  make.names(gsub("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2/",  "", list.of.files))),
         read.csv), envir = .GlobalEnv)





sites_merged_control <- rbind(
  Brimpton.Lake_sites_step1_2_control.csv,
  Brooker_sites_step1_2_control.csv   ,
  Bute_sites_step1_2_control.csv,
  Cadgee_sites_step1_2_control.csv  ,
  Carwarp_sites_step1_2_control.csv ,
  Karoonda_sites_step1_2_control.csv ,
  Lowaldie_sites_step1_2_control.csv ,
  Murlong_sites_step1_2_control.csv ,
  Ouyen_spade_sites_step1_2_control.csv  ,
  Waikerie_sites_step1_2_control.csv,
  Yenda_sites_step1_2_control.csv,
  YoungHusband_sites_step1_2_control.csv
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
  

                        
                              
                                 
                                 
                                 
                                
