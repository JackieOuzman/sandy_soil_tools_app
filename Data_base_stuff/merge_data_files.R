### merge all the data

library(ggplot2)
library(readxl)
library(tidyverse)
library(multcompView)
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

sites_merged <- rbind(Brimpton.Lake_sites_step1_2_neat.csv, 
              Brooker_sites_step1_2_neat.csv,
              Bute_sites_step1_2_neat.csv,
              Cadgee_sites_step1_2_neat.csv,
              Carwarp_sites_step1_2_neat.csv,
              Karoonda_sites_step1_2_neat.csv,
              Lowaldie_sites_step1_2_neat.csv ,
              Murlong_sites_step1_2_neat.csv ,
              Ouyen_spade_sites_step1_2_neat.csv ,
              Waikerie_sites_step1_2_neat.csv  ,  
              Yenda_sites_step1_2_neat.csv,
              YoungHusband_sites_step1_2_neat.csv)


sites_merged %>% 
  distinct(Descriptors) %>% 
  arrange(desc(Descriptors))

## we want them in a certain order - probably the best way would be to assign a number to a new clm
sites_merged <- sites_merged %>% 
  mutate(Descriptors_order = case_when(
    Descriptors == "Control" ~ 0,
    
    Descriptors == "Unmodified_SE14.band_8" ~ 1,
    Descriptors == "Unmodified_Bi-Agra.surface+band_8" ~ 2,
    Descriptors == "Unmodified_Lc.surface" ~ 3,
    Descriptors == "Unmodified_Cl.surface" ~ 4,
    Descriptors == "Unmodified_Cl.incorp_8" ~ 5,
    
   
    Descriptors == "Unmodified_Fert.surface" ~ 6,
    Descriptors == "Unmodified_Fert.foliar" ~ 7,
    Descriptors == "Unmodified_Fert.incorp_8" ~ 8,
    Descriptors == "Unmodified_Fert.band_8" ~ 9,
    Descriptors == "Unmodified_Fert.band_30" ~ 10,
    
    Descriptors == "Unmodified_Clay.incorp_10" ~ 11,
    Descriptors == "Unmodified_Clay.incorp_8" ~ 12,
    
    Descriptors == "Unmodified_K_added.surface" ~ 13,
    Descriptors == "Unmodified_Fert.band_30.Clay.incorp_10" ~ 14,
    
    
    Descriptors == "Spade.30_Lc@1.incorp_30" ~ 15,
    Descriptors == "Spade.30_Lc@2.incorp_30" ~ 16,
    Descriptors == "Spade.30_Lc@4.incorp_30" ~ 17,
    Descriptors == "Spade.30_Lc@6.incorp_30" ~ 18,
    Descriptors == "Spade.30_Lc@8.incorp_30" ~ 19,
    Descriptors == "Spade.30_Lc@10.incorp_30" ~ 20,
    Descriptors == "Spade.30_Lc@15.incorp_30" ~ 21,
    Descriptors == "Spade.30_Lc@20.incorp_30" ~ 22,
    
    Descriptors == "Spade.30_Lc@1.incorp_30.K_added.surface" ~ 23,
    Descriptors == "Spade.30_Lc@2.incorp_30.K_added.surface" ~ 24,
    Descriptors == "Spade.30_Lc@4.incorp_30.K_added.surface" ~ 25,
    Descriptors == "Spade.30_Lc@6.incorp_30.K_added.surface" ~ 26,
    Descriptors == "Spade.30_Lc@8.incorp_30.K_added.surface" ~ 27,
    Descriptors == "Spade.30_Lc@10.incorp_30.K_added.surface" ~ 28,
    Descriptors == "Spade.30_Lc@15.incorp_30.K_added.surface" ~ 29,
    Descriptors == "Spade.30_Lc@20.incorp_30.K_added.surface" ~ 30,
    
    
    Descriptors == "Spade.30_none" ~ 31,
    Descriptors == "Spade.30_Lc.incorp_30" ~ 32,
    Descriptors == "Spade.30_Cl.incorp_30" ~ 33,
    Descriptors == "Spade.30_Com.incorp_30" ~ 34,
    Descriptors == "Spade.30_Cereal.incorp_30" ~ 35,
    
    
    Descriptors == "Spade.30_K_added.surface" ~ 36,
    Descriptors == "Spade.30_Fert.incorp_30.K_added.incorp_30" ~ 37,
    Descriptors == "Spade.30_Fert.incorp_30" ~ 38,
    Descriptors == "Spade.30_Fert.incorp_30.Clay.incorp_30" ~ 39,
    Descriptors == "Spade.30_Clay.incorp_30" ~ 40,
    Descriptors == "Spade.30_Lc.incorp_30.Clay.incorp_30" ~ 41,
    Descriptors == "Spade.30_Lc.incorp_30.Fert.incorp_30" ~ 42,
    Descriptors == "Spade.30_Lc.incorp_30.Fert.incorp_30.Clay.incorp_30" ~ 43,
    
    Descriptors == "Spade.30_Vetch.incorp_30" ~ 44,
    Descriptors == "Spade.30_Vet_Cer_In.incorp_30" ~ 45,
    Descriptors == "Spade.30_Vet_Cer.incorp_30" ~ 46,
    
    
    Descriptors == "Rip.30_none" ~ 47,
    Descriptors == "Rip.30_Cl.surface" ~ 48,
    Descriptors == "Rip.30_Cl.band_30" ~ 49,
    #Descriptors == "" ~ 1,
    Descriptors == "Rip.30_Fert.band_8" ~ 50,
    Descriptors == "Rip.30_Fert.band_30" ~ 51,
    Descriptors == "Rip.30_Fert.incorp_30" ~ 52,
    Descriptors == "Rip.30_Lc.incorp_30" ~ 53,
    
    Descriptors == "Rip.40_none" ~ 54,
    Descriptors == "Rip.41_none" ~ 55,
    Descriptors == "Rip.41_Lc.incorp_41" ~ 56,
    Descriptors == "Rip.41_Fert.incorp_41" ~ 57,
    Descriptors == "Rip.60_none" ~ 58,
    Descriptors == "Rip.60_Lc.incorp_60" ~ 59,
    Descriptors == "Rip.60_Fert.band_8" ~ 60,
    Descriptors == "Rip.60_Cl.surface" ~ 61,
    Descriptors == "Rip.60_Cl.band_60" ~ 62,
    Descriptors == "Rip.60_Fert.band_60" ~ 63,
    Descriptors == "Rip.30+60_none" ~ 64,
    Descriptors == "Rip.60Spade.30_none" ~ 65,
    Descriptors == "Rip.60Spade.30_Lc.band_30+60" ~ 66,
    Descriptors == "Rip.30+60_Lc.band_30+60" ~ 67,
    
    Descriptors == "Rip.50_none" ~ 68,
    Descriptors == "Rip.50_Cl.surface" ~ 69,
    Descriptors == "Rip.50_Cl.incorp_50" ~ 70,
    Descriptors == "Rip.50_Cl.band_50" ~ 71,
    Descriptors == "Rip.50_Fert.surface" ~ 72,
    Descriptors == "Rip.50_Clay.incorp_50" ~ 73,
    Descriptors == "Inc.50_none" ~ 74,
    Descriptors == "Inc.50_Cl.incorp_50" ~ 75,
    
    Descriptors == "Rip.50IncRip_none" ~ 76,
    
    Descriptors == "Delving.18_none" ~ 78,
    Descriptors == "Delving.18_SE14.band_8" ~ 79,
    Descriptors == "Sweep.30_none" ~ 80,
    Descriptors == "Sweep.30_Cl.incorp_30" ~ 81,
    Descriptors == "Sweep.30_Lime.incorp_30" ~ 82,
    Descriptors == "Sweep.30_Cl.incorp_30.Clay.incorp_8" ~ 83,
    
    
    TRUE                      ~ 9999
    
  ))

                         
output_folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/"

write.csv(sites_merged, paste0(output_folder,"sites_merged",".csv"))                           
  

                        
                              
                                 
                                 
                                 
                                
