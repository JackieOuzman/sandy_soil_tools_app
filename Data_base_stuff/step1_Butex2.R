########################################################################################################################
######################            Bute sites                         #############################################
########################################################################################################################
library(ggplot2)
library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)

#1. Sams
#2. CSIRO


current.folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/"

# find the files that you want
list.of.files <- list.files(current.folder, ".xlsx",full.names=T) #the trick is getting the full name - just the excel files
#list.of.files <- list.files(current.folder, full.names=T) #the trick is getting the full name - all the files
list.of.files

### Sams 
Bute_Sam <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Bute_Trengove_jax.xlsx",
                          sheet = "Database format 2015_2021", skip = 1)
names(Bute_Sam)
Bute_Sam <- Bute_Sam %>% dplyr::select("ID":"comments"   )

### Sams metadata

# Bute_Sam_metadata <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Bute_Trengove_jax.xlsx", 
#                              sheet = "Site_metadata")

Bute_Sam_metadata <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Bute_Trengove_jax.xlsx", 
                                sheet = "Site_metadata", col_types = c("text", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "date", "numeric", "text", "text", 
                                                                       "text", "text", "text", "text", "text", 
                                                                       "text", 
                                                                       "date", "date", "date", 
                                                                       "date", "date", "date", 
                                                                       "date",
                                                                       "date", "date", "date", 
                                                                       "date", "date", "date", 
                                                                       "date"))




### CSIRO 
Bute_CSIRO <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Bute_Trial_2018_2021.xlsx",
                       sheet = "2018_2021", skip = 1)

Bute_CSIRO <- Bute_CSIRO %>% dplyr::select("ID":"comments"   )

### CSIRO metadata

Bute_CSIRO_metadata <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Bute_Trial_2018_2021.xlsx", 
                                  sheet = "Site_metadata", col_types = c("text", 
                                                                         "numeric", "numeric", "numeric", 
                                                                         "numeric", "numeric", "numeric", 
                                                                         "numeric", "numeric", "numeric", 
                                                                         "numeric", "numeric", "numeric", 
                                                                         "numeric", "numeric", "numeric", 
                                                                         "numeric", "numeric", "numeric", 
                                                                         "date", "numeric", "text", "text", 
                                                                         "text", "text", "text", "text", "text", 
                                                                         "text", 
                                                                         "date", "date", "date", 
                                                                         "date", "date", "date", 
                                                                         "date",
                                                                         "date", "date", "date", 
                                                                         "date", "date", "date", 
                                                                         "date"))




#############################################################################################################################################
########################      Merge the results data   and the metadata                            ########################################
############################################################################################################################################


Butes_sites <- rbind(Bute_Sam, Bute_CSIRO)
Butes_sites <- Butes_sites %>% 
  mutate(site_sub = site)

rm(Bute_Sam,Bute_CSIRO )



Butes_sites_metadata <- rbind(Bute_Sam_metadata, Bute_CSIRO_metadata)

rm(Bute_Sam_metadata,Bute_CSIRO_metadata )

# #### write out these file and then start with climate data and descriptors 
#   
write.csv(Butes_sites_metadata,"X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step1_collating_files/Butes_sites_metadata.csv" ,row.names = FALSE)
write.csv(Butes_sites,"X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step1_collating_files/Butes_results.csv" , row.names = FALSE)

#############################################################################################################################################
########################      soil constraints                                        ########################################
############################################################################################################################################

names(Butes_sites_metadata)

Butes_sites_soil_constraints <- Butes_sites_metadata %>% 
  dplyr::select(Repellence:other)
  
Butes_sites_soil_constraints


#### write out the file - 
 write.csv(Butes_sites_soil_constraints,
           "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step1_collating_files/Butes_sites_soil_constraints.csv" ,
           row.names = FALSE)

