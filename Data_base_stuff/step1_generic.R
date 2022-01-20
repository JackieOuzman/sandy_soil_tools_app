########################################################################################################################
######################            sites                         #############################################
########################################################################################################################
library(ggplot2)
library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)

#Processed files:
#1. Sams Bute
#2. CSIRO Bute
#3. Yenda
#4. Ouyen spade

name_site <- "Ouyen_spade"

current.folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/"

# find the files that you want
list.of.files <- list.files(current.folder, ".xlsx",full.names=T) #the trick is getting the full name - just the excel files
list.of.files <- list.files(current.folder, full.names=T) #the trick is getting the full name - all the files
list.of.files

### Sams 
# Bute_Sam <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Bute_Trengove_jax.xlsx",
#                           sheet = "Database format 2015_2021", skip = 1)
# names(Bute_Sam)
# Bute_Sam <- Bute_Sam %>% dplyr::select("ID":"comments"   )

### Sams metadata

# Bute_Sam_metadata <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Bute_Trengove_jax.xlsx",
#                              sheet = "Site_metadata")

# Bute_Sam_metadata <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Bute_Trengove_jax.xlsx",
#                                 sheet = "Site_metadata", col_types = c("text",
#                                                                        "numeric", "numeric", "numeric",
#                                                                        "numeric", "numeric", "numeric",
#                                                                        "numeric", "numeric", "numeric",
#                                                                        "numeric", "numeric", "numeric",
#                                                                        "numeric", "numeric", "numeric",
#                                                                        "numeric", "numeric", "numeric",
#                                                                        "date", "numeric", "text", "text",
#                                                                        "text", "text", "text", "text", "text",
#                                                                        "text",
#                                                                        "date", "date", "date",
#                                                                        "date", "date", "date",
#                                                                        "date",
#                                                                        "date", "date", "date",
#                                                                        "date", "date", "date",
#                                                                        "date"))




### CSIRO 
# Bute_CSIRO <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Bute_Trial_2018_2021.xlsx",
#                        sheet = "2018_2021", skip = 1)
# 
# Bute_CSIRO <- Bute_CSIRO %>% dplyr::select("ID":"comments"   )
# 
# ### CSIRO metadata
# 
# Bute_CSIRO_metadata <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Bute_Trial_2018_2021.xlsx",
#                                   sheet = "Site_metadata", col_types = c("text",
#                                                                          "numeric", "numeric", "numeric",
#                                                                          "numeric", "numeric", "numeric",
#                                                                          "numeric", "numeric", "numeric",
#                                                                          "numeric", "numeric", "numeric",
#                                                                          "numeric", "numeric", "numeric",
#                                                                          "numeric", "numeric", "numeric",
#                                                                          "date", "numeric", "text", "text",
#                                                                          "text", "text", "text", "text", "text",
#                                                                          "text",
#                                                                          "date", "date", "date",
#                                                                          "date", "date", "date",
#                                                                          "date",
#                                                                          "date", "date", "date",
#                                                                          "date", "date", "date",
#                                                                          "date"))



#3. Yenda



# site <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/SNSW Yenda Data Sheet 2021_jax.xlsx",
#                          sheet = "Database format Yenda 2017-2021", skip = 0)
# names(site)
# site <- site %>% dplyr::select("ID":"comments"   )
# 
# ### Yenda metadata
# 
# metadata <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/SNSW Yenda Data Sheet 2021_jax.xlsx",
#                                   sheet = "Site_metadata", col_types = c("text", 
#                                                                          "numeric", "numeric", "numeric", 
#                                                                          "numeric", "numeric", "numeric", 
#                                                                          "numeric", "numeric", "numeric", 
#                                                                          "numeric", "numeric", "numeric", 
#                                                                          "numeric", "numeric", "numeric", 
#                                                                          "numeric", "numeric", "numeric", 
#                                                                          "date", "numeric", "text", "text", 
#                                                                          "text", "text", "text", "text", "text", 
#                                                                          "text", 
#                                                                          "date", "date", "date", 
#                                                                          "date", "date", "date", 
#                                                                          "date",
#                                                                          "date", "date", "date", 
#                                                                          "date", "date", "date", 
#                                                                          "date"))


#4. Oyen Spade



 site <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Ouyen.xlsm",
                          sheet = "Database_format_spade", skip = 1)
 names(site)
 site <- site %>% dplyr::select("ID":"comments"   )
# 
### a metadata
# 
 metadata <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Ouyen.xlsm",
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
str(site)

# site <- rbind(Bute_Sam, Bute_CSIRO)
site <- site %>%
  mutate(site_sub = site)
  #mutate(site_sub = "Ouyen_spade")

#rm(Bute_Sam,Bute_CSIRO )

#metadata <- rbind(Bute_Sam_metadata, Bute_CSIRO_metadata)

#rm(Bute_Sam_metadata,Bute_CSIRO_metadata )

# #### write out these file and then start with climate data and descriptors 
#   
#write.csv(metadata,"X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step1_collating_files/Butes_sites_metadata.csv" ,row.names = FALSE)
#write.csv(site,"X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step1_collating_files/Butes_results.csv" , row.names = FALSE)

# write.csv(metadata,"X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step1_collating_files/Yenda_site_metadata.csv" ,row.names = FALSE)
# write.csv(site,"X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step1_collating_files/Yenda_results.csv" , row.names = FALSE)

### writing out files using name of site define at top of script


write.csv(metadata,
          paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step1_collating_files/", name_site, "_sites_metadata.csv") ,row.names = FALSE)
write.csv(site,
          paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step1_collating_files/", name_site, "_results.csv"),row.names = FALSE)
         
