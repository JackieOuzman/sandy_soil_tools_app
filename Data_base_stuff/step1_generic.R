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
#5. Lowaldie
#6. Brooker
#7. YoungHusband
#8. Waikerie
#9. New horizons sites Brimpton Lake, Cadgee, Karoonda
#10. Murlong
#11. impact

#name_site <- "Ouyen_spade"
#name_site <- "Bute"
#name_site <- "Lowaldie"
#name_site <- "Brooker"
#name_site <- "Younghusband"
#name_site <- "Waikerie"
#name_site <- "Brimpton Lake"
#name_site <- "Cadgee"
#name_site <- "Karoonda"
#name_site <- "Murlong"
#name_site <- "Carwarp" #CarwarpAmelioration
#name_site <- "Yenda" 
name_site <- "impact"

current.folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/"

# find the files that you want
list.of.files <- list.files(current.folder, ".xlsx",full.names=T) #the trick is getting the full name - just the excel files
list.of.files <- list.files(current.folder, full.names=T) #the trick is getting the full name - all the files
list.of.files

### Sams 
# Bute_Sam <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Bute_Trengove_jax.xlsx",
#                           sheet = "Database format 2015_2021", skip = 1)
# names(Bute_Sam)
#  Bute_Sam <- Bute_Sam %>% dplyr::select("ID":"comments"   )
# 
# ### Sams metadata
# 
#  Bute_Sam_metadata <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Bute_Trengove_jax.xlsx",
#                               sheet = "Site_metadata")
# 
#  Bute_Sam_metadata <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Bute_Trengove_jax.xlsx",
#                                  sheet = "Site_metadata", col_types = c("text",
#                                                                         "numeric", "numeric", "numeric",
#                                                                         "numeric", "numeric", "numeric",
#                                                                         "numeric", "numeric", "numeric",
#                                                                         "numeric", "numeric", "numeric",
#                                                                         "numeric", "numeric", "numeric",
#                                                                         "numeric", "numeric", "numeric",
#                                                                         "date", "numeric", "text", "text",
#                                                                         "text", "text", "text", "text", "text",
#                                                                         "text",
#                                                                         "date", "date", "date",
#                                                                         "date", "date", "date",
#                                                                         "date",
#                                                                         "date", "date", "date",
#                                                                         "date", "date", "date",
#                                                                         "date"))




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
 #                                  sheet = "Site_metadata", col_types = c("text",
 #                                                                         "numeric", "numeric", "numeric",
 #                                                                         "numeric", "numeric", "numeric",
 #                                                                         "numeric", "numeric", "numeric",
 #                                                                         "numeric", "numeric", "numeric",
 #                                                                         "numeric", "numeric", "numeric",
 #                                                                         "numeric", "numeric", "numeric",
 #                                                                         "date", "numeric", "text", "text",
 #                                                                         "text", "text", "text", "text", "text",
 #                                                                         "text",
 #                                                                         "date", "date", "date",
 #                                                                         "date", "date", "date",
 #                                                                         "date",
 #                                                                         "date", "date", "date",
 #                                                                         "date", "date", "date",
 #                                                                         "date"))


#4. Oyen Spade



# site <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Ouyen.xlsm",
#                           sheet = "Database_format_spade", skip = 1)
#  names(site)
#  site <- site %>% dplyr::select("ID":"comments"   )
# #
# ### a metadata
# #
#  metadata <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Ouyen.xlsm",
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

#5. Lowalide


# 
# site <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", name_site, ".xlsx"),
#                            sheet = "Database format", skip = 1)
# #  names(site)
# site <- site %>% dplyr::select("ID":"comments"   )
# # #
# # ### a metadata
# # #
# metadata <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", name_site, ".xlsx"),
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


# site <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "Murlong_Brooker_v2", ".xlsx"),
#                    sheet = "Database format brooker", skip = 1)
# #  names(site)
# site <- site %>% dplyr::select("ID":"comments"   )
# # #
# # ### a metadata
# # #
# metadata <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "Murlong_Brooker_v2", ".xlsx"),
#                        sheet = "Site_metadata", col_types = c("text",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "date", "numeric", "text", "text",
#                                                               "text", "text", "text", "text", "text",
#                                                               "text",
#                                                               "date", "date", "date",
#                                                               "date", "date", "date",
#                                                               "date",
#                                                               "date", "date", "date",
#                                                               "date", "date", "date",
#                                                               "date"))



# # # YoungHusband 2020 only!!!
# # # 
#  site <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "YoungHusband Sands Impact-SAGIT Data 2020-21_jaxs", ".xlsx"),
#                     sheet = "Database format Young Husband20", skip = 1)
#   names(site)
#  site <- site %>% dplyr::select("ID":"comments"   )
# # #
# # ### a metadata
# # #
# metadata <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "YoungHusband Sands Impact-SAGIT Data 2020-21_jaxs", ".xlsx"),
#                        sheet = "Site_metadata", col_types = c("text",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "date", "numeric", "text", "text",
#                                                               "text", "text", "text", "text", "text",
#                                                               "text",
#                                                               "date", "date", "date",
#                                                               "date", "date", "date",
#                                                               "date",
#                                                               "date", "date", "date",
#                                                               "date", "date", "date",
#                                                               "date"))


# Waikerie
# 
# site <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "Waikerie", ".xlsx"),
#                    sheet = "Database_format", skip = 1)
# names(site)
# site <- site %>% dplyr::select("ID":"comments"   )




# #
# ### a metadata
# #
# metadata <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "Waikerie", ".xlsx"),
#                        sheet = "Site_metadata", col_types = c("text",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "date", "numeric", "text", "text",
#                                                               "text", "text", "text", "text", "text",
#                                                               "text",
#                                                               "date", "date", "date",
#                                                               "date", "date", "date",
#                                                               "date",
#                                                               "date", "date", "date",
#                                                               "date", "date", "date",
#                                                               "date"))



#PIRSA_New Horizons

# site <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "PIRSA_New Horizons_Fraser_2014-2018_jax", ".xlsx"),
#                    sheet = "Database format", skip = 0)
# names(site)
# site <- site %>% dplyr::select("ID":"comments"   )
# 
# 
# 
# 
# # #
# # ### a metadata
# # #
# metadata <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "PIRSA_New Horizons_Fraser_2014-2018_jax", ".xlsx"),
#                        sheet = "Site_metadata", col_types = c("text",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "date", "numeric", "text", "text",
#                                                               "text", "text", "text", "text", "text",
#                                                               "text",
#                                                               "date", "date", "date",
#                                                               "date", "date", "date",
#                                                               "date",
#                                                               "date", "date", "date",
#                                                               "date", "date", "date",
#                                                               "date"))

# #Murlong
# 
# site <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "Murlong_Brooker_v2", ".xlsx"),
#                    sheet = "Database format Murlong", skip = 1)
# #  names(site)
# site <- site %>% dplyr::select("ID":"comments"   )
# # #
# # ### a metadata
# # #
# metadata <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "Murlong_Brooker_v2", ".xlsx"),
#                        sheet = "Site_metadata", col_types = c("text",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "date", "numeric", "text", "text",
#                                                               "text", "text", "text", "text", "text",
#                                                               "text",
#                                                               "date", "date", "date",
#                                                               "date", "date", "date",
#                                                               "date",
#                                                               "date", "date", "date",
#                                                               "date", "date", "date",
#                                                               "date"))

#CarwarpAmelioration

# site <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "CarwarpAmelioration", ".xlsx"),
#                    sheet = "Database_format", skip = 1)
# #  names(site)
# site <- site %>% dplyr::select("ID":"comments"   )
# # #
# # ### a metadata
# # #
# metadata <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "CarwarpAmelioration", ".xlsx"),
#                        sheet = "Site_metadata", col_types = c("text",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "numeric", "numeric", "numeric",
#                                                               "date", "numeric", "text", "text",
#                                                               "text", "text", "text", "text", "text",
#                                                               "text",
#                                                               "date", "date", "date",
#                                                               "date", "date", "date",
#                                                               "date",
#                                                               "date", "date", "date",
#                                                               "date", "date", "date",
#                                                               "date"))








#Impact_production_data_2021 and older years

site <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "Impact_production_data_2019-2021", ".xlsx"),
                    sheet = "primary data 2019 2020 2021")

site <- site %>% relocate(ID, .before = site)
names(site)
site <- site %>% dplyr::select("ID":"comments"   )
# #
# ### a metadata
# #
metadata <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "Impact_production_data_2019-2021", ".xlsx"),
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


#site <- rbind(Bute_Sam, Bute_CSIRO)
site <- site %>%
  mutate(site_sub = site)
  #mutate(site_sub = "CarwarpAmelioration")
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



#filter on site name
unique(metadata$`Site Name`)
unique(site$site)
name_site




# metadata <- metadata %>%
#    filter(`Site Name`== name_site)
# site <- site %>%
#   filter(site== name_site)

names(site)
test <- site %>% arrange(plot) %>% arrange(rep_block)

duplicated_ID <- site %>% 
  group_by(ID) %>% 
  summarise(count_ID = n()) 
  


write.csv(metadata,
          paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step1_collating_files/", name_site, "_sites_metadata.csv") ,row.names = FALSE)
write.csv(site,
          paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step1_collating_files/", name_site, "_results.csv"),row.names = FALSE)
         
