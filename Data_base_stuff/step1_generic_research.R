########################################################################################################################
######################            sites                         #############################################
########################################################################################################################


library(ggplot2)
library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)



current.folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/"

# find the files that you want
list.of.files <- list.files(current.folder, ".xlsx",full.names=T) #the trick is getting the full name - just the excel files
list.of.files <- list.files(current.folder, full.names=T) #the trick is getting the full name - all the files
list.of.files

### Sams 
Bute_Sam <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Bute_Trengove_jax.xlsx",
                         sheet = "Database format 2015_2021", skip = 1)
names(Bute_Sam)
Bute_Sam <- Bute_Sam %>% dplyr::select("ID":"comments"   )

### Sams metadata

 Bute_Sam_metadata <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Bute_Trengove_jax.xlsx",
                              sheet = "Site_metadata")

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
names(Bute_CSIRO)
Bute_CSIRO <- Bute_CSIRO %>% 
  rename("comments" ="...25")
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



#3. Yenda



Yenda <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/SNSW Yenda Data Sheet 2021_jax.xlsx",
                          sheet = "Database format Yenda 2017-2021", skip = 0)
 names(Yenda)
 Yenda <- Yenda %>% dplyr::select("ID":"comments"   )

 ### Yenda metadata

 metadata_Yenda <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/SNSW Yenda Data Sheet 2021_jax.xlsx",
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


#4. Ouyen_Spade



Ouyen_Spade <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Ouyen.xlsm",
                          sheet = "Database_format_spade", skip = 1)
 names(Ouyen_Spade)
 Ouyen_Spade <- Ouyen_Spade %>% dplyr::select("ID":"comments"   )
#
### a metadata
#
 metadata_Ouyen_Spade <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Ouyen.xlsm",
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

 Ouyen_Spade <- Ouyen_Spade %>%
   mutate(site = "Ouyen_Spade")
 
 metadata_Ouyen_Spade <- metadata_Ouyen_Spade %>%
   mutate(`Site Name` = "Ouyen_Spade")
 
#5. Lowaldie


 
Lowaldie <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "Lowaldie", ".xlsx"),
                           sheet = "Database format", skip = 1)
#  names(Lowaldie)
Lowaldie <- Lowaldie %>% dplyr::select("ID":"comments"   )
# #
# ### a metadata
# #
metadata_Lowaldie <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "Lowaldie", ".xlsx"),
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
#Brooker

Brooker <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "Murlong_Brooker_v2", ".xlsx"),
                   sheet = "Database format brooker", skip = 1)
#  names(Brooker)
Brooker <- Brooker %>% dplyr::select("ID":"comments"   )
# #
# ### a metadata
# #
metadata_Brooker <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "Murlong_Brooker_v2", ".xlsx"),
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



# # # YoungHusband 

YoungHusband <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Younghusband/", "Sandy Soils Younghusband Data jaxs2020_2021", ".xlsx"),
                    sheet = "Database format 2020_21", skip = 0)
  names(YoungHusband)
 YoungHusband <- YoungHusband %>% dplyr::select("ID":"comments"   )
# #
# ### a metadata
# #
metadata_YoungHusband <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Younghusband/", "Sandy Soils Younghusband Data jaxs2020_2021", ".xlsx"),
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


# Waikerie
# 
Waikerie <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "Waikerie", ".xlsx"),
                   sheet = "Database_format", skip = 1)
names(Waikerie)
Waikerie <- Waikerie %>% dplyr::select("ID":"comments"   )




# #
# ### a metadata
# #
metadata_Waikerie <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "Waikerie", ".xlsx"),
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



#PIRSA_New Horizons

New_Horizons <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "PIRSA_New Horizons_Fraser_2014-2018_jax", ".xlsx"),
                   sheet = "Database format", skip = 0)
names(New_Horizons)
New_Horizons <- New_Horizons %>% dplyr::select("ID":"comments"   )

### a metadata

metadata_New_Horizons <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "PIRSA_New Horizons_Fraser_2014-2018_jax", ".xlsx"),
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

# Murlong

Murlong <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "Murlong_Brooker_v2", ".xlsx"),
                   sheet = "Database format Murlong", skip = 1)
#  names(Murlong)
Murlong <- Murlong %>% dplyr::select("ID":"comments"   )
# #
# ### a metadata
# #
metadata_Murlong <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "Murlong_Brooker_v2", ".xlsx"),
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

#CarwarpAmelioration

CarwarpAmelioration <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "CarwarpAmelioration", ".xlsx"),
                   sheet = "Database_format", skip = 1)
#  names(CarwarpAmelioration)
CarwarpAmelioration <- CarwarpAmelioration %>% dplyr::select("ID":"comments"   )
# #
# ### a metadata
# #
metadata_CarwarpAmelioration <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/", "CarwarpAmelioration", ".xlsx"),
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



#change the name of the site

CarwarpAmelioration <- CarwarpAmelioration %>%
  mutate(site = "Carwarp_Amelioration") 

metadata_CarwarpAmelioration <- metadata_CarwarpAmelioration %>%
  mutate(`Site Name`  = "Carwarp_Amelioration") 


#############################################################################################################################################
########################      Merge the results data   and the metadata                            ########################################
############################################################################################################################################

#-----Research sites bind the primary data------#
site <- rbind(
  Brooker,
  Bute_CSIRO,
  Bute_Sam,
  CarwarpAmelioration,
  Lowaldie,
  Murlong,
  New_Horizons,
  Ouyen_Spade,
  Waikerie,
  Yenda,
  YoungHusband
)

#-----Add in an extra column for the sub site at this stage it is the same ------#
site <- site %>%
  mutate(site_sub = site) 
  

rm(Brooker,
   Bute_CSIRO,
   Bute_Sam,
   CarwarpAmelioration,
   Lowaldie,
   Murlong,
   New_Horizons,
   Ouyen_Spade,
   Waikerie,
   Yenda,
   YoungHusband)
#-----Research sites bind the metadata df ------#
 
str(metadata_Brooker)
str(metadata_Ouyen_Spade)

metadata <- rbind(
  metadata_Brooker,
  metadata_CarwarpAmelioration,
  metadata_Lowaldie,
  metadata_Murlong,
  metadata_New_Horizons,
  metadata_Ouyen_Spade,
  metadata_Waikerie,
  metadata_Yenda,
  metadata_YoungHusband,
  Bute_CSIRO_metadata,
  Bute_Sam_metadata
)




write.csv(metadata,
          paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step1_collating_files/", "research_sites", "_sites_metadata.csv") ,row.names = FALSE)
write.csv(site,
          paste0("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step1_collating_files/", "research_sites", "_results.csv"),row.names = FALSE)
         
