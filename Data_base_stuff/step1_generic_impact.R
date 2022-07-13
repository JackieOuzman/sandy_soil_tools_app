########################################################################################################################
######################            sites                         #############################################
########################################################################################################################


library(ggplot2)
library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)


name_site <- "impact"


current.folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/"

# find the files that you want
list.of.files <- list.files(current.folder, ".xlsx",full.names=T) #the trick is getting the full name - just the excel files
list.of.files <- list.files(current.folder, full.names=T) #the trick is getting the full name - all the files
list.of.files





#Impact_production_data_2021 and older years



site <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/", 
                          "Impact_production_data_2019-2021", ".xlsx"),
                   sheet = "primary_data_all")

site <- site %>% relocate(ID, .before = site)
names(site)
site <- site %>% dplyr::select("ID":"comments"   )

#### a metadata
# #
metadata <- read_excel(paste0("X:/Therese_Jackie/Sandy_soils/Development_database/", 
                              "Impact_production_data_2019-2021", ".xlsx"),
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



site <- site %>%
  mutate(site_sub = site)
 

names(site)
test <- site %>% arrange(plot) %>% arrange(rep_block)




write.csv(metadata,
          paste0("X:/Therese_Jackie/Sandy_soils/Development_database/step1_collating_files/", name_site, "_sites_metadata.csv") ,row.names = FALSE)
write.csv(site,
          paste0("X:/Therese_Jackie/Sandy_soils/Development_database/step1_collating_files/", name_site, "_results.csv"),row.names = FALSE)
         
