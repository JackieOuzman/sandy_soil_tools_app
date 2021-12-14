########################################################################################################################
######################            Murrays sites                            #############################################
########################################################################################################################
library(ggplot2)
library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)

#1. Ouyen
#2. Lowalide
#3. Waikerie
#4. Carwap



### Ouyen has two sets of trials. I need to merge the data into one file.
Ouyen_drill <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Ouyen.xlsm", 
                    sheet = "Database_format_drill", skip = 1)
Ouyen_drill <- Ouyen %>% dplyr::select("site":"comments"   )
Ouyen_spade <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Ouyen.xlsm", 
                          sheet = "Database_format_drill", skip = 1)
names(Ouyen)
Ouyen_spade <- Ouyen %>% dplyr::select("site":"comments"   )

Ouyen <- rbind(Ouyen_drill, Ouyen_spade)

rm(Ouyen_drill,Ouyen_spade)

### Ouyen metadata

Ouyen_metadata <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Ouyen.xlsm", 
                          sheet = "Site_metadata")
