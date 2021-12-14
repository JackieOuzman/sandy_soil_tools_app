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


### Lowalide 
Lowalide <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Lowaldie.xlsx",
                          sheet = "Database format", skip = 1)
names(Lowalide)
Lowalide <- Lowalide %>% dplyr::select("site":"comments"   )

### Lowalide metadata

Lowalide_metadata <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Lowaldie.xlsx", 
                             sheet = "Site_metadata")


### Waikerie 
Waikerie <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Waikerie.xlsx",
                       sheet = "Database_format", skip = 1)
names(Waikerie)
Waikerie <- Waikerie %>% dplyr::select("site":"comments"   )

### Waikerie metadata

Waikerie_metadata <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Waikerie.xlsx", 
                                sheet = "Site_metadata")

### Carwarp 
Carwarp <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/CarwarpAmelioration.xlsx",
                       sheet = "Database_format", skip = 1)
names(Carwarp)
Carwarp <- Carwarp %>% dplyr::select("site":"comments"   )

### Carwarp metadata

Carwarp_metadata <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/CarwarpAmelioration.xlsx", 
                                sheet = "Site_metadata")

#############################################################################################################################################

