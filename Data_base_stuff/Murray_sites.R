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


Ouyen <- rbind(Ouyen_drill, Ouyen_spade)

Ouyen <- Ouyen %>% dplyr::select("site":"comments"   )

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
########################      Merge the results data   and the metadata                            ########################################
############################################################################################################################################


Murryays_sites <- rbind(Carwarp, Waikerie, Lowalide, Ouyen)


rm(Carwarp,Waikerie,Lowalide, Ouyen )
Murrays_sites_metadata <- rbind(Ouyen_metadata, Carwarp_metadata, Waikerie_metadata, Lowalide_metadata)

rm(Carwarp_metadata,Waikerie_metadata,Lowalide_metadata, Ouyen_metadata )

#### write out these file and then start with climate data and descriptors
  
write.csv(Murrays_sites_metadata,"X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Murrays_sites_metadata.csv" ,row.names = FALSE)
write.csv(Murryays_sites,"X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Murrays_sites.csv" , row.names = FALSE)

#############################################################################################################################################
########################      soil constraints                                        ########################################
############################################################################################################################################

Ouyen_soils_constraints <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Ouyen.xlsm", 
                    sheet = "site_table")
Lowalide_soils_constraints <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Lowaldie.xlsx",
                       sheet = "site_table")
Waikerie_soils_constraints <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Waikerie.xlsx",
                       sheet = "site_table")
Carwarp_soils_constraints <- read_excel("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/CarwarpAmelioration.xlsx",
                      sheet = "site_table")

Murrays_sites_soil_constraints <- rbind(Ouyen_soils_constraints, Lowalide_soils_constraints, 
                                        Waikerie_soils_constraints, Carwarp_soils_constraints)
write.csv(Murrays_sites_soil_constraints,
          "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Murrays_sites_soil_constraints.csv" ,
          row.names = FALSE)
