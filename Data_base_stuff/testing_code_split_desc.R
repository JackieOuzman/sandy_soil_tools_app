
library(ggplot2)
library(readxl)
library(tidyverse)
#library(multcompView)

library(plotly)

library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)
library(hrbrthemes)
library(stringr)



data_file <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/sites_merged.csv"
data_file_control <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/sites_control_merged.csv"

summary_control_data_all <- read_csv(data_file_control, 
                                     col_types = cols(rep_block = col_character()))

##label problem with Mt Damper
summary_control_data_all <- summary_control_data_all %>% 
  mutate(
    site_sub = case_when(
      site_sub == "Mt Damper" ~ "Mt_Damper",
      TRUE ~ site_sub))

##label problem with Ouyen
summary_control_data_all <- summary_control_data_all %>% 
  mutate(
    site_sub = case_when(
      site_sub == "Oyen_Spade" ~ "Ouyen_Spade",
      TRUE ~ site_sub))
summary_control_data_all <- summary_control_data_all %>% 
  mutate(
    site = case_when(
      site == "Oyen_Spade" ~ "Ouyen_Spade",
      TRUE ~ site))


## list of sites that are ready ish

list_sites_ready <- c("Brimpton Lake",
                      "Brooker",
                      "Buckleboo",
                      "Bute_CSIRO",
                      "Bute_Trengrove",
                      "Cadgee",
                      "Carwarp_Amelioration",
                      "Cummins",
                      "Karkoo",
                      "Karoonda",
                      "Kooloonong_canola",
                      "Kooloonong_chickpea",
                      "Kooloonong_lentil",
                      "Kooloonong_lupin",
                      "Kybunga",#newly added
                      "Lowaldie_Crest",
                      "Lowaldie_Deep sand",
                      "Malinong", #newly added
                      "Monia_Gap", #newly added
                      "Mt Damper",
                      "Mt_Damper",
                      "Murlong",
                      "Oyen_Spade",
                      "Ouyen_Spade",
                      "Sherwood",
                      "Telopea_Downs",
                      "Tempy",
                      "Waikerie",
                      "Warnertown", #newly added
                      "Wynarka",
                      "Yenda"
                      #"Younghusband"
)

summary_control_data_all <- summary_control_data_all %>% 
  filter(site_sub %in% list_sites_ready  )




## how many different modification?
#####################################################################################
#### split the Descriptors into modification and amendments
#####################################################################################


#####################################################################################
#####  soil modifications      #####
#####################################################################################
summary_control_data_all <- summary_control_data_all %>%
  distinct(site, Descriptors, .keep_all=TRUE) %>%
  select(site,
         Descriptors )
summary_control_data_all <- summary_control_data_all %>% arrange(site,Descriptors )

### get the soil modification from the Descriptors

#1 soil modification with depth
summary_control_data_all <- summary_control_data_all %>% 
  mutate(soil_modification_depth =  str_extract(summary_control_data_all$Descriptors, "[^_]+"))#keep everything before the first_

#how many modification_depth
soil_modification_depth <- summary_control_data_all %>% 
  distinct(soil_modification_depth) %>% 
  arrange(soil_modification_depth) %>% 
  filter(soil_modification_depth != "Unmodified")
  
count(soil_modification_depth) #20 excluding the unmodified


#1a working clms - soil modification without depth 1 only
summary_control_data_all <- summary_control_data_all %>% 
  mutate(soil_modification_1 =  str_extract(summary_control_data_all$Descriptors, "[^.]+"))#keep everything before the first .

#the Unmodified seems to have a problem this fixes it :)
summary_control_data_all <- summary_control_data_all %>% 
  mutate(soil_modification_1 = str_extract(summary_control_data_all$soil_modification_1, "[^_]+")) #keep everything after the first _

#1a working clms - 2 soil modification without depth 2
summary_control_data_all <- summary_control_data_all %>% 
  mutate(soil_modification_2 = case_when(
    soil_modification_depth == "Rip.50Spade.30" ~        "Rip+Spade",
    soil_modification_depth == "Rip.60Spade.30" ~        "Rip+Spade",
    soil_modification_depth == "Rip.45IncRip+Spade.30" ~ "IncRip+Spade",
    soil_modification_depth == "Rip.60IncRip+Spade.30" ~ "IncRip+Spade",
    TRUE ~ "NA"
  ))

	
#2 all soil modification without depth (if 2 soil modification used keep this)	

summary_control_data_all <- summary_control_data_all %>% 
  mutate(soil_modification = case_when(
    soil_modification_2 == "NA" ~ soil_modification_1,
    TRUE ~ soil_modification_2
  ))
	
	
### remove the working clms
summary_control_data_all <- summary_control_data_all %>% 
  select(-soil_modification_1,
         -soil_modification_2)


#how many modification without depth
soil_modification_depth <- summary_control_data_all %>% 
  distinct(soil_modification) %>% 
  arrange(soil_modification) %>% 
  filter(soil_modification != "Unmodified")

count(soil_modification_depth) #7 excluding the unmodified 



#####################################################################################
#####  soil amendments      #####
#####################################################################################
back_up <- summary_control_data_all
### get the soil Amendment from the Descriptors
# gets all the amendments
summary_control_data_all <- summary_control_data_all %>% 
  mutate(amendment_all = sub("^[^_]*_", "", summary_control_data_all$Descriptors)) #keep everything after the first _
#pull out the first amendment listed but has rates
summary_control_data_all <- summary_control_data_all %>% 
  mutate(amendment_1 = str_extract(summary_control_data_all$amendment_all, "[^.]+"))#keep everything before the first 
#removed the rates in the first amendment listed 
summary_control_data_all <- summary_control_data_all %>% 
  mutate(amendment_1 = str_extract(summary_control_data_all$amendment_1, "[^@]+")) #keep everything before the first @


#pull out the second amendment listed but has rates (_/.)
summary_control_data_all <- summary_control_data_all %>% 
  mutate(amendment_2a = sub("^[^.]*.", "", summary_control_data_all$amendment_all)) 
summary_control_data_all <- summary_control_data_all %>% 
  mutate(amendment_2b = sub("^[^.]*.", "", summary_control_data_all$amendment_2a)) 
summary_control_data_all <- summary_control_data_all %>% 
  mutate(amendment_2 = str_extract(summary_control_data_all$amendment_2b, "[^.]+"))

### not quite right remove some problems!
unique(summary_control_data_all$amendment_2)

summary_control_data_all <- summary_control_data_all %>% 
  mutate(amendment_2 = case_when(
    amendment_2 ==  "surface_Yr18,19,20" ~ "",
    amendment_2 ==  "incorp_50" ~ "",
    amendment_2 ==  "band_30" ~ "",
    amendment_2 ==  "band_50" ~ "",
    amendment_2 ==  "surface" ~ "",
    TRUE ~ amendment_2
  ))
unique(summary_control_data_all$amendment_2)



#pull out the thrid amendment listed but has rates (_/.)
summary_control_data_all <- summary_control_data_all %>% 
  mutate(amendment_3a = sub("^[^.]*.", "", summary_control_data_all$amendment_2b)) 
summary_control_data_all <- summary_control_data_all %>% 
  mutate(amendment_3b = sub("^[^.]*.", "", summary_control_data_all$amendment_3a)) 
summary_control_data_all <- summary_control_data_all %>% 
  mutate(amendment_3 = str_extract(summary_control_data_all$amendment_3b, "[^.]+"))





### bugger the timing year for amenedment 2 is stuffed up!
unique(summary_control_data_all$amendment_1)
unique(summary_control_data_all$amendment_2)
unique(summary_control_data_all$amendment_3)



## amendments 1, 2, 3 together.
str(summary_control_data_all)


summary_control_data_all$amendment_1 <-  
  stringr::str_replace_na(summary_control_data_all$amendment_1 , replacement='')

summary_control_data_all$amendment_2 <-  
  stringr::str_replace_na(summary_control_data_all$amendment_2 , replacement='')  

summary_control_data_all$amendment_3 <-  
  stringr::str_replace_na(summary_control_data_all$amendment_3 , replacement='')


summary_control_data_all <-  summary_control_data_all %>% 
  mutate(amendment_123 = paste0(amendment_1,
                                amendment_2,
                                amendment_3))


#tidy up working outs
summary_control_data_all <- summary_control_data_all %>%
  select(-amendment_2a, -amendment_2b, -amendment_3a, -amendment_3b)

unique(summary_control_data_all$amendment_123)
#how many amendment without depth
amendment_no_depth <- summary_control_data_all %>% 
  distinct(amendment_123) %>% 
  arrange(amendment_123) %>% 
  filter(amendment_123 != "none")

count(amendment_no_depth) #26 excluding the none 
