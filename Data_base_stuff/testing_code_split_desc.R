
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




str(summary_control_data_all)
#keep the rep but not the year
cum_yld_gains_control <- summary_control_data_all %>% 
  group_by(site_sub, Descriptors,rep_block ) %>% 
  summarise(sum_yld_gains = sum(yield_gain, na.rm = TRUE),
            max_yr_post_amelioration = max(yr_post_amelioration  , na.rm = TRUE))
cum_yld_gains_control

cum_yld_gains_control <- ungroup(cum_yld_gains_control)






#####################################################################################
#####  soil modifications      #####
#####################################################################################
# summary_control_data_all <- summary_control_data_all %>%
#   distinct(site, Descriptors, .keep_all=TRUE) %>%
#   select(site,
#          Descriptors )
# summary_control_data_all <- summary_control_data_all %>% arrange(site,Descriptors )

### get the soil modification from the Descriptors

#1 soil modification with depth
cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(soil_modification_depth =  str_extract(cum_yld_gains_control$Descriptors, "[^_]+"))#keep everything before the first_

#how many modification_depth
soil_modification_depth <- cum_yld_gains_control %>% 
  distinct(soil_modification_depth) %>% 
  arrange(soil_modification_depth) %>% 
  filter(soil_modification_depth != "Unmodified")
  
count(soil_modification_depth) #20 excluding the unmodified


#1a working clms - soil modification without depth 1 only
cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(soil_modification_1 =  str_extract(cum_yld_gains_control$Descriptors, "[^.]+"))#keep everything before the first .

#the Unmodified seems to have a problem this fixes it :)
cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(soil_modification_1 = str_extract(cum_yld_gains_control$soil_modification_1, "[^_]+")) #keep everything after the first _

#1a working clms - 2 soil modification without depth 2
cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(soil_modification_2 = case_when(
    soil_modification_depth == "Rip.50Spade.30" ~        "Rip+Spade",
    soil_modification_depth == "Rip.60Spade.30" ~        "Rip+Spade",
    soil_modification_depth == "Rip.45IncRip+Spade.30" ~ "IncRip+Spade",
    soil_modification_depth == "Rip.60IncRip+Spade.30" ~ "IncRip+Spade",
    TRUE ~ "NA"
  ))

	
#2 all soil modification without depth (if 2 soil modification used keep this)	

cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(soil_modification = case_when(
    soil_modification_2 == "NA" ~ soil_modification_1,
    TRUE ~ soil_modification_2
  ))
	
	
### remove the working clms
cum_yld_gains_control <- cum_yld_gains_control %>% 
  select(-soil_modification_1,
         -soil_modification_2)


#how many modification without depth
soil_modification_depth <- cum_yld_gains_control %>% 
  distinct(soil_modification) %>% 
  arrange(soil_modification) %>% 
  filter(soil_modification != "Unmodified")

count(soil_modification_depth) #7 excluding the unmodified 



#####################################################################################
#####  soil amendments      #####
#####################################################################################
back_up <- cum_yld_gains_control
### get the soil Amendment from the Descriptors
# gets all the amendments
cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(amendment_all = sub("^[^_]*_", "", cum_yld_gains_control$Descriptors)) #keep everything after the first _
#pull out the first amendment listed but has rates
cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(amendment_1 = str_extract(cum_yld_gains_control$amendment_all, "[^.]+"))#keep everything before the first 
#removed the rates in the first amendment listed 
cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(amendment_1 = str_extract(cum_yld_gains_control$amendment_1, "[^@]+")) #keep everything before the first @


#pull out the second amendment listed but has rates (_/.)
cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(amendment_2a = sub("^[^.]*.", "", cum_yld_gains_control$amendment_all)) 
cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(amendment_2b = sub("^[^.]*.", "", cum_yld_gains_control$amendment_2a)) 
cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(amendment_2 = str_extract(cum_yld_gains_control$amendment_2b, "[^.]+"))

### not quite right remove some problems!
unique(cum_yld_gains_control$amendment_2)

cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(amendment_2 = case_when(
    amendment_2 ==  "surface_Yr18,19,20" ~ "",
    amendment_2 ==  "incorp_50" ~ "",
    amendment_2 ==  "band_30" ~ "",
    amendment_2 ==  "band_50" ~ "",
    amendment_2 ==  "surface" ~ "",
    TRUE ~ amendment_2
  ))
unique(cum_yld_gains_control$amendment_2)



#pull out the thrid amendment listed but has rates (_/.)
cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(amendment_3a = sub("^[^.]*.", "", cum_yld_gains_control$amendment_2b)) 
cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(amendment_3b = sub("^[^.]*.", "", cum_yld_gains_control$amendment_3a)) 
cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(amendment_3 = str_extract(cum_yld_gains_control$amendment_3b, "[^.]+"))





### bugger the timing year for amenedment 2 is stuffed up!
unique(cum_yld_gains_control$amendment_1)
unique(cum_yld_gains_control$amendment_2)
unique(cum_yld_gains_control$amendment_3)



## amendments 1, 2, 3 together.
str(cum_yld_gains_control)


cum_yld_gains_control$amendment_1 <-  
  stringr::str_replace_na(cum_yld_gains_control$amendment_1 , replacement='')

cum_yld_gains_control$amendment_2 <-  
  stringr::str_replace_na(cum_yld_gains_control$amendment_2 , replacement='')  

cum_yld_gains_control$amendment_3 <-  
  stringr::str_replace_na(cum_yld_gains_control$amendment_3 , replacement='')


cum_yld_gains_control <-  cum_yld_gains_control %>% 
  mutate(amendment_123 = paste0(amendment_1,
                                amendment_2,
                                amendment_3))


#tidy up working outs
cum_yld_gains_control <- cum_yld_gains_control %>%
  select(-amendment_2a, -amendment_2b, -amendment_3a, -amendment_3b)

unique(cum_yld_gains_control$amendment_123)
#how many amendment without depth
amendment_no_depth <- cum_yld_gains_control %>% 
  distinct(amendment_123) %>% 
  arrange(amendment_123) %>% 
  filter(amendment_123 != "none")

count(amendment_no_depth) #26 excluding the none 
