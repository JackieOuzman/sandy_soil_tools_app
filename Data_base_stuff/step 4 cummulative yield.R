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
#install.packages("hrbrthemes")

library(ggpubr)


data_file <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/sites_merged.csv"
data_file_control <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/sites_control_merged.csv"

## download the data using the specified file path above

summary_data_all <- read_csv(data_file, 
                             col_types = cols(rep_block = col_character()))
summary_control_data_all <- read_csv(data_file_control, 
                                     col_types = cols(rep_block = col_character()))



### make a dummy clm to filter out Younghusband 2021 data
summary_data_all <- summary_data_all %>%
  dplyr::mutate(site_yr_dummy = paste0(site, year))

## filter out the 2021 Younghusband data
summary_data_all <- summary_data_all %>%
  filter(site_yr_dummy  != "Younghusband2021") 



### make a dummy clm to filter out Younghusband 2021 data
summary_control_data_all <- summary_control_data_all %>%
  dplyr::mutate(site_yr_dummy = paste0(site, year))

## filter out the 2021 Younghusband data
summary_control_data_all <- summary_control_data_all %>%
  filter(site_yr_dummy  != "Younghusband2021") 



unique(summary_control_data_all$site_sub)
unique(summary_control_data_all$site)

str(summary_control_data_all)



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
                      "Yenda",
                      "Younghusband"
)




summary_control_data_all <- summary_control_data_all %>% 
  filter(site_sub %in% list_sites_ready  )
summary_data_all <- summary_data_all %>% 
  filter(site_sub %in% list_sites_ready  )


# Cumulative yields for yield gains  

str(summary_control_data_all)
#keep the rep but not the year

summary_control_data_all <- summary_control_data_all %>% 
  mutate(yield_gain = yield-control_yield, na.rm = TRUE)

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

#1.1 Youndhusband has 'Unmodified+DeepTill.18' which needs to be recoded as "Unmodified"

cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(soil_modification_depth = case_when(
    soil_modification_depth == "Unmodified+DeepTill.18" ~ "Unmodified",
    TRUE ~ soil_modification_depth))







#1a working clms - soil modification without depth 1 only
cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(soil_modification_1 =  str_extract(cum_yld_gains_control$Descriptors, "[^.]+"))#keep everything before the first .

#the Unmodified seems to have a problem this fixes it :)
cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(soil_modification_1 = str_extract(cum_yld_gains_control$soil_modification_1, "[^_]+")) #keep everything after the first _

#1a.1 Youndhusband has 'Unmodified+DeepTill.18' which needs to be recoded as "Unmodified"

cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(soil_modification_1 = case_when(
    soil_modification_1 == "Unmodified+DeepTill" ~ "Unmodified",
    TRUE ~ soil_modification_1))

cum_yld_gains_control %>%  distinct(soil_modification_1) %>% arrange(soil_modification_1)



#1a working clms - 2 soil modification without depth 2
cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(soil_modification_2 = case_when(
    soil_modification_depth == "Rip.50Spade.30" ~        "Rip+Spade",
    soil_modification_depth == "Rip.60Spade.30" ~        "Rip+Spade",
    soil_modification_depth == "Rip.45IncRip+Spade.30" ~ "IncRip+Spade",
    soil_modification_depth == "Rip.60IncRip+Spade.30" ~ "IncRip+Spade",
    
    soil_modification_depth == "Rip.30IncRip" ~           "IncRip",
    soil_modification_depth == "Rip.40IncRip" ~           "IncRip",
    soil_modification_depth == "Rip.45IncRip" ~           "IncRip",
    soil_modification_depth == "Rip.50IncRip" ~           "IncRip",
    soil_modification_depth == "Rip.60IncRip" ~           "IncRip",
    
    
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

############################################################################
### ALL THE DATA ####

## cal the mean yield gains for modification
mean_gains_values <- cum_yld_gains_control %>% 
  group_by(soil_modification) %>% 
  summarise(mod_mean_gains = mean(sum_yld_gains,  na.rm = TRUE))

cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(`length of trial` = as.factor(max_yr_post_amelioration))


#### summary stats and plots
## cal the mean yield gains for modification

names(cum_yld_gains_control)

mean_cum_yld_gains_control <- cum_yld_gains_control %>% 
  dplyr::group_by(soil_modification) %>% 
  dplyr::summarise(mean_gains_modification = mean(sum_yld_gains,  na.rm = TRUE))





mean_cum_yld_gains_control_Rip <- mean_cum_yld_gains_control %>% 
  dplyr::filter(soil_modification == "Rip")
mean_cum_yld_gains_control_Rip <- round(mean_cum_yld_gains_control_Rip[1,2],2)

mean_cum_yld_gains_control_spade <- mean_cum_yld_gains_control %>% 
  dplyr::filter(soil_modification == "Spade")
mean_cum_yld_gains_control_spade <- round(mean_cum_yld_gains_control_spade[1,2],2)

mean_cum_yld_gains_control_Rip
mean_cum_yld_gains_control_spade

max(cum_yld_gains_control$sum_yld_gains)

### Plots for all the cumlative yield data

cum_yld_gains_control %>% 
  filter(soil_modification =="Spade"| soil_modification == "Rip") %>% 
  ggplot( aes(x = sum_yld_gains)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), group = 1, colour = 1, fill = "white") +
  
  geom_vline(data = mean_cum_yld_gains_control%>% 
               filter(soil_modification =="Spade"| soil_modification == "Rip"), 
             aes(xintercept=mean_gains_modification),       color="blue", linetype="dashed", size=1)+
  
  geom_vline(data = mean_cum_yld_gains_control %>% 
               filter(soil_modification =="Spade"| soil_modification == "Rip"), 
             aes(xintercept=0), color="black", linetype="dashed", size=1)+
  
  facet_wrap(.~ soil_modification)+
  
  labs(title = "Cumulative control yield",
       subtitle ="note that for each site, treatment and rep yields are summed over multiple years\nand it is matched to summed control yield",
       caption = paste0("Mean cumulative yield gain, Rip: ", 
                        mean_cum_yld_gains_control_Rip,
                        " Spade: ",mean_cum_yld_gains_control_spade),
       x = "cumulative yield gain t/ha", y = "frequency of occurance")




############################################################################
### SUBSET OF DATA NO AMENDMENTS ####
names(cum_yld_gains_control)
cum_yld_gains_control_NO_Amendments <- cum_yld_gains_control %>% 
  filter(amendment_123 == "none")

mean_cum_yld_gains_control_NO_Amendenents <- cum_yld_gains_control_NO_Amendments %>% 
  dplyr::group_by(soil_modification) %>% 
  dplyr::summarise(mean_gains_modification = mean(sum_yld_gains,  na.rm = TRUE))


mean_cum_yld_gains_control_Rip_NO_Amendenents <- mean_cum_yld_gains_control_NO_Amendenents %>% 
  dplyr::filter(soil_modification == "Rip")
mean_cum_yld_gains_control_Rip_NO_Amendenents <- round(mean_cum_yld_gains_control_Rip_NO_Amendenents[1,2],2)

mean_cum_yld_gains_control_spade_NO_Amendenents <- mean_cum_yld_gains_control_NO_Amendenents %>% 
  dplyr::filter(soil_modification == "Spade")
mean_cum_yld_gains_control_spade_NO_Amendenents <- round(mean_cum_yld_gains_control_spade_NO_Amendenents[1,2],2)

mean_cum_yld_gains_control_Rip_NO_Amendenents
mean_cum_yld_gains_control_spade_NO_Amendenents


max(cum_yld_gains_control_NO_Amendments$sum_yld_gains)

cum_yld_gains_control_NO_Amendments %>% 
  filter(soil_modification =="Spade"| soil_modification == "Rip") %>% 
  ggplot( aes(x = sum_yld_gains)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), group = 1, colour = 1, fill = "white") +
  
  geom_vline(data = mean_cum_yld_gains_control_NO_Amendenents%>% 
               filter(soil_modification =="Spade"| soil_modification == "Rip"), 
             aes(xintercept=mean_gains_modification),       color="blue", linetype="dashed", size=1)+
  
  geom_vline(data = mean_cum_yld_gains_control_NO_Amendenents %>% 
               filter(soil_modification =="Spade"| soil_modification == "Rip"), 
             aes(xintercept=0), color="black", linetype="dashed", size=1)+
  
  facet_wrap(.~ soil_modification)+
  
  labs(#title = "Cumulative control yield with No Amendments",
       #subtitle ="note that for each site, treatment and rep yields are summed over multiple years\nand it is matched to summed control yield",
       # caption = paste0("Mean cumulative yield gain, Rip: ", 
       #                  mean_cum_yld_gains_control_Rip_NO_Amendenents,
       #                  " Spade: ",mean_cum_yld_gains_control_spade_NO_Amendenents),
       x = "cumulative yield gain t/ha", y = "frequency of occurance")
