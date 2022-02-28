
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

#### split the Descriptors into modification

summary_control_data_all <- summary_control_data_all %>%
  distinct(site, Descriptors, .keep_all=TRUE) %>%
  select(site,
         Descriptors )
summary_control_data_all <- summary_control_data_all %>% arrange(site,Descriptors )

### get the soil modification from the Descriptors
summary_control_data_all <- summary_control_data_all %>% 
  mutate(soil_modification_1 =  str_extract(summary_control_data_all$Descriptors, "[^.]+"))#keep everything before the first .
head(summary_control_data_all)
#the Unmodified seems to have a problem this fixes it :)
summary_control_data_all <- summary_control_data_all %>% 
  mutate(soil_modification_1a = str_extract(summary_control_data_all$soil_modification_1, "[^_]+")) #keep everything after the first _

#1, pull out everthing before first _

summary_control_data_all <- summary_control_data_all %>% 
  mutate(soil_modification_1b =  str_extract(summary_control_data_all$Descriptors, "[^_]+"))#keep everything before the first_

# summary_control_data_all <- summary_control_data_all %>% 
#   mutate(soil_modification_1text =  str_trunc(summary_control_data_all$soil_modification_1b, 
#                                               6,
#                                               side="left",
#                                               ellipsis = "")) #keep 6 strings from the end
# 
# summary_control_data_all <- summary_control_data_all %>% 
#   mutate(soil_modification_2text =  str_trunc(summary_control_data_all$soil_modification_1b, 
#                                               8,
#                                               side="left",
#                                               ellipsis = "")) #keep 6 strings from the end
# summary_control_data_all <- summary_control_data_all %>% 
#   mutate(soil_modification_3text =  str_trunc(summary_control_data_all$soil_modification_2text, 
#                                               5,
#                                               side="right",
#                                               ellipsis = "")) #keep 6 strings from the end  
#          
# head(summary_control_data_all)













### problem for later ...
summary_control_data_all <- summary_control_data_all %>% 
  mutate(soil_modification = case_when(
    Descriptors == "Rip.30IncRip_Gypsum.incorp_30" ~ "IncRip",
    Descriptors == "Rip.30IncRip_none" ~ "IncRip",
    Descriptors == "Rip.40IncRip_Lc.incorp_40" ~ "IncRip",
    Descriptors == "Rip.40IncRip_none" ~ "IncRip",
    Descriptors == "Rip.45IncRip_Fert_APP.band_45" ~ "IncRip",
    Descriptors == "Rip.45IncRip_Fert_High.band_45" ~ "IncRip",
    Descriptors == "Rip.45IncRip_Fert_Low.band_45" ~ "IncRip",
    Descriptors == "Rip.45IncRip_none" ~ "IncRip",
    Descriptors == "Rip.45IncRip_Fert.incorp_45" ~ "IncRip",
    #Descriptors == "Rip.45IncRip+Spade.30_none" ~ "IncRip",
    Descriptors == "Rip.50IncRip_Cl.surface" ~ "IncRip",
    Descriptors == "Rip.50IncRip_none" ~ "IncRip",
    Descriptors == "Rip.50IncRip_none" ~ "IncRip",
    Descriptors == "Rip.50IncRip_none" ~ "IncRip",
    Descriptors == "Rip.60IncRip_none" ~ "IncRip",
    
    Descriptors == "Rip.60Spade.30_none" ~ "Rip+Spade",
    Descriptors == "Rip.50Spade.30_none" ~ "Rip+Spade",
    Descriptors == "Rip.60Spade.30_none" ~ "Rip+Spade",
    Descriptors == "Rip.45IncRip+Spade.30_none" ~ "Rip+Spade",
    Descriptors == "Rip.60IncRip+Spade.30_none	" ~ "Rip+Spade",
    Descriptors == "Rip.60Spade.30_none" ~ "Rip+Spade",
    Descriptors == "Rip.60Spade.30_Lc.incorp_30+band_60	" ~ "Rip+Spade",
    
    Descriptors == "Rip.60IncRip+Spade.30_none" ~ "IncRip+Spade",
    Descriptors == "Rip.45IncRip+Spade.30_none" ~ "IncRip+Spade",
    TRUE ~ soil_modification))


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




#pull out the thrid amendment listed but has rates (_/.)
summary_control_data_all <- summary_control_data_all %>% 
  mutate(amendment_3a = sub("^[^.]*.", "", summary_control_data_all$amendment_2b)) 
summary_control_data_all <- summary_control_data_all %>% 
  mutate(amendment_3b = sub("^[^.]*.", "", summary_control_data_all$amendment_3a)) 
summary_control_data_all <- summary_control_data_all %>% 
  mutate(amendment_3 = str_extract(summary_control_data_all$amendment_3b, "[^.]+"))

#tidy up working outs
summary_control_data_all <- summary_control_data_all %>%
  select(-amendment_2a, -amendment_2b, -amendment_3a, -amendment_3b)



### bugger the timing year for amenedment 2 is stuffed up!
unique(summary_control_data_all$amendment_1)
unique(summary_control_data_all$amendment_2)
unique(summary_control_data_all$amendment_3)

summary_control_data_all <- summary_control_data_all %>% 
  mutate(amendment_2 = case_when(
    amendment_2 ==  "surface_Yr18,19,20" ~ "NA",
    amendment_2 ==  "incorp_50" ~ "NA",
    amendment_2 ==  "band_30" ~ "NA",
    amendment_2 ==  "band_50" ~ "NA",
    amendment_2 ==  "surface" ~ "NA",
    TRUE ~ amendment_2
  ))





