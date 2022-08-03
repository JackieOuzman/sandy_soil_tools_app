

library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)

### step 1 geeting the DB files into a format that the app will like:)

# These are the output files I am wanting to create
#1.  \\FSSA2-ADL\clw-share1\mallee_mod\Therese_Jackie\Sandy_soils\App_development2021\sandy_soil_tools_app\sandy_soil_tools_app\site_location_plus_info_v2.csv
#2.  \\FSSA2-ADL\clw-share1\mallee_mod\Therese_Jackie\Sandy_soils\App_development2021\sandy_soil_tools_app\sandy_soil_tools_app\primary_data_all_v2.csv 


#1. is the metdata and I have done this manually for now - but I will need to change this to automatc and run off the DB files
#2. is the DB trial results but there are a few extra column that I may or may not need. I will try and create them here so the app will run.
# these are grouping, modification (control ripping etc), 	non_wetting,	acidic,	physical,	rainfall_mean_annual,	site_numb. 


#2
DB_df <- read.csv("X:/Therese_Jackie/Sandy_soils/Development_database/completeDB/sites_merged_all_messy.csv")
names(DB_df)
list_of_Descriptors <- distinct(DB_df, Descriptors)


#####################################################################################################################
###########                    Still having trouble with Ouyen placement and Younghusband         ###################
#####################################################################################################################

### get the soil modification from the Descriptors

#1 soil modification with depth
DB_df_soil_mod <- DB_df %>% 
  mutate(soil_modification_depth =  str_extract(DB_df$Descriptors, "[^_]+"))#keep everything before the first_


#1.1 Youndhusband has 'Unmodified+DeepTill.18' which needs to be recoded as "Unmodified"

DB_df_soil_mod <- DB_df_soil_mod %>% 
  mutate(soil_modification_depth = case_when(
    soil_modification_depth == "Unmodified+DeepTill.18" ~ "Unmodified",
    TRUE ~ soil_modification_depth))

#how many modification_depth
soil_modification_depth <- DB_df_soil_mod %>% 
  distinct(soil_modification_depth) %>% 
  arrange(soil_modification_depth) %>% 
  filter(soil_modification_depth != "Unmodified")

count(soil_modification_depth) #23 excluding the unmodified


#1a working clms - soil modification without depth 1 only
DB_df_soil_mod <- DB_df_soil_mod %>% 
  mutate(soil_modification_1 =  str_extract(DB_df_soil_mod$Descriptors, "[^.]+"))#keep everything before the first .

#the Unmodified seems to have a problem this fixes it :)
DB_df_soil_mod <- DB_df_soil_mod %>% 
  mutate(soil_modification_1 = str_extract(DB_df_soil_mod$soil_modification_1, "[^_]+")) #keep everything after the first _

#1a.1 Youndhusband has 'Unmodified+DeepTill.18' which needs to be recoded as "Unmodified"

DB_df_soil_mod <- DB_df_soil_mod %>% 
  mutate(soil_modification_1 = case_when(
    soil_modification_1 == "Unmodified+DeepTill" ~ "Unmodified",
    soil_modification_1 == "Control"  ~ "Unmodified",
    soil_modification_1 == "Pre"                 ~ "PreDrill",
    TRUE ~ soil_modification_1))

DB_df_soil_mod %>%  distinct(soil_modification_1) %>% arrange(soil_modification_1)





#1a working clms - 2 soil modification without depth 2

distinct(DB_df_soil_mod,soil_modification_depth)

DB_df_soil_mod <- DB_df_soil_mod %>% 
  mutate(soil_modification_2 = case_when(
    soil_modification_depth == " Rip.50Spade.30" ~        "Rip+Spade",
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


distinct(DB_df_soil_mod,soil_modification_2)



#2 all soil modification without depth (if 2 soil modification used keep this)	

DB_df_soil_mod <- DB_df_soil_mod %>% 
  mutate(soil_modification = case_when(
    soil_modification_2 == "NA" ~ soil_modification_1,
    TRUE ~ soil_modification_2
  ))

# names(DB_df_soil_mod)
# 
# what_check <- DB_df_soil_mod %>% 
#   select(ID, site, year, Descriptors,soil_modification_depth, soil_modification_1, soil_modification_2, soil_modification)
#   
  
  
### remove the working clms
DB_df_soil_mod <- DB_df_soil_mod %>% 
  dplyr::select(-soil_modification_1,
                -soil_modification_2)


#how many modification without depth
DB_df_soil_mod_depth <- DB_df_soil_mod %>% 
  distinct(soil_modification) %>% 
  arrange(soil_modification) %>% 
  filter(soil_modification != "Unmodified")

count(DB_df_soil_mod_depth) #9 excluding the unmodified 

DB_df_soil_mod %>%
  distinct(soil_modification) %>%
  arrange(soil_modification) 




#####################################################################################
#####  soil amendments      #####
#####################################################################################

### get the soil Amendment from the Descriptors
# gets all the amendments
DB_df_soil_mod <- DB_df_soil_mod %>% 
  mutate(amendment_all = sub("^[^_]*_", "", DB_df_soil_mod$Descriptors)) #keep everything after the first _
#pull out the first amendment listed but has rates
DB_df_soil_mod <- DB_df_soil_mod %>% 
  mutate(amendment_1 = str_extract(DB_df_soil_mod$amendment_all, "[^.]+"))#keep everything before the first 
#removed the rates in the first amendment listed 
DB_df_soil_mod <- DB_df_soil_mod %>% 
  mutate(amendment_1 = str_extract(DB_df_soil_mod$amendment_1, "[^@]+")) #keep everything before the first @

#temp <- summary_control_data_all %>%  distinct(amendment_1) %>% arrange(amendment_1)


#pull out the second amendment listed but has rates (_/.)
DB_df_soil_mod <- DB_df_soil_mod %>% 
  mutate(amendment_2a = sub("^[^.]*.", "", DB_df_soil_mod$amendment_all)) 
DB_df_soil_mod <- DB_df_soil_mod %>% 
  mutate(amendment_2b = sub("^[^.]*.", "", DB_df_soil_mod$amendment_2a)) 
DB_df_soil_mod <- DB_df_soil_mod %>% 
  mutate(amendment_2 = str_extract(DB_df_soil_mod$amendment_2b, "[^.]+"))

#temp <- summary_control_data_all %>%  distinct(amendment_2) %>% arrange(amendment_2)



### not quite right remove some problems!
unique(DB_df_soil_mod$amendment_2)

DB_df_soil_mod <- DB_df_soil_mod %>% 
  mutate(amendment_2 = case_when(
    amendment_2 ==  "surface_Yr18,19,20" ~ "",
    amendment_2 ==  "incorp_50" ~ "",
    amendment_2 ==  "band_30" ~ "",
    amendment_2 ==  "band_50" ~ "",
    amendment_2 ==  "surface" ~ "",
    
    TRUE ~ amendment_2
  ))
unique(DB_df_soil_mod$amendment_2)



#pull out the thrid amendment listed but has rates (_/.)
DB_df_soil_mod <- DB_df_soil_mod %>% 
  mutate(amendment_3a = sub("^[^.]*.", "", DB_df_soil_mod$amendment_2b)) 
DB_df_soil_mod <- DB_df_soil_mod %>% 
  mutate(amendment_3b = sub("^[^.]*.", "", DB_df_soil_mod$amendment_3a)) 
DB_df_soil_mod <- DB_df_soil_mod %>% 
  mutate(amendment_3 = str_extract(DB_df_soil_mod$amendment_3b, "[^.]+"))


temp <- DB_df_soil_mod %>%  distinct(amendment_3) %>% arrange(amendment_3)


### bugger the timing year for amenedment 2 is stuffed up!
unique(DB_df_soil_mod$amendment_1)
unique(DB_df_soil_mod$amendment_2)
unique(DB_df_soil_mod$amendment_3)



## amendments 1, 2, 3 together.
str(DB_df_soil_mod)


DB_df_soil_mod$amendment_1 <-  
  stringr::str_replace_na(DB_df_soil_mod$amendment_1 , replacement='')

DB_df_soil_mod$amendment_2 <-  
  stringr::str_replace_na(DB_df_soil_mod$amendment_2 , replacement='')  

DB_df_soil_mod$amendment_3 <-  
  stringr::str_replace_na(DB_df_soil_mod$amendment_3 , replacement='')


DB_df_soil_mod <-  DB_df_soil_mod %>% 
  mutate(amendment_123 = paste0(amendment_1,
                                amendment_2,
                                amendment_3))


#tidy up working outs
DB_df_soil_mod <- DB_df_soil_mod %>%
  dplyr::select(-amendment_2a, -amendment_2b, -amendment_3a, -amendment_3b)

unique(DB_df_soil_mod$amendment_123)
#how many amendment without depth
DB_df_soil_mod_no_depth <- DB_df_soil_mod %>%
  distinct(amendment_123) %>%
  arrange(amendment_123) %>%
  filter(amendment_123 != "none")



DB_df_soil_mod<- DB_df_soil_mod %>% 
  mutate(amendment_code1 = case_when(
    amendment_123 ==  "none" ~ "none",
    amendment_123 ==  "Control" ~ "none",
    
    amendment_123 == "drill_20+7"              ~ "none",
    amendment_123 == "30+30_none_annual"       ~ "none",
    amendment_123 == "drill_20+20_none"        ~ "none",
    amendment_123 == "drill_20+20_none_annual" ~ "none",
    amendment_123 == "30+7"                    ~ "none",
    amendment_123 == "30+30_none"              ~ "none",
    
    
    amendment_123 ==  "Cl" ~ "animal",
    
    amendment_123 ==  "Lc" ~ "plant",
    amendment_123 ==  "Cereal" ~ "plant",
    amendment_123 ==  "Vetch" ~ "plant",
    amendment_123 ==  "Vet_Cer" ~ "plant",
    amendment_123 ==  "Vet_Cer_In" ~ "plant", 
    amendment_123 ==  "Com" ~ "plant",
    
    amendment_123 ==  "Fert" ~        "fertiliser",
    amendment_123 ==  "Fert_High"   ~ "fertiliser",
    amendment_123 ==  "Fert_Low"    ~ "fertiliser",
    amendment_123 ==  "Fert_APP"    ~ "fertiliser",
    amendment_123 ==  "K_added"     ~ "fertiliser",
    amendment_123 ==  "FertK_added" ~ "fertiliser",
    amendment_123 ==  "30+30_Fert"  ~ "fertiliser",
    amendment_123 ==  "drill_20+20_Fert" ~ "fertiliser",
    
    amendment_123 ==  "Gypsum" ~ "non organic",
    amendment_123 ==  "Clay" ~ "non organic",
    amendment_123 ==  "Lime" ~ "non organic",
    amendment_123 ==  "Bi_Agra" ~ "non organic",
    amendment_123 ==  "SE14" ~ "non organic",
    
    
    amendment_123 ==  "ClFertClay" ~ "mixed",
    amendment_123 ==  "ClFert" ~ "mixed",
    amendment_123 ==  "ClClay" ~ "mixed",
    amendment_123 ==  "ClLime" ~ "mixed",
    amendment_123 ==  "ClGypsum" ~ "mixed",
    
    amendment_123 ==  "LcFert" ~ "mixed",
    amendment_123 ==  "LcClay" ~ "mixed",
    amendment_123 ==  "LcFertClay" ~ "mixed",
    amendment_123 ==  "LcK_added" ~ "mixed",
    
    amendment_123 ==  "FertClay" ~ "mixed",
    
    
    TRUE ~ "check"
    
    
  ))


#####################################################################################
#####  add 2 clms   modification and   grouping  #####
#####################################################################################



DB_df_soil_mod %>%
  distinct(amendment_code1) %>%
  arrange(amendment_code1) 

names(DB_df_soil_mod)

# just change name soil_modification
DB_df_soil_mod <- DB_df_soil_mod %>% 
  rename(modification = soil_modification) %>% 
  select(-soil_modification_depth)





#now I want to make clm grouping this is a bit different to what we had in the first app

DB_df_soil_mod <- DB_df_soil_mod %>% 
   mutate(grouping = paste0(modification , " ", amendment_code1)) 
 
DB_df_soil_mod<- DB_df_soil_mod %>% 
   mutate(grouping = case_when(
     Descriptors == "Control" ~ "Control",
     TRUE ~ grouping
   ))

#remove some working clms

DB_df_soil_modt <- DB_df_soil_mod %>%
  select(
    -amendment_all,-amendment_1,-amendment_2,-amendment_3,-amendment_123,
    -amendment_code1
  )

#####################################################################################
#####  add 4 clms    non_wetting,	acidic,	physical, nutrient #####
#####################################################################################

distinct(DB_df_soil_mod, Repellence)
names(DB_df_soil_mod)

DB_df_soil_mod<- DB_df_soil_mod %>% 
  mutate(non_wetting = case_when(
    Repellence == 0 ~ "green",
    Repellence == 1 ~ "orange",
    Repellence == 2 ~ "red",
    TRUE ~ "NA"
  ))
 
DB_df_soil_mod<- DB_df_soil_mod %>% 
  mutate(acidic = case_when(
    Acidity == 0 ~ "green",
    Acidity == 1 ~ "orange",
    Acidity == 2 ~ "red",
    TRUE ~ "NA"
  ))

DB_df_soil_mod<- DB_df_soil_mod %>% 
  mutate(physical = case_when(
    Physical == 0 ~ "green",
    Physical == 1 ~ "orange",
    Physical == 2 ~ "red",
    TRUE ~ "NA"
  ))
DB_df_soil_mod<- DB_df_soil_mod %>% 
  mutate(nutrient = case_when(
    Nutrient == 0 ~ "green",
    Nutrient == 1 ~ "orange",
    Nutrient == 2 ~ "red",
    TRUE ~ "NA"
  ))

names(DB_df_soil_mod)
what_check <- DB_df_soil_mod %>% 
   select(ID, site, year, Descriptors,Repellence, non_wetting,Acidity, acid, Physical, physical,Nutrient,  nutrient)


#####################################################################################
#####  add 2 clms    site_numb ,	rainfall_mean_annual	site_numb #####
#####################################################################################


DB_df_soil_mod <-  separate(DB_df_soil_mod, col=met_name_number, into=c('name-temp', 'site_numb'), sep='_', remove = FALSE)

annual_rain <- read.csv("X:/Therese_Jackie/Sandy_soils/Sands weather/met_file2022/annual_rain_2022_research_impact_sites.csv")

names(annual_rain)
annual_rain$site <- as.character(annual_rain$site)
DB_df_soil_mod$met_name_number <- as.character(DB_df_soil_mod$met_name_number)

annual_rain <- annual_rain %>% 
  rename(met_name_number = site,
         rainfall_mean_annual = annual_rain ) %>% 
  select(-X )






DB_df_soil_mod <- left_join(DB_df_soil_mod, annual_rain, by = "met_name_number" )



names(DB_df_soil_mod)
DB_df_soil_mod <- DB_df_soil_mod %>% 
  select(- 'name-temp')



#####################################################################################
##### I think this is good now :) 2. #####
#####################################################################################

write.csv(DB_df_soil_mod,
          "X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/sandy_soil_tools_app/primary_data_all_v2.csv", row.names = FALSE)




#####################################################################################
##### This is for the econmics. #####
#####################################################################################

extra_table_v2 <- 
  read.csv("X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/sandy_soil_tools_app/extra_table_v2.csv")
names(extra_table_v2)
#remove the clm that I need to fill
extra_table_v2<- extra_table_v2 %>% select(-non_wetting,-acidic,-physical,-nutrient,-rainfall_mean_annual,-site_numb )

df <- read.csv("X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/sandy_soil_tools_app/primary_data_all_v2.csv")

names(df)
#make the df small so I only have site non_wetting	acidic	physical	nutrient	rainfall_mean_annual	site_numb

df<- df %>% select(site, non_wetting,	acidic,	physical,	nutrient,	rainfall_mean_annual,	site_numb)
df <- df %>%  distinct(site, .keep_all = TRUE)


extra_table_v2 <- left_join(extra_table_v2, df, by = "site")

write.csv(extra_table_v2,
          "X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/sandy_soil_tools_app/extra_table_v2.csv", row.names = FALSE)








#####################################################################################
##### This is for the econmics I think  ?. making file called yield_table_av_v2 #####
#####################################################################################

# df <- read.csv("X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/sandy_soil_tools_app/primary_data_all_v2.csv")
# names(df)
# 
# #df <- df %>% select(ID, site, year, plot,rep_block, Descriptors, grouping, modification,yr_post_amelioration,  yield )
# 
# 
# 
# #############################################################################################
# ############                     control results only               ##########################
# #############################################################################################
# primary_control <- df %>% 
#   dplyr::select(ID:crop ,
#                 site_sub,
#                 plot,
#                 rep_block,
#                 rip,rip_depth,
#                 timing,
#                 Descriptors,
#                 establishment:dry_biomass,
#                 Latitude, Longitude,
#                 Amelioration_Year,yr_post_amelioration,
#                 sowing_date,harvest_date,previous_crop,
#                 decile,
#                 grouping, modification
#                 )
# #str(primary_control)
# unique(primary_control$Descriptors)
# 
# #we just want the control 
# 
# primary_control <- primary_control %>% 
#   dplyr::filter(Descriptors == "Control" )
# 
# 
# ## Looks like I want to match up the control with the site year and rep_block
# 
# 
# #a) make the dataset for control as small as possible and rename the yield biomass and established headings
# str(primary_control)
# 
# primary_control <- primary_control %>% 
#   dplyr::select(site, year, rep_block,site_sub,
#                 establishment, yield, dry_biomass)
# 
# primary_control <- primary_control %>% 
#   dplyr::rename(
#     control_establishment = establishment,
#     control_yield = yield ,
#     control_dry_biomass = dry_biomass) %>% 
#   dplyr::mutate(for_join = paste0(site_sub, "_", year,"_", rep_block))
# 
# primary_control <- primary_control %>% 
#   dplyr::select(for_join, control_establishment, control_yield, control_dry_biomass)
# 
# #b) make a join clm 
# 
# df_neat <- df %>% 
#   dplyr::mutate(for_join = paste0(site_sub, "_", year,"_", rep_block))
# 
# 
# ##############################################################################################################
# primary_with_control <- left_join(df_neat, primary_control)
# #check <- anti_join(primary_neat, primary_control)
# 
# #a) remove the controls in the descriptors
# unique(primary_with_control$Descriptors)
# primary_with_control <- primary_with_control %>% 
#   filter(Descriptors != "Control")
# 
# write.csv(primary_with_control,
#           "X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/sandy_soil_tools_app/TEST_yield_table_av_v2.csv", 
#           row.names = FALSE)



######################################################################################################
##### MAYBE TRY THIS _ This is for the econmics I think  ?. making file called yield_table_av_v2 #####
#####################################################################################################


yld_ave <- read.csv("X:/Therese_Jackie/Sandy_soils/Development_database/stats_batch_output/final_method/ANOVA_by_Yr_sites_merged_90percent.csv")
names(yld_ave)
yld_ave <- yld_ave %>% 
  select(site,
         year,
         Descriptors,
         #crop, need to get this
         #years post need to get this
         #grouping - need to get this
         #modification - need to get this
         mean #change name to yield (but I will need two clm one with yield (modfied), yield (un modified),
         #data source - dummy clm,
         #price - dummy clm
  )
yld_ave <- yld_ave %>%
  rename(yield = mean)


#get data from df
df <- read.csv("X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/sandy_soil_tools_app/primary_data_all_v2.csv")
names(df)

df_small <- df %>% select(site, year, Descriptors, grouping, modification, crop, yr_post_amelioration )
df_small <- df_small %>% 
  distinct(site, year, Descriptors, .keep_all = TRUE)


str(yld_ave)
str(df_small)

## make a clm to join
yld_ave <- yld_ave %>% 
  mutate(for_join = paste0(site, "_", year, "_", Descriptors))
df_small <- df_small %>% 
  mutate(for_join = paste0(site, "_", year, "_", Descriptors)) %>% 
  select(for_join, grouping, modification, crop, yr_post_amelioration)

yld_ave <- left_join(yld_ave, df_small)


### make a df that just contains control ylds and one that doesnt contain controls
yld_ave_control <- yld_ave %>% 
  filter(Descriptors == "Control")

yld_ave_non_control <- yld_ave %>% 
  filter(Descriptors != "Control")

names(yld_ave_control)
# rename the yld clm

yld_ave_control <-yld_ave_control %>% 
  rename("yield  (un modified)" = "yield") %>% 
  select(for_join,"yield  (un modified)" )

yld_ave_non_control <-yld_ave_non_control %>% 
  rename("yield (modified)" = "yield")
	
names(yld_ave_control)
names(yld_ave_non_control)
yld_ave_with_new_clm_for_yld <- left_join(yld_ave_non_control, yld_ave_control)

names(yld_ave_with_new_clm_for_yld)


### make some dummy clm
yld_ave_with_new_clm_for_yld <- yld_ave_with_new_clm_for_yld %>%
  mutate("data source" = "trial",
         price = 0)


write.csv(yld_ave_with_new_clm_for_yld,
          "X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/sandy_soil_tools_app/yield_table_av_v2.csv",
          row.names = FALSE)
