

library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)


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

