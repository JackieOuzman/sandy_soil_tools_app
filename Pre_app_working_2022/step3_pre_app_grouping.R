

library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)





#####################################################################################
##### This is for the yield. making file called yield_table_av_v2 #####
#####################################################################################


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

df_small <- df %>% select(site, year, Descriptors, grouping, modification, crop, yr_post_amelioration, decile, rainfall_mean_annual )
df_small <- df_small %>% 
  distinct(site, year, Descriptors, .keep_all = TRUE)


str(yld_ave)
str(df_small)

## make a clm to join
yld_ave <- yld_ave %>% 
  mutate(for_join = paste0(site, "_", year, "_", Descriptors)) %>% 
         mutate(for_join_control = paste0(site, "_", year))## make a new clm to join the control to non control file (it has site and year but no desciptors)

df_small <- df_small %>% 
  mutate(for_join = paste0(site, "_", year, "_", Descriptors)) %>% 
  select(for_join, grouping, modification, crop, yr_post_amelioration, decile, rainfall_mean_annual)

yld_ave <- left_join(yld_ave, df_small)
names(yld_ave)


### make a df that just contains control ylds and one that doesnt contain controls
yld_ave_control <- yld_ave %>% 
  filter(Descriptors == "Control")

yld_ave_non_control <- yld_ave %>% 
  filter(Descriptors != "Control")

names(yld_ave_control)
# rename the yld clm

yld_ave_control <-yld_ave_control %>% 
  rename("yield  (un modified)" = "yield") %>% 
  select(for_join_control,"yield  (un modified)" )

yld_ave_non_control <-yld_ave_non_control %>% 
  rename("yield (modified)" = "yield") %>% 
  select(- for_join )
	




names(yld_ave_control)
names(yld_ave_non_control)
yld_ave_with_new_clm_for_yld <- left_join(yld_ave_non_control, yld_ave_control)

names(yld_ave_with_new_clm_for_yld)


### make some dummy clm
yld_ave_with_new_clm_for_yld <- yld_ave_with_new_clm_for_yld %>%
  mutate("data source" = "trial",
         price = 0)




## -- Add in the new descriptors names -- ##


new_Descriptors_names <- read.csv("C:/Users/ouz001/working_from_home_post_Sep2022/sandy_soil_tools_app/list_of_Descriptors_with_new_Desciptors_name.csv")
#--- join the new descriptors to the DB_df --##

yld_ave_with_new_clm_for_yld <- left_join(yld_ave_with_new_clm_for_yld, new_Descriptors_names)


write.csv(yld_ave_with_new_clm_for_yld,
          "yield_table_av_v2.csv",
          row.names = FALSE)

