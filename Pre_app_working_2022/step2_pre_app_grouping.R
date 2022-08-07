

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
          "extra_table_v2.csv",
          row.names = FALSE)





