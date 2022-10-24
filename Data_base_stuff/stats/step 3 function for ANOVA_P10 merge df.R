### This is to merge Younghusband (AKA trouble) analysis with the other sites.

library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)



df1_Younghusband <- read.csv("X:/Therese_Jackie/Sandy_soils/Development_database/stats_batch_output/final_method/ANOVA_by_Yr_sites_merged_90percent_Younghusband.csv")
df2_No_Younghusband <- read.csv("X:/Therese_Jackie/Sandy_soils/Development_database/stats_batch_output/final_method/ANOVA_by_Yr_sites_merged_90percent_no_younghusband.csv")

merge_ANOVA_sites_yr <- rbind(df1_Younghusband,df2_No_Younghusband)

## now if ANOVA is ns then change the group LSD to ns
names(merge_ANOVA_sites_yr)

merge_ANOVA_sites_yr <- merge_ANOVA_sites_yr %>% 
  mutate(
    groups_LSD = case_when(
       ANOVA_sign_0.1 == "ns"   ~ "ns",
       TRUE                      ~ "other"
    )
  )

      
      
write.csv(merge_ANOVA_sites_yr,"X:/Therese_Jackie/Sandy_soils/Development_database/stats_batch_output/final_method/ANOVA_by_Yr_sites_merged_90percent.csv" ,
          row.names = FALSE)
