


#ANOVA table


library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)

ANOVA_Cum_Yld_df <- read.csv("X:/Therese_Jackie/Sandy_soils/Development_database/stats_batch_output/final_method/Cum_ANOVA_sites_merged_90.csv")
ANOVA_Cum_Yld_df %>% 
  dplyr::distinct(site) #just amking sure we have Younghusband - we do:)

names(ANOVA_Cum_Yld_df)


ANOVA_Cum_Yld_df <- ANOVA_Cum_Yld_df %>% 
  dplyr::select(site,
                Descriptors,
                mean_cum_yld,
                sd,
                count,
                year,
                #std_error,
                p_value_ANOVA_cum,
                #F_value_ANOVA_cum,
                ANOVA_sign_0.1)

#make names user freindly
ANOVA_Cum_Yld_df <- ANOVA_Cum_Yld_df %>% 
  rename(Yield = mean_cum_yld,
         "p value" = p_value_ANOVA_cum,
         Significance = ANOVA_sign_0.1,
         "Standard error" = sd)
#split year into start and end year
ANOVA_Cum_Yld_df <- ANOVA_Cum_Yld_df %>% separate(year, c("start","end"), "to", remove = FALSE)

#format the P value
ANOVA_Cum_Yld_df$`p value` <- format(ANOVA_Cum_Yld_df$`p value`, digits = 4)

write.csv(ANOVA_Cum_Yld_df,
          "ANOVA_Cum_Yld_df_v2.csv",
          row.names = FALSE)

