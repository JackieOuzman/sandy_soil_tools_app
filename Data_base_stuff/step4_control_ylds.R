### How to present yield gains compared to the control.



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
#install.packages("hrbrthemes")

#################################################################################################################
####   Get the data #####



#####################################################################################################################

data_file <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/sites_merged.csv"
data_file_control <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/sites_control_merged.csv"

## download the data using the specified file path above

summary_data_all <- read_csv(data_file, 
                             col_types = cols(rep_block = col_character()))
summary_control_data_all <- read_csv(data_file_control, 
                            col_types = cols(rep_block = col_character()))


unique(summary_control_data_all$site_sub)
unique(summary_control_data_all$site)

str(summary_control_data_all)





### scatter plot control vs treatment yld with no summary of the data
# every trial yield point that has a matching control yld is displayed

scatter_control_vs_trial <- ggplot(data = summary_control_data_all, mapping = aes(control_yield, yield,)) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  geom_point(alpha= 0.4) +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Control yield - all data - no summary\nnote each site, treatment, year and rep is matched to control",
       x = "control yield t/ha", y = "trial yield t/ha")
scatter_control_vs_trial
ggplotly(scatter_control_vs_trial)

ggplot(data = summary_control_data_all, mapping = aes(control_yield, yield,)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  labs(title = "Control yield - all data - no summary\nnote each site, treatment, year and rep is matched to control
       \nFacet wrap is years post amelioration, the NA related to trial with no metdata",
       x = "control yield t/ha", y = "trial yield t/ha")+
  facet_wrap(.~ yr_post_amelioration)




ggplot(data = summary_control_data_all, mapping = aes(control_yield, yield,)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  labs(title = "Control yield - all data - no summary\nnote each site, treatment, year and rep is matched to control
       \nFacet wrap is sites",
       x = "control yield t/ha", y = "trial yield t/ha")+
  facet_wrap(.~ site_sub)

unique(summary_control_data_all$Descriptors)

#### split the Descriptors into modification

summary_control_data_all <- summary_control_data_all %>% 
  mutate(Descriptors_temp = Descriptors) %>% 
  separate(Descriptors_temp, c("modification_with_depth", "amendment"), sep = "_")

unique(summary_control_data_all$modification_with_depth)

summary_control_data_all <- summary_control_data_all %>% 
  mutate(modification = case_when(
    
    modification_with_depth ==  "Unmodified" ~ "Unmodified",
    modification_with_depth ==  "Spade.30" ~ "Spade",
    modification_with_depth ==  "Sweep.30" ~ "Sweep",
    modification_with_depth ==  "Delving.18" ~ "Delving",
    
    modification_with_depth ==  "Rip.30" ~ "Rip",
    modification_with_depth ==  "Rip.40" ~ "Rip",
    modification_with_depth ==  "Rip.41" ~ "Rip",
    modification_with_depth ==  "Rip.50" ~ "Rip",
    modification_with_depth ==  "Rip.50IncRip" ~ "Rip",
    modification_with_depth ==  "Rip.60" ~ "Rip",
    modification_with_depth ==  "Rip.30+60" ~ "Rip",
    modification_with_depth ==  "Rip.60Spade.30" ~ "Rip",
    
    modification_with_depth ==  "Inc.50" ~ "Inc",
    modification_with_depth ==  "Delving.18" ~ "Delving",
    TRUE ~ "check"))
    




ggplot(summary_control_data_all, mapping = aes(control_yield, yield,)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  labs(title = "Control yield - all data - no summary\nnote each site, treatment, year and rep is matched to control
       \nFacet wrap is modification",
       x = "control yield t/ha", y = "trial yield t/ha")+
  facet_wrap(.~ modification)

##############################################################################################################################
#### Yld gain #####

#### need to make a yield gain clm first
summary_control_data_all <- summary_control_data_all %>% 
  mutate(yield_gain = yield-control_yield, na.rm = TRUE)


ggplot(summary_control_data_all, aes(yield_gain)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 1, fill = "white") +
  geom_vline(data = summary_control_data_all, aes(xintercept=mean(yield_gain,  na.rm = TRUE)), color="blue", linetype="dashed", size=1)+
  geom_vline(data = summary_control_data_all, aes(xintercept=0), color="black", linetype="dashed", size=1)+
  labs(title = "Yield gains - all data - no summary\nNote each site, treatment, year and rep is matched to control\nBlue line is mean yield gain",
             x = "yield gain t/ha", y = "frequency of occurance")
  


## cal the mean yield gains for modification
mean_values <- summary_control_data_all %>% 
  group_by(modification) %>% 
  summarise(mod_mean = mean(yield_gain,  na.rm = TRUE))
  
  
# Histogram for each modification with mean yield gain plotted
ggplot(summary_control_data_all, aes(x = yield_gain)) + 
  # geom_histogram(aes(y = ..density..),colour = 1, fill = "white") +
  geom_histogram(aes(y = (..count..)/sum(..count..)), group = 1, colour = 1, fill = "white") +
  geom_vline(data = mean_values, aes(xintercept=mod_mean),       color="blue", linetype="dashed", size=1)+
  geom_vline(data = summary_control_data_all, aes(xintercept=0), color="black", linetype="dashed", size=1)+
  facet_wrap(.~ modification)+
  labs(title = "Yield gains - all data - no summary\nNote each site, treatment, year and rep is matched to control
         \nFacet wrap is modification\nBlue line is mean yield gain",
       x = "yield gain t/ha", y = "frequency of occurance")




#########################################################################################################################
###############             cummulative yields                                    #######################################
#########################################################################################################################

str(summary_control_data_all)

#keep the rep but not the year
cum_yld_control <- summary_control_data_all %>% 
  group_by(site_sub, Descriptors,rep_block ) %>% 
  summarise(sum_yld = sum(yield, na.rm = TRUE),
            sum__control_yld = sum(control_yield, na.rm = TRUE),
            max_yr_post_amelioration = max(yr_post_amelioration  , na.rm = TRUE))
cum_yld_control

cum_yld_control <- cum_yld_control %>% 
  mutate(`length of trial` = as.factor(max_yr_post_amelioration))

scatter_control_vs_trial_cum_yld <- 
  ggplot(data = cum_yld_control, mapping = aes(sum__control_yld, sum_yld, )) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Cumulative control yield - all data - no summary\nnote that for each site, treatment and rep all yield is summed over all years\nand it is matched to summed control
      \nBlack line is 1:1, blue is regression line",
       x = "cumulative control yield t/ha", y = "cumulative trial yield t/ha")+
  geom_abline(intercept = 0, slope = 1, linetype="dashed")

scatter_control_vs_trial_cum_yld
ggplotly(scatter_control_vs_trial_cum_yld)


scatter_control_vs_trial_cum_yld_trial_lenghth <- 
  ggplot(data = cum_yld_control, mapping = aes(sum__control_yld, sum_yld, colour = `length of trial`)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  labs(title = "Cumulative control yield - all data - no summary",
       subtitle ="note that for each site, treatment and rep yields are summed over multiple years\nand it is matched to summed control yield",
       x = "cumulative control yield t/ha", y = "cumulative trial yield t/ha")
scatter_control_vs_trial_cum_yld_trial_lenghth

#########################################################################################################################
###############             cummulative yields   with yld gains                   #######################################
#########################################################################################################################

str(summary_control_data_all)
#keep the rep but not the year
cum_yld_gains_control <- summary_control_data_all %>% 
  group_by(site_sub, Descriptors,rep_block ) %>% 
  summarise(sum_yld_gains = sum(yield_gain, na.rm = TRUE),
            max_yr_post_amelioration = max(yr_post_amelioration  , na.rm = TRUE))
cum_yld_gains_control




#### split the Descriptors into modification

cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(Descriptors_temp = Descriptors) %>% 
  separate(Descriptors_temp, c("modification_with_depth", "amendment"), sep = "_")

unique(cum_yld_gains_control$modification_with_depth)


cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(modification = case_when(
    
    modification_with_depth ==  "Unmodified" ~ "Unmodified",
    modification_with_depth ==  "Spade.30" ~ "Spade",
    modification_with_depth ==  "Sweep.30" ~ "Sweep",
    modification_with_depth ==  "Delving.18" ~ "Delving",
    
    modification_with_depth ==  "Rip.30" ~ "Rip",
    modification_with_depth ==  "Rip.40" ~ "Rip",
    modification_with_depth ==  "Rip.41" ~ "Rip",
    modification_with_depth ==  "Rip.50" ~ "Rip",
    modification_with_depth ==  "Rip.50IncRip" ~ "Rip",
    modification_with_depth ==  "Rip.60" ~ "Rip",
    modification_with_depth ==  "Rip.30+60" ~ "Rip",
    modification_with_depth ==  "Rip.60Spade.30" ~ "Rip",
    
    modification_with_depth ==  "Inc.50" ~ "Inc",
    modification_with_depth ==  "Delving.18" ~ "Delving",
    TRUE ~ "check"))



## cal the mean yield gains for modification
mean_gains_values <- cum_yld_gains_control %>% 
  group_by(modification) %>% 
  summarise(mod_mean_gains = mean(sum_yld_gains,  na.rm = TRUE))

cum_yld_gains_control <- cum_yld_gains_control %>% 
  mutate(`length of trial` = as.factor(max_yr_post_amelioration))

ggplot(cum_yld_gains_control, aes(sum_yld_gains)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 1, fill = "white") +
  geom_vline(data = cum_yld_gains_control, aes(xintercept=mean(sum_yld_gains,  na.rm = TRUE)), color="blue", linetype="dashed", size=1)+
  geom_vline(data = cum_yld_gains_control, aes(xintercept=0), color="black", linetype="dashed", size=1)+
  labs(title = "Cumulative control yield - all data - no summary",
       subtitle ="note that for each site, treatment and rep yields are summed over multiple years\nand it is matched to summed control yield",
       x = "cumulative yield gain t/ha", y = "frequency of occurance")


# Histogram for each modification with mean yield gain plotted
str(cum_yld_gains_control)

cum_yld_gains_control %>% 
  filter(modification =="Spade"| modification == "Rip") %>% 
 ggplot( aes(x = sum_yld_gains)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), group = 1, colour = 1, fill = "white") +
  geom_vline(data = mean_gains_values %>% 
               filter(modification =="Spade"| modification == "Rip"), aes(xintercept=mod_mean_gains),       color="blue", linetype="dashed", size=1)+
  geom_vline(data = cum_yld_gains_control%>% 
               filter(modification =="Spade"| modification == "Rip"), aes(xintercept=0), color="black", linetype="dashed", size=1)+
  facet_wrap(.~ modification)+
  labs(title = "Cumulative control yield",
       subtitle ="note that for each site, treatment and rep yields are summed over multiple years\nand it is matched to summed control yield",
       x = "cumulative yield gain t/ha", y = "frequency of occurance")









