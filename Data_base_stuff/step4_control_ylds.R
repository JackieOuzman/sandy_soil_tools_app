### How to present yield gains compared to the control.



library(ggplot2)
library(readxl)
library(tidyverse)
#library(multcompView)

library(plotly)

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
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Control yield - all data - no summary\nnote each site, treatment, year and rep is matched to control",
       x = "control yield t/ha", y = "trial yield t/ha")
scatter_control_vs_trial
ggplotly(scatter_control_vs_trial)

ggplot(data = summary_control_data_all, mapping = aes(control_yield, yield,)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Control yield - all data - no summary\nnote each site, treatment, year and rep is matched to control
       \nFacet wrap is years post amelioration, the NA related to trial with no metdata",
       x = "control yield t/ha", y = "trial yield t/ha")+
  facet_wrap(.~ yr_post_amelioration)




ggplot(data = summary_control_data_all, mapping = aes(control_yield, yield,)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
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
  labs(title = "Control yield - all data - no summary\nnote each site, treatment, year and rep is matched to control
       \nFacet wrap is modification",
       x = "control yield t/ha", y = "trial yield t/ha")+
  facet_wrap(.~ modification)



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




