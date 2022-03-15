
#Graphing the yld results for site and sum of all years


library(ggplot2)
library(readxl)
library(tidyverse)
library(multcompView)
library(scales)

#site_yrs_list <- "Brimpton LakeX2014to2018"

### List of sites I want to run analysis for:
site_yrs_list <- c("Carwarp_AmeliorationX2018to2020",
                    "Brimpton LakeX2014to2018")



#######################################################################################################################################################
##########                                                    As a loop                                                                       ##########
#######################################################################################################################################################

for (site_yrs_list in site_yrs_list){
  
  
#################################################################################################################
  
  
data_file_Cum_ANOVA <- "X:/Therese_Jackie/Sandy_soils/Development_database/stats_batch_output/Cum_ANOVA_sites_merged.csv"
data_file <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/sites_merged.csv"
  
## download the data using the specified file path above
  
Cum_ANOVA_results <- read_csv(data_file_Cum_ANOVA)
summary_data_all <- read_csv(data_file, 
                             col_types = cols(rep_block = col_character()))

##########################################
site_and_yr <- as.data.frame(str_split(site_yrs_list, "X"),
                             col.names = "site_and_yr" )

site_and_yr$site_and_yr <- as.character(site_and_yr$site_and_yr)
site_and_yr
a <- site_and_yr[1,1]
b <- site_and_yr[2,1]
b

######################################
######################################

##### order the Descriptors
order <- c(
  
  
)

###############################################################################################################################################
################                 ANOVA DATA                    ################
###############################################################################################################################################

Cum_ANOVA_results$Descriptors <- factor(Cum_ANOVA_results$Descriptors,
                                    levels = order)


#filter the ANOVA data on site first
Cum_ANOVA_results_site <- Cum_ANOVA_results %>%
  filter(site == a) 


#Max value for the plots
max_sum_cum <- max(Cum_ANOVA_results_site$mean_cum_yld, na.rm = TRUE) 
max_sum_cum <- max_sum_cum +0.5
max_sum_cum <- ceiling(max_sum_cum)

Cum_ANOVA_results_site$LSD_cum <- as.double(Cum_ANOVA_results_site$LSD_cum)
LSD_cum <- max(Cum_ANOVA_results_site$LSD_cum)
LSD_cum <- signif(LSD_cum, digits = 4)
LSD_cum





###############################################################################################################################################
################                 TRIAL DATA                    ################
###############################################################################################################################################

#filter the TRIAL data on site 
site_year_yld_summary_site <- summary_data_all %>%
  filter(site == a) 

site_year_yld_summary <- site_year_yld_summary_site %>% 
  dplyr::group_by(Descriptors, year) %>%
  dplyr::summarise(mean=mean(yield, na.rm = TRUE), 
            sd=sd(yield, na.rm = TRUE),
            count = n(),
            std_error = sd/(sqrt(count))
  ) %>%
  arrange(desc(mean))

site_year_yld_summary$Descriptors <- factor(site_year_yld_summary$Descriptors,
                                            levels = order)

###############################################################################################################################################


#assign the years in the correct order


order_yrs <- c(
  "2014",
  "2015",
  "2016",
  "2017",
  "2018",
  "2019",
  "2020",
  "2021")

site_year_yld_summary$year <- factor(site_year_yld_summary$year,
                                        levels = order_yrs)

  
#I need to add the max value of the Year this is specified in the list that goes into loop
max_yr <- as.data.frame(str_split(b, "to"), col.names = "Yr" )
max_yr$Yr <- as.character(max_yr$Yr)
max_yr$Yr <- as.double(max_yr$Yr)
max_yr <- max(max_yr$Yr)

Cum_ANOVA_results_site <- Cum_ANOVA_results_site %>% 
  dplyr::mutate(year = as.factor(max_yr))
print(Cum_ANOVA_results_site$year)






CumPlot_LSD <- site_year_yld_summary %>% 
  ggplot( aes(x = factor(Descriptors), y = mean, fill = year, colour = year)) + 
  geom_bar(stat = "identity",  alpha = 0.5)  +
  labs(x="", 
       y="Cumulative Yield (t/ha)", 
       title = paste0(a),
       subtitle = paste0("ANOVA. LSD = " ,LSD_cum))+
  theme_bw() + 
  scale_y_continuous(breaks=seq(0,max_sum_cum,by=1.0), limits = c(0, max_sum_cum))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  geom_text(data = Cum_ANOVA_results_site,
            aes(x = factor(Descriptors), y = mean_cum_yld,label=groups_LSD_cum), 
            position = position_dodge(0.80), 
            size = 3,
            vjust=-0.5, hjust=0.1, 
            colour = "gray25")


### save the plot
ggsave(CumPlot_LSD,
       device = "png",
       filename = paste0("Plot_yield_", 
                         a,"_", b, "_Cum_ANOVA_Plot_LSD", ".png"),
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/stats_batch_output/Yield_Cumulative_LSD_Plots/",
       width=8.62,
       height = 6.28,
       dpi=600
)

#names(Cum_ANOVA_results_site)
CumPlot_Dun <- site_year_yld_summary %>% 
  ggplot( aes(x = factor(Descriptors), y = mean, fill = year, colour = year)) + 
  geom_bar(stat = "identity",  alpha = 0.5)  +
  labs(x="", 
       y="Cumulative Yield (t/ha)", 
       title = paste0(a),
       subtitle = paste0("ANOVA. Dunnett's test" ))+
  theme_bw() + 
  scale_y_continuous(breaks=seq(0,max_sum_cum,by=1.0), limits = c(0, max_sum_cum))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  geom_text(data = Cum_ANOVA_results_site,
            aes(x = factor(Descriptors), y = mean_cum_yld,label=significance_control), 
            position = position_dodge(0.80), 
            size = 3,
            vjust=-0.5, hjust=0.1, 
            colour = "gray25")


### save the plot
ggsave(CumPlot_Dun,
       device = "png",
       filename = paste0("Plot_yield_", 
                         a,"_", b, "_Cum_ANOVA_Plot_Dun", ".png"),
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/stats_batch_output/Yield_Cumulative_Dunnetts_Plots/",
       width=8.62,
       height = 6.28,
       dpi=600
)


rm(a,
   b,
   data_file,
   data_file_Cum_ANOVA,
   LSD_cum,
   max_sum_cum,
   max_yr,
   order,
   order_yrs,
   
   Cum_ANOVA_results,
   Cum_ANOVA_results_site,
   CumPlot_LSD,
   CumPlot_Dun,
   site_and_yr,
   site_year_yld_summary,
   site_year_yld_summary_site,
   summary_data_all)


}








