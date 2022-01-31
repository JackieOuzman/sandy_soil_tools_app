#install.packages("multcompView")

library(ggplot2)
library(readxl)
library(tidyverse)
library(multcompView)


############################################################################################################
##### order the Descriptors

summary_data <-summary_data %>%
  mutate(name = factor(Descriptors, 
                       levels=c("Control", 
                                   "Unmodified_Fert.foliar" ,
                                   "Unmodified_Fert.incorp_8" ,
                                   "Unmodified_Cl.incorp_8" ,
                                   "Sweep.30_none" ,
                                   "Sweep.30_Lime.incorp_30" ,
                                   "Sweep.30_Cl.incorp_30",
                                   "Sweep.30_Cl.incorp_30.Clay.incorp_8" ,
                                  "Rip.60_none"
                                ))) 
  



#################################################################################################################
####   Get the data #####



#####################################################################################################################

data_file <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/sites_merged.csv"
##################################################################################################################
## download the data using the specified file path above

summary_data_all <- read_csv(data_file)

str(summary_data_all)
summary_data_all %>% distinct(site)



####################################################################################################################################
####################################################################################################################################
###https://statdoe.com/two-way-anova-in-r/
####################################################################################################################################
####################################################################################################################################

site_name <- "Yenda"
## for what years
summary_data_all %>% filter(site == site_name) %>% 
  distinct(year)

year_selected <- 2021

# Compute the analysis of variance
summary_data <- summary_data_all %>%
  filter(site == site_name) %>% 
  filter(year==year_selected)

anova <- aov(yield ~ Descriptors, data = summary_data)
# Summary of the analysis
summary(anova)

str(summary_data)
data_summary <- summary_data %>% 
  group_by(Descriptors, Descriptors_order) %>%
  summarise(mean=mean(yield), 
            sd=sd(yield)) %>%
  arrange(desc(mean))
print(data_summary)


tukey <- TukeyHSD(anova)
print(tukey)


tukey.cld <- multcompLetters4(anova, tukey) # default is threshold = 0.05
print(tukey.cld)


#adding the compact letter display to the table with means and sd
cld <- as.data.frame.list(tukey.cld$Descriptors)
data_summary$Tukey <- cld$Letters
print(data_summary)

## add in some details 
data_summary <- data_summary %>% 
  mutate(site = site_name,
         year = year_selected)

data_summary <- data_summary %>% 
  arrange(Descriptors_order)

data_summary


output_folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/"

write.csv(data_summary, paste0(output_folder,site_name,".csv"))

###############################################################################################################

#https://statdoe.com/barplot-for-two-factors-in-r/

output_folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/"
site_name <- "Yenda"
### Run some stats 
year_selected <- 2021


data_summary <- read_csv(paste0(output_folder,site_name,".csv"))
print(data_summary)



# barplot with letters
data_summary %>%  arrange(Descriptors_order) %>% 
ggplot( aes(x = factor(Descriptors), y = mean)) + 
  geom_bar(stat = "identity",  alpha = 0.5)  +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width = 0.25) +
  labs(x="", y="yield t/ha", title = paste(site_name," Year = ", year_selected)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = c(0.1, 0.75)) +
  geom_text(aes(label=Tukey), position = position_dodge(0.90), size = 3, 
            vjust=-0.8, hjust=-0.5, colour = "gray25")+
  theme(axis.text.x=element_text(angle=45,hjust=1))
