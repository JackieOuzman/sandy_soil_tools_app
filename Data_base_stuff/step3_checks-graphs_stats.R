library(ggplot2)
library(readxl)
library(tidyverse)
#################################################################################################################
####   Get the data #####

current.folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2"

# find the files that you want
list.of.files <- list.files(current.folder, ".csv",full.names=T) #the trick is getting the full name - just the excel files
#list.of.files <- list.files(current.folder, full.names=T) #the trick is getting the full name - all the files
list.of.files

#####################################################################################################################
#### Get summary data 



# 3. Yenda
data_file <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2/Yenda_sites_step1_2_neat.csv"
control_data_file_metadata <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2/Yenda_sites_step1_2_control.csv"



##################################################################################################################
## download the data using the specified file path above

summary_data <- read_csv(data_file)

str(summary_data$Descriptors)
unique(summary_data$Descriptors)

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
  
############################################################################################################  
##### plots by year using the stats option in ggplot

ggplot(summary_data, aes(x=name, y=yield)) +
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap(.~ year)+
  labs(
    x = "",
    y = "Yield t/ha")+
   theme(axis.text.x=element_text(angle=45,hjust=1)) 

############################################################################################################  
##### Cal mean and error bars (SE) outside the orginal dataset.
str(summary_data)


### all the data
summary_data_v1 <- summary_data %>%
  group_by(name) %>%
  summarise(
    yield_mean = mean(yield, na.rm = TRUE),
    yield_SD = sd(yield,  na.rm = TRUE),
    yield_SE=yield_SD/sqrt(n()),
    
    est_mean = mean(establishment, na.rm = TRUE),
    est_SD = sd(establishment,  na.rm = TRUE),
    est_SE=est_SD/sqrt(n()),
    
    biomass_mean = mean(dry_biomass, na.rm = TRUE),
    biomass_SD = sd(dry_biomass,  na.rm = TRUE),
    biomass_SE=biomass_SD/sqrt(n()),
  )

summary_data_v1

### Yield plot
summary_data_v1 %>%
  ggplot(aes(x = name , y = yield_mean)) +
  geom_col()+
  theme_classic() +
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=yield_mean-yield_SE, ymax=yield_mean+yield_SE)) +
  labs(
    x = "",
    y = "Yield t/ha")+
  theme(axis.text.x=element_text(angle=45,hjust=1))



### by year the data
summary_data_v2 <- summary_data %>%
  group_by(name, year) %>%
  summarise(
    yield_mean = mean(yield, na.rm = TRUE),
    yield_SD = sd(yield,  na.rm = TRUE),
    yield_SE=yield_SD/sqrt(n()),
    
    est_mean = mean(establishment, na.rm = TRUE),
    est_SD = sd(establishment,  na.rm = TRUE),
    est_SE=est_SD/sqrt(n()),
    
    biomass_mean = mean(dry_biomass, na.rm = TRUE),
    biomass_SD = sd(dry_biomass,  na.rm = TRUE),
    biomass_SE=biomass_SD/sqrt(n()),
  )
summary_data_v2

summary_data_v2 %>%
  ggplot(aes(x = name , y = yield_mean)) +
  geom_col()+
  theme_classic() +
  facet_wrap(.~ year)+
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=yield_mean-yield_SE, ymax=yield_mean+yield_SE)) +
  labs(
    x = "",
    y = "Yield t/ha")+
  theme(axis.text.x=element_text(angle=45,hjust=1))


### Run some stats #too many comparisions

summary_data
# Compute the analysis of variance
res.aov <- aov(yield ~ Descriptors, data = summary_data)
# Summary of the analysis
summary(res.aov)

