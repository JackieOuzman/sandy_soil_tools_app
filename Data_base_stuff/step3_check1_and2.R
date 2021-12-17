

library(ggplot2)
library(readxl)
library(tidyverse)
#################################################################################################################
####   Get the data #####
Murrays_data_map <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Murrays_sites_step1_2.csv"

Murrays_data_v_control_map <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Murrays_sites_step1_2_control.csv"



Murrays_data <- read_csv(Murrays_data_map)

Murrays_data_v_control <- read_csv(Murrays_data_v_control_map)

#################################################################################################################
####   Ouyen_drill################################################################################################
#################################################################################################################

######## Yld bar plots ########

## make plots like Micheal paper for Ouyen.
str(Murrays_data)

Murrays_data_map_ouyen_drill <- Murrays_data %>% 
  filter(site_sub == "Ouyen_drill")

unique(Murrays_data$site_sub)
unique(Murrays_data_map_ouyen_drill$Descriptors)
str(Murrays_data_map_ouyen_drill)

p<-Murrays_data_map_ouyen_drill %>% 
mutate(Descriptors = factor(Descriptors, levels=c("Control", 
                                                  "Unmodified_Fert.7.5", 
                                                  "pre_drill.20_none", 
                                                  "pre_drill.20_Fert.20",
                                                  "Rip.30_none", 
                                                  "Rip.30_Fert.30"))) %>% 

  ggplot( aes(x=Descriptors , y=yield)) +
  geom_bar(stat = "summary", fun = "mean")+
  facet_wrap(.~year)+
  theme(axis.text.x = element_text(angle = 90))

p


### should we be adding the timing in the Descriptors

Murrays_data_map_ouyen_drill <- Murrays_data_map_ouyen_drill %>% 
  mutate(Descriptors_2 = paste0(Descriptors ,"_", timing))

unique(Murrays_data_map_ouyen_drill$Descriptors_2)

p2<-Murrays_data_map_ouyen_drill %>% 
  mutate(Descriptors_2 = factor(Descriptors_2, levels=c("Control_NA", 
                                                    
                                                    "Unmodified_Fert.7.5_NA",
                                                    
                                                    "pre_drill.20_none_NA",
                                                    "pre_drill.20_none_annual",
                                                    
                                                    "pre_drill.20_Fert.20_NA",
                                                    "pre_drill.20_Fert.20_annual",
                                                    
                                                    "Rip.30_none_NA",
                                                    "Rip.30_none_annual",
                                                    
                                                    "Rip.30_Fert.30_NA",
                                                    "Rip.30_Fert.30_annual"))) %>% 
  
  ggplot( aes(x=Descriptors_2 , y=yield)) +
  geom_bar(stat = "summary", fun = "mean")+
  facet_wrap(.~year)+
  theme(axis.text.x = element_text(angle = 90))

p2

######## Yld scatter plots vs control ########

Murrays_data_map_ouyen_drill_control <- Murrays_data_v_control %>% 
  filter(site_sub == "Ouyen_drill")
unique(Murrays_data_map_ouyen_drill_control$Descriptors)

str(Murrays_data_map_ouyen_drill_control)



scatter<-Murrays_data_map_ouyen_drill_control %>% 
  mutate(Descriptors = factor(Descriptors, levels=c("Control", 
                                                    "Unmodified_Fert.7.5", 
                                                    "pre_drill.20_none", 
                                                    "pre_drill.20_Fert.20",
                                                    "Rip.30_none", 
                                                    "Rip.30_Fert.30"))) %>% 
  
  ggplot( aes(x=control_yield , y=yield)) +
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, color="black")+
  stat_summary(fun=mean, geom="point", shape=18,
               size=3, color="red")+
  #geom_bar(stat = "summary", fun = "mean")+
  #facet_wrap(.~year)+
  theme(axis.text.x = element_text(angle = 90))
scatter




#################################################################################################################
####         Ouyen_spade             ###########################################################################
#################################################################################################################

######## Yld bar plots ########

## make plots like Micheal paper for Ouyen.
str(Murrays_data)

Murrays_data_map_ouyen_spade <- Murrays_data %>% 
  filter(site_sub == "Ouyen_spade")

unique(Murrays_data$site_sub)
unique(Murrays_data_map_ouyen_spade$Descriptors)
str(Murrays_data_map_ouyen_spade)

p_spade<-Murrays_data_map_ouyen_spade %>% 
  mutate(Descriptors = factor(Descriptors, levels=c("Control", 
                                                    "Spade.30_none", 
                                                    "Spade.30_cereal.30", 
                                                    "Spade.30_vetch.30",
                                                    "Spade.30_vet_cer.30", 
                                                    "Spade.30_vet_cer_in.30",
                                                    "Spade.30_Com.30",
                                                    "Spade.30_Cl.30",
                                                    "Spade.30_Fert.30"))) %>% 
  
  ggplot( aes(x=Descriptors , y=yield)) +
  geom_bar(stat = "summary", fun = "mean")+
  facet_wrap(.~year)+
  theme(axis.text.x = element_text(angle = 90))

p_spade

######## Yld scatter plots vs control ########

Murrays_data_map_ouyen_spade_control <- Murrays_data_v_control %>% 
  filter(site_sub == "Ouyen_spade")
unique(Murrays_data_map_ouyen_spade_control$Descriptors)

str(Murrays_data_map_ouyen_spade_control)

scatter_spade<-Murrays_data_map_ouyen_spade_control %>% 
  mutate(Descriptors = factor(Descriptors, levels=c("Control", 
                                                    "Spade.30_none", 
                                                    "Spade.30_cereal.30", 
                                                    "Spade.30_vetch.30",
                                                    "Spade.30_vet_cer.30", 
                                                    "Spade.30_vet_cer_in.30",
                                                    "Spade.30_Com.30",
                                                    "Spade.30_Cl.30",
                                                    "Spade.30_Fert.30"))) %>% 
  
  ggplot( aes(x=control_yield , y=yield)) +
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, color="black")+
  stat_summary(fun=mean, geom="point", shape=18,
               size=3, color="red")+
  theme(axis.text.x = element_text(angle = 90))
scatter_spade




#################################################################################################################
####   Lowaldie ################################################################################################
#################################################################################################################

######## Yld bar plots ########

## make plots like Micheal paper for Ouyen.
str(Murrays_data)
unique(Murrays_data$site)

Murrays_data_map_Lowaldie <- Murrays_data %>% 
  filter(site_sub == "Lowaldie")

unique(Murrays_data_map_Lowaldie$site_sub)
unique(Murrays_data_map_Lowaldie$Descriptors)


Lowaldie<-Murrays_data_map_Lowaldie %>% 
  mutate(Descriptors = factor(Descriptors, levels=c("Control", 
                                                    "Rip.40_none",
                                                    "Rip.60_none"))) %>% 
  
  ggplot( aes(x=Descriptors , y=yield)) +
  geom_bar(stat = "summary", fun = "mean")+
  facet_wrap(.~year)+
  theme(axis.text.x = element_text(angle = 90))

Lowaldie


######## Yld scatter plots vs control ########

Murrays_data_Lowaldie <- Murrays_data_v_control %>% 
  filter(site_sub == "Lowaldie")


scatter_Lowaldie<-Murrays_data_Lowaldie %>% 
  ggplot( aes(x=control_yield , y=yield)) +
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, color="black")+
  stat_summary(fun=mean, geom="point", shape=18,
               size=3, color="red")+
  theme(axis.text.x = element_text(angle = 90))
scatter_Lowaldie



#################################################################################################################
####   Waikerie ################################################################################################
#################################################################################################################

######## Yld bar plots ########


str(Murrays_data)
unique(Murrays_data$site)

Murrays_data_map_Waikerie <- Murrays_data %>% 
  filter(site_sub == "Waikerie")

unique(Murrays_data_map_Waikerie$site_sub)
unique(Murrays_data_map_Waikerie$Descriptors)


Waikerie<-Murrays_data_map_Waikerie %>% 
  mutate(Descriptors = factor(Descriptors, levels=c("Control",
                                                    "Unmodified_Cl.surface",
                                                    "Unmodified_Fert.surface",
                                                    
                                                    "Rip.30_none" ,
                                                    "Rip.30_Cl.surface",
                                                    "Rip.30_Fert.surface" ,
                                                    
                                                    "Rip.30_Cl.8" ,
                                                    "Rip.30_Fert.8",
                                                    
                                                    "Rip.60_none" ,
                                                    "Rip.60_Cl.surface" ,
                                                    "Rip.60_Cl.8"   ,
                                                    
                                                    "Rip.60_Fert.surface"  ,
                                                    "Rip.60_Fert.8"  
                                                    ))) %>% 
  
  ggplot( aes(x=Descriptors , y=yield)) +
  geom_bar(stat = "summary", fun = "mean")+
  facet_wrap(.~year)+
  theme(axis.text.x = element_text(angle = 90))

Waikerie


######## Yld scatter plots vs control ########

Murrays_data_Waikerie_control <- Murrays_data_v_control %>% 
  filter(site_sub == "Waikerie")


scatter_Waikerie<-Murrays_data_Waikerie_control %>% 
  ggplot( aes(x=control_yield , y=yield)) +
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, color="black")+
  stat_summary(fun=mean, geom="point", shape=18,
               size=3, color="red")+
  theme(axis.text.x = element_text(angle = 90))
scatter_Waikerie


#################################################################################################################
####   Carwarp ################################################################################################
#################################################################################################################

######## Yld bar plots ########


str(Murrays_data)
unique(Murrays_data$site)

Murrays_data_map_Carwarp <- Murrays_data %>% 
  filter(site_sub == "Carwarp")

unique(Murrays_data_map_Carwarp$site_sub)
unique(Murrays_data_map_Carwarp$Descriptors)


Carwarp<-Murrays_data_map_Carwarp %>% 
  mutate(Descriptors = factor(Descriptors, levels=c("Control",
                                                    "Unmodified_Lc.surface",
                                                    
                                                    
                                                    "Rip.30_none" ,
                                                    "Rip.30_Lc.30" ,
                                                    
                                                    "Rip.60_none" ,
                                                    "Rip.60_Lc.60",
                                                    
                                                    "Rip.60&30_none",
                                                    "Rip.60&30_Lc.60&30",
                                                    
                                                    "Spade.30_none"  ,
                                                    "Spade.30_Lc.surface" ,
                                                    
                                                    "Rip.60Spade.30_none",
                                                    "Rip.60Spade.30_Lc.60&30"
                                                    
  ))) %>% 
  
  ggplot( aes(x=Descriptors , y=yield)) +
  geom_bar(stat = "summary", fun = "mean")+
  facet_wrap(.~year)+
  theme(axis.text.x = element_text(angle = 90))

Carwarp


######## Yld scatter plots vs control ########

Murrays_data_Carwarp_control <- Murrays_data_v_control %>% 
  filter(site_sub == "Carwarp")


scatter_Carwarp<-Murrays_data_Carwarp_control %>% 
  ggplot( aes(x=control_yield , y=yield)) +
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, color="black")+
  stat_summary(fun=mean, geom="point", shape=18,
               size=3, color="red")+
  theme(axis.text.x = element_text(angle = 90))
scatter_Carwarp



######## Yld scatter plots vs control for all of Murray########




scatter_Murray<-Murrays_data_v_control %>% 
  ggplot( aes(x=control_yield , y=yield)) +
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, color="black")+
  stat_summary(fun=mean, geom="point", shape=18,
               size=3, color="red")+
  theme(axis.text.x = element_text(angle = 90))
scatter_Murray
