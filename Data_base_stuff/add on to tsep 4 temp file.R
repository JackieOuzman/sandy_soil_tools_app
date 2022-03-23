
#### slide 1
View(no_amendment)

no_amendment %>%  filter(soil_modification != "Unmodified") %>%
  ggplot(mapping = aes(control_yield, yield)) +
  geom_point(aes(colour= soil_modification),alpha= 0.4) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed", size = 0.5)+
  geom_smooth(data = no_amendment, method = lm, se = FALSE, colour = "black", size = 0.5) +
  labs(#title = "Control yield - subset of data no amendment",
       #subtitle = "note each site, treatment, year and rep is matched to control",
       x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ soil_modification)



#### slide 2
### same as above but with unmodified removed and title removed for TB 

names(no_amendment)
no_amendment %>%  filter(soil_modification != "Unmodified") %>%
  filter(year == 2021) %>%
  
  ggplot(mapping = aes(control_yield, yield)) +
  geom_point(aes(colour= soil_modification),alpha= 0.4) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed", size = 0.5)+
  geom_smooth(data = no_amendment %>%  filter(soil_modification != "Unmodified"),
              method = lm, se = FALSE, colour = "black", size = 0.5) +
  labs(#title = "Control yield - subset of data no amendment",
       #subtitle = "note each site, treatment, year and rep is matched to control",
       x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ soil_modification)



#### slide 3

order_modification <- c("Rip",
                        "Rip+Spade", 
                        "Spade", 
                        "Sweep" , 
                        #"Inc" ,
                        "IncRip" ,
                        "IncRip+Spade",
                        #"Delving",
                        "DiscInv",
                        "Unmodified" )
summary_control_data_all$soil_modification <- factor(summary_control_data_all$soil_modification,
                                         levels = order_modification)



summary_control_data_all %>%  
  filter(soil_modification != "Unmodified") %>% 
  filter(year == 2021) %>%
  
  ggplot( mapping = aes(control_yield, yield,)) +
  geom_point(aes(colour= soil_modification),alpha= 0.4) +
  geom_smooth(method = lm, se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  
  theme_bw()+
  labs(#title = "Control yield - all data - no summary\nnote each site, treatment, year and rep is matched to control
    #\nFacet wrap is modification",
    x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ soil_modification)



#### slide 4


ggplot(data = no_amendment, mapping = aes(control_yield, yield)) +
  geom_point(aes(colour= soil_modification),alpha= 0.4) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed", size = 0.5)+
  #geom_smooth(data = no_amendment, method = lm, se = FALSE, colour = "black", size = 0.5) +
  labs(title = "Control yield - subset of data no amendment",
       subtitle = "note each site, treatment, year and rep is matched to control",
       x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ as.factor(yr_post_amelioration))

no_amendment %>% 
  dplyr::filter(!is.na(yr_post_amelioration)) %>% 
  dplyr::filter( soil_modification != "Unmodified") %>% 
ggplot(mapping = aes(control_yield, yield)) +
  geom_point(aes(colour= soil_modification),alpha= 0.4) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed", size = 0.5)+
  #geom_smooth(data = no_amendment, method = lm, se = FALSE, colour = "black", size = 0.5) +
  labs(title = "Control yield - subset of data no amendment",
       subtitle = "note each site, treatment, year and rep is matched to control",
       x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ as.factor(yr_post_amelioration))



#### slide 5


mean_value <- mean(summary_control_data_all$yield_gain, na.rm = TRUE)
mean_value <- round(mean_value, 2)

ggplot(summary_control_data_all, aes(yield_gain)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 1, fill = "white") +
  geom_vline(data = summary_control_data_all, aes(xintercept=mean(yield_gain,  na.rm = TRUE)), color="blue", linetype="dashed", size=1)+
  geom_vline(data = summary_control_data_all, aes(xintercept=0), color="black", linetype="dashed", size=1)+
  labs(title = "Yield gains - all data - no summary\nNote each site, treatment, year and rep is matched to control\nBlue line is mean yield gain",
       subtitle = paste0("Mean yield gain ", mean_value, "t/ha"),
       x = "yield gain t/ha", y = "frequency of occurrence")



#### slide 6


mean_gains_values_no_amendment_Rip <- mean_gains_values_no_amendment %>% 
  dplyr::filter(soil_modification == "Rip")
mean_gains_values_no_amendment_Rip <- round(mean_gains_values_no_amendment_Rip[1,2],2)

mean_gains_values_no_amendment_spade <- mean_gains_values_no_amendment %>% 
  dplyr::filter(soil_modification == "Spade")
mean_gains_values_no_amendment_spade <- round(mean_gains_values_no_amendment_spade[1,2],2)


cum_yld_no_amendment %>% 
  filter(soil_modification =="Spade"| soil_modification == "Rip") %>% 
  ggplot( aes(x = sum_yld)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), group = 1, colour = 1, fill = "white") +
  
  geom_vline(data = mean_gains_values_no_amendment%>% 
               filter(soil_modification =="Spade"| soil_modification == "Rip"), 
             aes(xintercept=mod_mean_gains),       color="blue", linetype="dashed", size=1)+
  
  geom_vline(data = cum_yld_no_amendment %>% 
               filter(soil_modification =="Spade"| soil_modification == "Rip"), 
             aes(xintercept=0), color="black", linetype="dashed", size=1)+
  facet_wrap(.~ soil_modification)+
  labs(#title = "Cumulative control yield - subset of data no amendment",
       #subtitle ="note that for each site, treatment and rep yields are summed over multiple years\nand it is matched to summed control yield",
       # caption = paste0("Mean cumulative yield gain, Rip: ", 
       #                  mean_gains_values_no_amendment_Rip,
       #                  " Spade: ",mean_gains_values_no_amendment_spade),
       x = "cumulative yield gain t/ha", y = "frequency of occurance")



#### slide 7


summary_control_data_amendment_code1 <- summary_control_data_all %>%  
  filter(amendment_code1 != "none") %>%  
  filter(amendment_code1 != "non organic") %>% 
  filter(amendment_code1 != "mixed" ) 

#unique(summary_control_data_amendment_code1$amendment_code1)

summary_control_data_amendment_code1$amendment_code1 <-
  factor(summary_control_data_amendment_code1$amendment_code1,
         levels = c("animal", "plant", "fertiliser"))


summary_control_data_amendment_code1 %>%  
  filter(!is.na(yield)) %>% 
  filter(!is.na(control_yield)) %>% 
  
  ggplot( mapping = aes(control_yield, yield)) +
  geom_point(aes(colour= amendment_code1),alpha= 0.4) +
  geom_smooth(method = lm, se = FALSE, na.rm = TRUE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Control yield - all data - no summary\nnote each site, treatment, year and rep is matched to control
    \nFacet wrap is filtered data amendments",
       x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ amendment_code1)



#### slide 8


summary_control_data_amendment_code1 %>% 
  filter(soil_modification != "Unmodified") %>% 
  filter(!is.na(yield)) %>% 
  filter(!is.na(control_yield)) %>% 
  
  ggplot( mapping = aes(control_yield, yield,)) +
  geom_point(aes(colour= amendment_code1),alpha= 0.4) +
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Control yield - no summary. ANY soil modification \nnote each site, treatment, year and rep is matched to control
      \nFacet wrap is amendment",
       x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ amendment_code1)



#### slide 9

summary_control_data_amendment_code1 %>% 
  filter(!is.na(yield)) %>% 
  filter(!is.na(control_yield)) %>% 
  filter(soil_modification == "Unmodified") %>% 
  
  ggplot( mapping = aes(control_yield, yield,)) +
  geom_point(aes(colour= amendment_code1),alpha= 0.4) +
  geom_smooth(method = lm, se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Control yield - no summary. BUT no soil modification \nnote each site, treatment, year and rep is matched to control
      \nFacet wrap is amendment",
       x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ amendment_code1)



#### slide 10


summary_control_data_amendment_code1 %>% 
  filter(soil_modification == "Rip") %>% 
  filter(!is.na(yield)) %>% 
  filter(!is.na(control_yield)) %>% 
  
  ggplot( mapping = aes(control_yield, yield,)) +
  geom_point(aes(colour= amendment_code1),alpha= 0.4) +
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Control yield - no summary. Ripping only \nnote each site, treatment, year and rep is matched to control
      \nFacet wrap is amendment",
       x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ amendment_code1)



### Therese is worries about the cummulative yields
cum_yld_control

write.csv(cum_yld_control, "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/cumlative_yld_temp.csv",
          row.names = FALSE )
