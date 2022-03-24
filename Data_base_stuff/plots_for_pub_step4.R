######################################################################################################################
#### slide 1
View(no_amendment)
names(no_amendment)

summary_control_data_all %>% 
  filter(amendment_all ==  "none"  ) %>% 
  filter(soil_modification != "Unmodified") %>% 
  
  
  ggplot(mapping = aes(control_yield, yield)) +
  geom_point(aes(colour= soil_modification),alpha= 0.4) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed", size = 0.5)+
  geom_smooth(data = summary_control_data_all %>% 
                filter(amendment_all ==  "none"  ) %>% 
                filter(soil_modification != "Unmodified"), 
              
              
              method = lm, se = FALSE, colour = "black", size = 0.5) +
  
  
  
  labs(title = "Control yield - subset of data no amendment",
       subtitle = "note each site, treatment, year and rep is matched to control",
       x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ soil_modification)#+
  #theme(plot.background = element_rect(fill = "#BFD5E3"))

#################
summary_control_data_all %>% 
  filter(amendment_all ==  "none"  ) %>% 
  filter(soil_modification != "Unmodified") %>% 
  dplyr::group_by(soil_modification) %>% 
  dplyr::summarise(mean_yld_gain = round(mean(yield_gain, na.rm = TRUE),2)) %>% 
  arrange(mean_yld_gain)



######################################################################################################################
#### slide 2
### same as above but with unmodified removed and title removed for TB 


  summary_control_data_all %>% 
  filter(amendment_all ==  "none"  ) %>% 
  filter(soil_modification != "Unmodified") %>% 
  filter(year == 2021) %>%
  
  ggplot(mapping = aes(control_yield, yield)) +
  geom_point(aes(colour= soil_modification),alpha= 0.4) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed", size = 0.5)+
  geom_smooth(data = summary_control_data_all %>% 
                filter(amendment_all ==  "none"  ) %>% 
                filter(soil_modification != "Unmodified"), 
              
              
              method = lm, se = FALSE, colour = "black", size = 0.5) +
  
  
  
  labs(#title = "Control yield - subset of data no amendment",
       #subtitle = "note each site, treatment, year and rep is matched to control",
       x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ soil_modification)#+
#theme(plot.background = element_rect(fill = "#BFD5E3"))

#################
summary_control_data_all %>% 
  filter(amendment_all ==  "none"  ) %>% 
  filter(soil_modification != "Unmodified") %>% 
  filter(year == 2021) %>%
  dplyr::group_by(soil_modification) %>% 
  dplyr::summarise(mean_yld_gain = round(mean(yield_gain, na.rm = TRUE),2)) %>% 
  arrange(mean_yld_gain)
  



######################################################################################################################

#### slide 3

summary_control_data_all %>% 
  filter(soil_modification != "Unmodified") %>% 
  filter(year == 2021) %>%
  
  ggplot(mapping = aes(control_yield, yield)) +
  geom_point(aes(colour= soil_modification),alpha= 0.4) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed", size = 0.5)+
  geom_smooth(data = summary_control_data_all %>% 
                filter(amendment_all ==  "none"  ) %>% 
                filter(soil_modification != "Unmodified"), 
              
              
              method = lm, se = FALSE, colour = "black", size = 0.5) +
  
  
  
  labs(    x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ soil_modification)#+
#theme(plot.background = element_rect(fill = "#BFD5E3"))

#################
summary_control_data_all %>% 
  filter(soil_modification != "Unmodified") %>% 
  filter(year == 2021) %>%
  dplyr::group_by(soil_modification) %>% 
  dplyr::summarise(mean_yld_gain = round(mean(yield_gain, na.rm = TRUE),2)) %>% 
  arrange(mean_yld_gain)




######################################################################################################################
#### slide 4


summary_control_data_all %>% 
  filter(amendment_all ==  "none"  ) %>% 
  filter(soil_modification != "Unmodified") %>% 
  dplyr::filter(!is.na(yr_post_amelioration)) %>% 
  
  ggplot(mapping = aes(control_yield, yield)) +
  geom_point(aes(colour= soil_modification),alpha= 0.4) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed", size = 0.5)+
  #geom_smooth(data = no_amendment, method = lm, se = FALSE, colour = "black", size = 0.5) +
  labs(title = "Control yield - subset of data no amendment",
       subtitle = "note each site, treatment, year and rep is matched to control",
       x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ as.factor(yr_post_amelioration))#+
  #theme(plot.background = element_rect(fill = "#BFD5E3"))




######################################################################################################################
#### slide 5


mean_yield_gain <- mean(summary_control_data_all$yield_gain, na.rm = TRUE)
mean_yield_gain <- round(mean_yield_gain, 2)

ggplot(summary_control_data_all, aes(yield_gain)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 1, fill = "white") +
  geom_vline(data = summary_control_data_all, aes(xintercept=mean(yield_gain,  na.rm = TRUE)), color="blue", linetype="dashed", size=1)+
  geom_vline(data = summary_control_data_all, aes(xintercept=0), color="black", linetype="dashed", size=1)+
  labs(title = "Yield gains - all data - no summary\nNote each site, treatment, year and rep is matched to control\nBlue line is mean yield gain",
       subtitle = paste0("Mean yield gain ", mean_yield_gain, "t/ha"),
       x = "yield gain t/ha", y = "frequency of occurrence")


summary_control_data_all %>% 
  dplyr::group_by(soil_modification) %>% 
  dplyr::summarise(mean_yld_gain_mod = round(mean(yield_gain, na.rm = TRUE), 2))%>% 
  arrange(mean_yld_gain_mod)


######################################################################################################################
#### slide 6


mean_cum_yield_gain_soil_mod <- cum_yld_control %>% 
  dplyr::group_by(soil_modification) %>% 
  dplyr::summarise(mean_cum_yield_gain_soil_mod = round(mean(sum__yld_gain, na.rm = TRUE),2))

mean_cum_yield_gain_soil_mod_rip <- mean_cum_yield_gain_soil_mod %>% 
  filter(soil_modification == "Rip")

mean_cum_yield_gain_soil_mod_spade <- mean_cum_yield_gain_soil_mod %>% 
  filter(soil_modification == "Spade")




cum_yld_control %>% 
  filter(soil_modification =="Spade"| soil_modification == "Rip") %>% 
  
  
  ggplot( aes(x = sum__yld_gain)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), group = 1, colour = 1, fill = "white") +
  
  geom_vline(data = mean_cum_yield_gain_soil_mod %>% 
               filter(soil_modification =="Spade"| soil_modification == "Rip"), 
             aes(xintercept=mean_cum_yield_gain_soil_mod),       
             color="blue", linetype="dashed", size=1)+
  
  geom_vline(data = cum_yld_control%>% 
               filter(soil_modification =="Spade"| soil_modification == "Rip"), 
             aes(xintercept=0), color="black", linetype="dashed", size=1)+
  
  facet_wrap(.~ soil_modification)+
  
  labs(title = "Cumulative control yield",
       subtitle ="note that for each site, treatment and rep yields are summed over multiple years\nand it is matched to summed control yield",
       caption = paste0("Mean cumulative yield gain for ripping = ", mean_cum_yield_gain_soil_mod_rip[1,2], " t/ha",
                        " AND ",
                        "spading = ", mean_cum_yield_gain_soil_mod_spade[1,2], " t/ha"
       ),
       x = "cumulative yield gain t/ha", y = "frequency of occurrence")#+
  #theme( plot.background = element_rect(colour = "dark blue", fill=NA, size=5))

mean_cum_yield_gain_soil_mod%>% 
  arrange(mean_cum_yield_gain_soil_mod)

######################################################################################################################
#### slide 7


summary_control_data_all$amendment_code1 <-
  factor(summary_control_data_all$amendment_code1,
         levels = c("animal", "plant", "fertiliser"))


summary_control_data_all %>%  
  filter(amendment_code1 != "none") %>%  
  filter(amendment_code1 != "non organic") %>% 
  filter(amendment_code1 != "mixed" ) %>% 
  
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



######################################################################################################################
#### slide 8


summary_control_data_all %>%  
  filter(amendment_code1 != "none") %>%  
  filter(amendment_code1 != "non organic") %>% 
  filter(amendment_code1 != "mixed" ) %>% 
  
  filter(!is.na(yield)) %>% 
  filter(!is.na(control_yield)) %>% 
  
  filter(soil_modification != "Unmodified") %>% 
  
  
  
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




######################################################################################################################
#### slide 9

summary_control_data_all %>%  
  filter(amendment_code1 != "none") %>%  
  filter(amendment_code1 != "non organic") %>% 
  filter(amendment_code1 != "mixed" ) %>% 
  
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


##############################################################################################################################
#### slide 10
 


  summary_control_data_all %>%  
  filter(amendment_code1 != "none") %>%  
  filter(amendment_code1 != "non organic") %>% 
  filter(amendment_code1 != "mixed" ) %>% 
  
  filter(!is.na(yield)) %>% 
  filter(!is.na(control_yield)) %>% 
  
  
  filter(soil_modification == "Rip") %>% 
  
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
  labs(title = "Control yield - no summary. Ripping only \nnote each site, treatment, year and rep is matched to control
      \nFacet wrap is amendment",
       x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ amendment_code1)








