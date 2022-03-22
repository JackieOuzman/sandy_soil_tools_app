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
       x = "control yield t/ha", y = "trial yield t/ha")+
  facet_wrap(.~ soil_modification)



order_modification <- c("Rip",
                        "Rip+Spade", 
                        "Spade", 
                        "Sweep" , 
                        "Inc" ,
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
    x = "control yield t/ha", y = "trial yield t/ha")+
  facet_wrap(.~ soil_modification)
