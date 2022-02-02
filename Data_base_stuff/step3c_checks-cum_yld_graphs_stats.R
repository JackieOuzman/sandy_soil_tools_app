#how to make cumulative yield plots and anlysis for each site

data_file <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/sites_merged.csv"

## download the data using the specified file path above

summary_data_all <- read_csv(data_file)
##### order the Descriptors
order <- c("Control" ,
           "Unmodified_SE14.band_8" ,
           "Unmodified_Bi-Agra.surface+band_8" ,
           "Unmodified_Lc.surface" ,
           "Unmodified_Cl.surface" ,
           "Unmodified_Cl.incorp_8" ,
           "Unmodified_Cl@3.incorp_8" ,#Unmodified_Cl@3.incorp_8
           "Unmodified_Fert.surface" ,
           "Unmodified_Fert.foliar" ,#Unmodified_Fert.foliar  
           "Unmodified_Fert.incorp_8" ,#Unmodified_Fert.incorp_8  
           "Unmodified_Fert.band_8" ,
           "Unmodified_Fert.band_30" ,
           "Unmodified_Clay.incorp_10" ,
           "Unmodified_Clay.incorp_8" ,
           "Unmodified_K_added.surface" ,
           "Unmodified_Fert.band_30.Clay.incorp_10" ,
           "Spade.30_Lc@1.incorp_30" ,
           "Spade.30_Lc@2.incorp_30" ,
           "Spade.30_Lc@4.incorp_30" ,
           "Spade.30_Lc@6.incorp_30" ,
           "Spade.30_Lc@8.incorp_30" ,
           "Spade.30_Lc@10.incorp_30" ,
           "Spade.30_Lc@15.incorp_30" ,
           "Spade.30_Lc@20.incorp_30" ,
           "Spade.30_Lc@1.incorp_30.K_added.surface" ,
           "Spade.30_Lc@2.incorp_30.K_added.surface" ,
           "Spade.30_Lc@4.incorp_30.K_added.surface" ,
           "Spade.30_Lc@6.incorp_30.K_added.surface", 
           "Spade.30_Lc@8.incorp_30.K_added.surface" ,
           "Spade.30_Lc@10.incorp_30.K_added.surface", 
           "Spade.30_Lc@15.incorp_30.K_added.surface", 
           "Spade.30_Lc@20.incorp_30.K_added.surface" ,
           "Spade.30_none" ,
           "Spade.30_Lc.incorp_30", 
           "Spade.30_Cl.incorp_30" ,
           "Spade.30_Com.incorp_30" ,
           "Spade.30_Cereal.incorp_30", 
           "Spade.30_K_added.surface" ,
           "Spade.30_Fert.incorp_30.K_added.incorp_30", 
           "Spade.30_Fert.incorp_30" ,
           "Spade.30_Fert.incorp_30.Clay.incorp_30", 
           "Spade.30_Clay.incorp_30" ,
           "Spade.30_Lc.incorp_30.Clay.incorp_30" ,
           "Spade.30_Lc.incorp_30.Fert.incorp_30" ,
           "Spade.30_Lc.incorp_30.Fert.incorp_30.Clay.incorp_30" ,
           "Spade.30_Vetch.incorp_30" ,
           "Spade.30_Vet_Cer_In.incorp_30" ,
           "Spade.30_Vet_Cer.incorp_30" ,
           "Rip.30_none" ,
           "Rip.30_Cl.surface" ,
           "Rip.30_Cl.band_30" ,
           "Rip.30_Fert.band_8" ,
           "Rip.30_Fert.band_30" ,
           "Rip.30_Fert.incorp_30" ,
           "Rip.30_Lc.incorp_30" ,
           "Rip.40_none" ,
           "Rip.41_none" ,
           "Rip.41_Lc.incorp_41" ,
           "Rip.41_Fert.incorp_41" ,
           "Rip.60_none" ,
           "Rip.60_Lc.incorp_60" ,
           "Rip.60_Fert.band_8" ,
           "Rip.60_Cl.surface" ,
           "Rip.60_Cl.band_60" ,
           "Rip.60_Fert.band_60" ,
           "Rip.30+60_none" ,
           "Rip.60Spade.30_none" ,
           "Rip.60Spade.30_Lc.band_30+60", 
           "Rip.30+60_Lc.band_30+60" ,
           "Rip.50_none" ,
           "Rip.50_Cl.surface" ,
           "Rip.50_Cl.incorp_50" ,
           "Rip.50_Cl.band_50" ,
           "Rip.50_Fert.surface" ,
           "Rip.50_Clay.incorp_50", 
           "Inc.50_none" ,
           "Inc.50_Cl.incorp_50" ,
           "Rip.50IncRip_none" ,
           "Delving.18_none" ,
           "Delving.18_SE14.band_8" ,
           "Sweep.30_none" ,#Sweep.30_none
           "Sweep.30_Cl.incorp_30" ,
           "Sweep.30_Cl@3.incorp_30",
           "Sweep.30_Cl@6.incorp_30",
           "Sweep.30_Cl@6.incorp_30.Yrs17,18,19",
           "Sweep.30_Cl@9.incorp_30",             
           
           "Sweep.30_Cl@3.incorp_30.Lime.incorp_8",
           "Sweep.30_Lime.incorp_30" ,
           "Sweep.30_Cl.incorp_30.Clay.incorp_8" )




summary_data_all$Descriptors <- factor(summary_data_all$Descriptors,
                                       levels = order)





str(summary_data_all)
unique(summary_data_all$year)
unique(summary_data_all$site)

site_name <- "Yenda"


yld_site <- summary_data_all %>%
  filter(site == site_name)

#subset the data
cumulative_yld <- yld_site %>% 
  select(Descriptors, year, rep_block,yield)

#keep the rep but not the year
cumulative_yld <- cumulative_yld %>% 
  group_by(Descriptors,rep_block ) %>% 
  summarise(sum_yld = sum(yield, na.rm = TRUE))
cumulative_yld


anova_cum_yld <- aov(sum_yld ~ Descriptors, data = cumulative_yld)
# Summary of the analysis
summary(anova_cum_yld)

tukey_cum_yld <- TukeyHSD(anova_cum_yld)
print(tukey_cum_yld)


tukey_cum_yld.cld <- multcompLetters4(anova_cum_yld, tukey_cum_yld) # default is threshold = 0.05
print(tukey_cum_yld.cld)


### this is to get the mean value of the cumlative yield
cumulative_yld_table <- cumulative_yld %>% 
  group_by(Descriptors) %>%
  summarise(mean_cum_yld=mean(sum_yld, na.rm = TRUE), 
            sd=sd(sum_yld, na.rm = TRUE),
            count = n(),
            std_error = sd/(sqrt(count))
  ) %>%
  arrange(desc(mean_cum_yld))
print(cumulative_yld_table)


#adding the compact letter display to the table with means and sd
cld_sum <- as.data.frame.list(tukey_cum_yld.cld$Descriptors)
cumulative_yld_table$Tukey <- cld_sum$Letters
print(cumulative_yld_table)

## add in some details 
cumulative_yld_table <- cumulative_yld_table %>% 
  mutate(site = site_name)

cumulative_yld_table <- cumulative_yld_table %>% 
  arrange(Descriptors)

cumulative_yld_table


###############################################################################################################
### Now for the plots ###

str(yld_site)
site_year_yld_summary <- yld_site %>% 
  group_by(Descriptors, year) %>%
  summarise(mean=mean(yield, na.rm = TRUE), 
            sd=sd(yield, na.rm = TRUE),
            count = n(),
            std_error = sd/(sqrt(count))
  ) %>%
  arrange(desc(mean))
print(site_year_yld_summary)


site_year_yld_summary$Descriptors <- factor(site_year_yld_summary$Descriptors,
                                   levels = order)

site_year_yld_summary$Year <- factor(site_year_yld_summary$year,
                                            levels = c("2021","2020","2019","2018","2017","2016","2015","2014"))

# barplot with letters
plot_cumulative_yld <- site_year_yld_summary %>% 
  ggplot( aes(x = factor(Descriptors), y = mean, fill = Year, colour = Year)) + 
  geom_bar(stat = "identity",  alpha = 0.5)  +
  labs(x="", y="Cumulative Yield (t/ha)", title = paste(site_name))+
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x=element_text(angle=45,hjust=1))
  

  plot_cumulative_yld
  
  output_folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/"
  
  
  
  ggsave(plot_cumulative_yld,
         device = "png",
         filename = paste0("Plot_cumulative_yield", site_name,"_",  ".png"),
         path= output_folder,
         width=8.62,
         height = 6.28,
         dpi=600
  )
  
  write.csv(cumulative_yld_table, paste0(output_folder,"Cumulative_Yield_",site_name,".csv"))
