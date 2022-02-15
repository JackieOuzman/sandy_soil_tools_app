#how to make cumulative yield plots and anlysis for each site

data_file <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/sites_merged.csv"

## download the data using the specified file path above




summary_data_all <- read_csv(data_file, 
                             col_types = cols(rep_block = col_character()))


### For brooker remove low rep data
# brooker <- summary_data_all %>%
#   filter(site_sub == "Brooker") %>% 
#   filter(Descriptors %in% c(
#     "Control",
#     "Unmodified_K_added.surface",
#     "Spade.30_none",
#     "Spade.30_K_added.surface",
#     "Spade.30_Fert.incorp_30",
#     "Spade.30_Fert.incorp_30.K_added.incorp_30",
#     "Spade.30_Lc@4.incorp_30",
#     "Spade.30_Lc@4.incorp_30.K_added.surface",
#     "Spade.30_Lc@8.incorp_30",
#     "Spade.30_Lc@8.incorp_30.K_added.surface",
#     "Spade.30_Lc@15.incorp_30",
#     "Spade.30_Lc@15.incorp_30.K_added.surface"))

##### order the Descriptors see step 3b


str(summary_data_all)
unique(summary_data_all$year)
unique(summary_data_all$site)

#site_name <- "Yenda"
#site_name <- "Bute_Trengrove"
#site_name <- "Bute_CSIRO"
#site_name <-"Ouyen_spade"
#site_name <-"Lowaldie_Crest"      #"Lowaldie_Deep sand"
#site_name <-"Lowaldie_Deep sand"
#site_name <-"Brooker_reduced_trial"



yld_site <- summary_data_all %>%
  filter(site_sub == site_name)
#yld_site <- brooker 
  
str(yld_site)

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

p_value_cum <- summary(anova_cum_yld)[[1]][1,5]
F_value_cum <- summary(anova_cum_yld)[[1]][1,4]

### this is to get the mean value of the cumlative yield
cumulative_yld_table <- cumulative_yld %>% 
  group_by(Descriptors) %>%
  summarise(mean_cum_yld=mean(sum_yld, na.rm = TRUE), 
            sd=sd(sum_yld, na.rm = TRUE),
            count = n(),
            std_error = sd/(sqrt(count)),
            p_value  = p_value_cum,
            F_value  = F_value_cum
  ) %>%
  arrange(desc(mean_cum_yld))
print(cumulative_yld_table)


#adding the compact letter display to the table with means and sd
cld_sum <- as.data.frame.list(tukey_cum_yld.cld$Descriptors)
cumulative_yld_table$Tukey <- cld_sum$Letters
print(cumulative_yld_table)

## add in some details 
cumulative_yld_table <- cumulative_yld_table %>% 
  #mutate(site = site_name)
  mutate(site = site_name_output)

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
                                            #levels = c("2021","2020","2019","2018","2017","2016","2015","2014"))
                                            #levels = c("2021","2020","2019","2018"))
                                            # levels = c("2020","2019","2018", "2017"))
                                            # levels = c("2020","2019"))
                                             levels = c("2021","2020","2019"))
                                            #  levels = c("2020","2019","2018"))
                                            #levels = c("2018","2017","2016","2015","2014"))
                                            #levels = c("2018","2017","2015","2014"))
# barplot with letters
plot_cumulative_yld <- site_year_yld_summary %>% 
  ggplot( aes(x = factor(Descriptors), y = mean, fill = Year, colour = Year)) + 
  geom_bar(stat = "identity",  alpha = 0.5)  +
  #labs(x="", y="Cumulative Yield (t/ha)", title = paste(site_name))+
  labs(x="", y="Cumulative Yield (t/ha)", title = paste(site_name_output))+
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x=element_text(angle=45,hjust=1))
  

  plot_cumulative_yld
  
  output_folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/"
  
  
  
  ggsave(plot_cumulative_yld,
         device = "png",
         #filename = paste0("Plot_cumulative_yield", site_name,"_",  ".png"),
         filename = paste0("Plot_cumulative_yield", site_name_output,"_",  ".png"),
         path= output_folder,
         width=8.62,
         height = 6.28,
         dpi=600
  )
  
  #write.csv(cumulative_yld_table, paste0(output_folder,"Cumulative_Yield_",site_name,".csv"))
  write.csv(cumulative_yld_table, paste0(output_folder,"Cumulative_Yield_",site_name_output,".csv"))
