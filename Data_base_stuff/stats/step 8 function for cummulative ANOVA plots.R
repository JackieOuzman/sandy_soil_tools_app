
#Graphing the yld results for site and sum of all years


library(ggplot2)
library(readxl)
library(tidyverse)
library(multcompView)
library(scales)

site_yrs_list = "Brimpton LakeX2014to2018"

### List of sites I want to run analysis for:
site_yrs_list <- c("Brimpton LakeX2014to2018",
                   "BrookerX2019to2021",
                   "BucklebooX2019to2021",
                   "Bute_CSIROX2018to2021",
                   "Bute_TrengroveX2015to2021",
                   "CadgeeX2014to2018",
                   #"CadgeeX2016", #no yield data
                   "Carwarp_AmeliorationX2018to2020",
                   "CumminsX2019to2021",
                   "KarkooX2019to2021",
                   "KaroondaX2014to2018",
                   "Kooloonong_canolaX2021",
                   "Kooloonong_chickpeaX2019to2021",
                   "Kooloonong_lentilX2019to2021",
                   "Kooloonong_lupinX2019to2021",
                   "KybungaX2019to2021",
                   "Lowaldie_CrestX2019to2021",
                    
                   "Lowaldie_Deep sandX2019to2021",
                   
                   "MalinongX2019to2021",
                   "Monia_GapX2019to2021",
                   "Mt DamperX2019to2021",
                   
                   "MurlongX2018to2021",
                   "Ouyen_SpadeX2017to2020",
                   "Ouyen_PlacementX2017to2020",
                   
                   "SherwoodX2019to2021",
                   "Telopea_DownsX2020to2021",
                   "TempyX2019to2020", #"TempyX2021", #no yield
                   "WaikerieX2018to2020",
                   "WarnertownX2019to2021",
                   "WynarkaX2019to2021",
                   "YendaX2017to2021",
                   "YounghusbandX2020to2021"
                   )



#######################################################################################################################################################
##########                                                    As a loop                                                                       ##########
#######################################################################################################################################################

for (site_yrs_list in site_yrs_list){
  
  
#################################################################################################################
  
  
data_file_Cum_ANOVA <- "X:/Therese_Jackie/Sandy_soils/Development_database/stats_batch_output/final_method/Cum_ANOVA_sites_merged_90.csv"
                       
data_file <- "X:/Therese_Jackie/Sandy_soils/Development_database/completeDB/sites_merged.csv"
             
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
  "Control",
  "Unmodified+OnRow_none",
  "Unmodified_SE14.band_8",
  "Unmodified_Bi_Agra.surface+band_8",
  "Unmodified_Lc.surface",
  "Unmodified_Cl.surface",
  "Unmodified_Cl@2.5.surface_Yr18,19,20",
  "Unmodified_Cl@3.incorp_8",
  "Unmodified_Cl@5.incorp_8",
  "Unmodified_Cl@5.incorp_8.Fert.surface",
  "Unmodified_Cl@5.incorp_8.Clay.incorp_8",
  "Unmodified_Cl@5.incorp_8.Fert.surface.Clay.incorp_8",
  "Unmodified_Cl@7.5.surface",
  "Unmodified_Cl@20.incorp_8",
  "Unmodified_Cl@20.incorp_8.Fert.surface",
  "Unmodified_Cl@20.incorp_8.Clay.incorp_8",
  "Unmodified_Cl@20.incorp_8.Fert.surface.Clay.incorp_8",
  "Unmodified_Fert.foliar",
  "Unmodified_Fert.surface",
  "Unmodified_Fert.incorp_8",
  "Unmodified_Fert.band_8",
  "Unmodified_K_added.surface",
  "Unmodified_Fert.band_30",
  "Unmodified_Fert.surface.Clay.incorp_8",
  "Unmodified_Fert.band_30.Clay.incorp_10",
  "Unmodified_Clay.check",
  "Unmodified_Clay.incorp_8",
  "Unmodified_Clay.incorp_10",
  "Unmodified_none.Fert.surface", #ouyen DD
  
  
  "Pre_drill_20+7.5_none", #ouyen DD
  "Pre_drill_20+7.5_none_annual",
  "Pre_drill_20+20_none",
  "Pre_drill_20+20_none_annual",
  "Pre_drill_20+20_Fert.banded_20",
  "Pre_drill_20+20_Fert.banded_20_annual",
  
  
  
  "Spade.30_none",
  "Spade.30_Lc@1.incorp_30",
  "Spade.30_Lc@2.incorp_30",
  "Spade.30_Lc@4.incorp_30",
  "Spade.30_Lc@6.incorp_30",
  "Spade.30_Lc@8.incorp_30",
  "Spade.30_Lc@15.incorp_30",
  "Spade.30_Lc@10.incorp_30",
  "Spade.30_Lc@20.incorp_30",
  "Spade.30_Lc@1.incorp_30.K_added.surface",
  "Spade.30_Lc@2.incorp_30.K_added.surface",
  "Spade.30_Lc@4.incorp_30.K_added.surface",
  "Spade.30_Lc@6.incorp_30.K_added.surface",
  "Spade.30_Lc@8.incorp_30.K_added.surface",
  "Spade.30_Lc@10.incorp_30.K_added.surface",
  "Spade.30_Lc@15.incorp_30.K_added.surface",
  "Spade.30_Lc@20.incorp_30.K_added.surface",
  "Spade.30_Lc.incorp_30",
  "Spade.30_Lc.incorp_30.Fert.incorp_30",
  "Spade.30_Lc.incorp_30.Clay.incorp_30",
  "Spade.30_Lc.incorp_30.Fert.incorp_30.Clay.incorp_30",
  "Spade.30_Cl.incorp_30",
  "Spade.30_Cl.incorp_30.Gypsum.incorp_30",
  "Spade.30_Fert.incorp_30.Clay.incorp_30",
  "Spade.30_Com.incorp_30",
  "Spade.30_Cereal.incorp_30",
  "Spade.30_Vetch.incorp_30",
  "Spade.30_Vet_Cer.incorp_30",
  "Spade.30_Vet_Cer_In.incorp_30",
  "Spade.30_K_added.surface",
  "Spade.30_Fert.incorp_30",
  "Spade.30_Fert.incorp_30.K_added.incorp_30",
  
  "Spade.30_Clay@250.incorp_30",
  "Spade.30_Clay@500.incorp_30_Yr07,20",
  "Spade.30_Clay.check",
  "Spade.30_Clay.incorp_30",
  "Spade.30_Gypsum.incorp_30",
  
  "Rip.30_none",
  "Rip.35_none",
  "Rip.30_Cl.surface",
  "Rip.30_Cl@7.5.surface",
  "Rip.30_Cl.band_30",
  "Rip.30_Cl@7.5.band_30",
  "Rip.30_Lc.incorp_30",
  "Rip.30_Lc.band_30",
  "Rip.30_Fert.surface",
  "Rip.30_Fert.incorp_30",
  "Rip.30_Fert.band_8",
  "Rip.30_Fert.band_30",
  "Rip.30IncRip_none",
  "Rip.30+60_none",
  "Rip.30IncRip_Gypsum.incorp_30",
  "Rip.30+60_Lc.band_30+60",
  
  "Rip_30+7.5_none", #ouyen DD
  "Rip_30+7.5_none_annual",
  "Rip_30+30_none",
  "Rip_30+30_none_annual",
  "Rip_30+30_Fert.banded_30",
  "Rip_30+30_Fert.banded_30_annual",
  
  "Sweep.30_none",
  "Sweep.30_Lime.incorp_30",
  "Sweep.30_Cl@9.incorp_30",
  "Sweep.30_Cl@9.incorp_30_Yr17,18,19",
  "Sweep.30_Cl@6.incorp_30",
  "Sweep.30_Cl@3.incorp_30.Lime.incorp_8",
  "Sweep.30_Cl@3.incorp_30",
  
  "Rip.40_none",
  "Rip.40IncRip_none",
  "Rip.40IncRip_Lc.incorp_30",
  "Rip.40IncRip_Lc.incorp_40",
  
  
  "Rip.40_Lc.incorp_40",
  "Rip.40_Fert.incorp_40",
  
  "Rip.45_none",
  "Rip.45IncRip_none",
  "Rip.45IncRip+Spade.30_none",
  "Rip.45IncRip_Fert.incorp_45",
  "Rip.45IncRip_Fert_Low.band_45",
  "Rip.45IncRip_Fert_High.band_45",
  "Rip.45IncRip_Fert_APP.band_45",
  
  "Rip.50_none",
  "Rip.50_Cl.surface",
  "Rip.50_Cl@2.5.surface_Yr18,19,20",
  "Rip.50_Cl@7.5.surface",
  "Rip.50_Cl@5.incorp_20",#changed
  "Rip.50_Cl@7.5.band_50",
  "Rip.50_Cl@20.incorp_20",#changed
  "Rip.50_Cl.deep",
  "Rip.50_Cl.band_50",
  "Rip.50_Cl@5.incorp_20.Fert.surface", #changed
  "Rip.50_Cl@5.incorp_20.Clay.incorp_20",#changed
  "Rip.50_Cl@5.incorp_20.Fert.surface.Clay.incorp_20",#changed
  "Rip.50_Cl@20.incorp_20.Fert.surface", #changed
  "Rip.50_Cl@20.incorp_20.Clay.incorp_20",#changed
  "Rip.50_Cl@20.incorp_20.Fert.surface.Clay.incorp_20",#changed
  "Rip.50_Fert.surface",
  "Rip.50_Clay.incorp_20",#changed
  "Rip.50_Fert.surface.Clay.incorp_20",#changed
  "Rip.50Spade.30_none",
  #"Inc.50_none",
  #"Inc.50_Cl@7.5.incorp_50",
  "Rip.50IncRip_none",
  "Rip.50IncRip_Cl.incorp_50",
  "Rip.50IncRip_Cl.surface",
  "Rip.50IncRip_Cl@7.5.incorp_50",
  
  "Rip.60_none",
  "Rip.60_Cl.surface",
  "Rip.60_Cl.band_60",
  "Rip.60_Lc.incorp_60",
  "Rip.60_Lc.band_60",
  "Rip.60_Fert.band_8",
  "Rip.60_Fert.band_60",
  "Rip.60Spade.30_none",
  "Rip.60Spade.30_Lc.band_30+60",
  "Rip.60Spade.30_Lc.incorp_30+band_60",
  "Rip.60IncRip_none",
  "Rip.60IncRip+Spade.30_none",
  
  
  #"Delving.18_none",
  #"Delving.18_SE14.band_8",
  "Unmodified+DeepTill.18_none",
  "Unmodified+DeepTill.18_SE14.band_8",
  
  "DiscInv.30_none"
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
### brooker is a problem site I want to filter out these ones:

site_year_yld_summary_site <- site_year_yld_summary_site %>%
  filter(Descriptors  != "Spade.30_Lc@1.incorp_30") %>%
  filter(Descriptors  != "Spade.30_Lc@1.incorp_30.K_added.surface") %>%
  filter(Descriptors  != "Spade.30_Lc@2.incorp_30") %>%
  filter(Descriptors  != "Spade.30_Lc@2.incorp_30.K_added.surface") %>%
  filter(Descriptors  != "Spade.30_Lc@6.incorp_30") %>%
  filter(Descriptors  != "Spade.30_Lc@6.incorp_30.K_added.surface") %>%
  filter(Descriptors  != "Spade.30_Lc@10.incorp_30") %>%
  filter(Descriptors  != "Spade.30_Lc@10.incorp_30.K_added.surface") %>%
  filter(Descriptors  != "Spade.30_Lc@20.incorp_30") %>% 
  filter(Descriptors  != "Spade.30_Lc@20.incorp_30.K_added.surface")

### Younghusband is a problem site I want to filter out these ones there is not the same level of reps for treatments:

site_year_yld_summary_site <- site_year_yld_summary_site %>%
  
  filter(Descriptors  != "Unmodified+DeepTill.18_SE14.band_8") %>%
  filter(Descriptors  != "Unmodified+DeepTill.18_none") %>%
  filter(Descriptors  != "Unmodified+DeepTill.18_none") %>%
  filter(Descriptors  != "Unmodified+OnRow_none") 


site_year_yld_summary_site <- site_year_yld_summary_site[!( site_year_yld_summary_site$site == "Younghusband" & ( site_year_yld_summary_site$Descriptors == "Control" )),] 



# ### make a dummy clm to filter out Younghusband 2021 data
# site_year_yld_summary_site <- site_year_yld_summary_site %>%
#   dplyr::mutate(site_yr_dummy = paste0(site, year))
# 
# ## filter out the 2021 Younghusband data
# site_year_yld_summary_site <- site_year_yld_summary_site %>%
#   filter(site_yr_dummy  != "Younghusband2021") 


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
  "2021",
  "2020",
  "2019",
  "2018",
  "2017",
  "2016",
  "2015",
  "2014"
  )

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

names(Cum_ANOVA_results_site)
### make a new clm for plotting letters if the cum ANOVA is significant
Cum_ANOVA_results_site <- Cum_ANOVA_results_site %>% 
  dplyr::mutate(groups_LSD_cum_display = case_when(
    ANOVA_sign_0.1 == "ns" ~ "",
    TRUE ~ groups_LSD_cum  ))

Cum_ANOVA_results_site <- Cum_ANOVA_results_site %>% 
  dplyr::mutate(significance_control_display = case_when(
    ANOVA_sign_0.1 == "ns" ~ "",
    TRUE ~ significance_control_0.1  ))



Cum_ANOVA_results_site <- Cum_ANOVA_results_site %>% 
  dplyr::mutate(LSD_cum_display = case_when(
    ANOVA_sign_0.1 == "ns" ~ paste0("") ,
    TRUE ~ paste0("LSD = ", signif(LSD_cum, digits = 4))))

Cum_ANOVA_results_site <- Cum_ANOVA_results_site %>% 
  dplyr::mutate(Dunnetts_display = case_when(
    ANOVA_sign_0.1 == "ns" ~ paste0("") ,
    TRUE ~ paste0("Dunnetts test")))

### some sites have average yld that is different to cum yield this sorts it out
max_sum_cum_1 <-site_year_yld_summary %>% 
  dplyr::group_by(Descriptors) %>%
  dplyr::summarise(yld=sum(mean, na.rm = TRUE))
max_sum_cum_1 <- max(max_sum_cum_1$yld, na.rm = TRUE) 

max_max <- data.frame(name = c("max_cum_sum" , "max_av" ),
                      value = c(max_sum_cum, max_sum_cum_1))

max_sum_cum <- max(max_max$value, na.rm = TRUE) 
max_sum_cum <- max_sum_cum +0.5
max_sum_cum <- ceiling(max_sum_cum)




CumPlot_LSD <- site_year_yld_summary %>% 
  ggplot( aes(x = factor(Descriptors), y = mean, fill = year, colour = year)) + 
  geom_bar(stat = "identity",  alpha = 0.5)  +
  labs(x="", 
       y="Cumulative Yield (t/ha)", 
       title = paste0(a),
       subtitle = paste0("ANOVA " ,Cum_ANOVA_results_site$LSD_cum_display))+
  theme_bw() + 
  scale_y_continuous(breaks=seq(0,max_sum_cum,by=1.0), limits = c(0, max_sum_cum))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  
  theme(
    axis.text.x=element_text(angle=50,hjust=1, size = 10),
    axis.text.y=element_text(size = 10),
    plot.title = element_text(size = 20))+
  
  geom_text(data = Cum_ANOVA_results_site,
            aes(x = factor(Descriptors), y = (mean_cum_yld +0.5), label=groups_LSD_cum_display), 
            position = position_dodge(0.80), 
            size = 3,
            vjust=-0.5, hjust=0.1, 
            colour = "gray25")
CumPlot_LSD

### save the plot
ggsave(CumPlot_LSD,
       device = "png",
       filename = paste0("Plot_yield_", 
                         a,"_", b, "_Cum_ANOVA_Plot_LSD", ".png"),
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/stats_batch_output/final_method/Yield_Cumulative_LSD_Plots/",
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
       subtitle = paste0("ANOVA ", Cum_ANOVA_results_site$Dunnetts_display ))+
  theme_bw() + 
  scale_y_continuous(breaks=seq(0,max_sum_cum,by=1.0), limits = c(0, max_sum_cum))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  
  theme(
    axis.text.x=element_text(angle=50,hjust=1, size = 10),
    axis.text.y=element_text(size = 10),
    plot.title = element_text(size = 20))+
  
  geom_text(data = Cum_ANOVA_results_site,
            aes(x = factor(Descriptors), y = (mean_cum_yld +0.5),label=significance_control_display), 
            position = position_dodge(0.80), 
            size = 3,
            vjust=-0.5, hjust=0.1, 
            colour = "gray25")


### save the plot
ggsave(CumPlot_Dun,
       device = "png",
       filename = paste0("Plot_yield_", 
                         a,"_", b, "_Cum_ANOVA_Plot_Dun", ".png"),
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/stats_batch_output/final_method/Yield_Cumulative_Dunnetts_Plots/",
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








