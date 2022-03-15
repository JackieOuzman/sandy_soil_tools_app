

#Graphing the yld results for site and Yr


library(ggplot2)
library(readxl)
library(tidyverse)
library(multcompView)
library(scales)

#site_yrs_list <- "Brimpton LakeX2014"

### List of sites I want to run analysis for:
site_yrs_list <- c("Brimpton LakeX2014",
                   "Brimpton LakeX2015",
                   "Brimpton LakeX2016",
                   "Brimpton LakeX2017",
                   "Brimpton LakeX2018",
                   
                   "BrookerX2019",
                   "BrookerX2020",
                   "BrookerX2021",
                   
                   "BucklebooX2019",
                   "BucklebooX2020",
                   "BucklebooX2021",
                   
                   "Bute_CSIROX2018",
                   "Bute_CSIROX2019",
                   "Bute_CSIROX2020",
                   "Bute_CSIROX2021",
                   
                   "Bute_TrengroveX2015",
                   "Bute_TrengroveX2016",
                   "Bute_TrengroveX2017",
                   "Bute_TrengroveX2018",
                   "Bute_TrengroveX2019",
                   "Bute_TrengroveX2020",
                   "Bute_TrengroveX2021",
                   
                   "CadgeeX2014",
                   "CadgeeX2015",
                   #"CadgeeX2016", #no yield data
                   "CadgeeX2017",
                   "CadgeeX2018",
                   
                   "Carwarp_AmeliorationX2018",
                   "Carwarp_AmeliorationX2019",
                   "Carwarp_AmeliorationX2020",
                   
                   "CumminsX2019",
                   "CumminsX2020",
                   "CumminsX2021",
                   
                   "KarkooX2019",
                   "KarkooX2020",
                   "KarkooX2021",
                   
                   "KaroondaX2014",
                   "KaroondaX2015",
                   "KaroondaX2016",
                   "KaroondaX2017",
                   "KaroondaX2018",
                   
                   "Kooloonong_canolaX2021",
                   
                   "Kooloonong_chickpeaX2019",
                   "Kooloonong_chickpeaX2020",
                   "Kooloonong_chickpeaX2021",
                   
                   "Kooloonong_lentilX2019",
                   "Kooloonong_lentilX2020",
                   "Kooloonong_lentilX2021",
                   
                   "Kooloonong_lupinX2019",
                   "Kooloonong_lupinX2020",
                   "Kooloonong_lupinX2021",
                   
                   "KybungaX2019",
                   "KybungaX2020",
                   "KybungaX2021",
                   
                   "Lowaldie_CrestX2019",
                   "Lowaldie_CrestX2020",
                   #"Lowaldie_CrestX2021", #no data 
                   
                   "Lowaldie_Deep sandX2019",
                   "Lowaldie_Deep sandX2020",
                   #"Lowaldie_Deep sandX2021",#no data
                   
                   "MalinongX2019",
                   "MalinongX2020",
                   "MalinongX2021",
                   
                   "Monia_GapX2019",
                   "Monia_GapX2020",
                   "Monia_GapX2021",
                   
                   "Mt DamperX2019",
                   "Mt DamperX2020",
                   "Mt DamperX2021"
)



#######################################################################################################################################################
##########                                                    As a loop                                                                       ##########
#######################################################################################################################################################

for (site_yrs_list in site_yrs_list){
  

##################################################################################################################


data_file <- "X:/Therese_Jackie/Sandy_soils/Development_database/stats_batch_output/ANOVA_by_Yr_sites_merged.csv"

## download the data using the specified file path above

ANOVA_results <- read_csv(data_file) 
                             


##########################################
site_and_yr <- as.data.frame(str_split(site_yrs_list, "X"),
                             col.names = "site_and_yr" )

site_and_yr$site_and_yr <- as.character(site_and_yr$site_and_yr)
site_and_yr
a <- site_and_yr[1,1]
b <- site_and_yr[2,1]
b <- as.double(b)

######################################

##### order the Descriptors
order <- c(
  "Control",
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
  
  "Sweep.30_none",
  "Sweep.30_Lime.incorp_30",
  "Sweep.30_Cl@9.incorp_30",
  "Sweep.30_Cl@6.incorp_30_Yr18,19,20",
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
  "Inc.50_none",
  "Inc.50_Cl@7.5.incorp_50",
  "Rip.50IncRip_none",
  "Rip.50IncRip_Cl.incorp_50",
  "Rip.50IncRip_Cl.surface",
  
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
  
  "Delving.18_none",
  "Delving.18_SE14.band_8",
  "DiscInv.30_none"
  
)



ANOVA_results$Descriptors <- factor(ANOVA_results$Descriptors,
                                         levels = order)


#filter the data on site first
ANOVA_results_site <- ANOVA_results %>%
  filter(site == a) 

max_yld <- max(ANOVA_results_site$mean, na.rm = TRUE) 
max_yld <- max_yld +0.5
max_yld <- ceiling(max_yld)

ANOVA_results_site$LSD <- as.double(ANOVA_results_site$LSD)
LSD <- max(ANOVA_results_site$LSD)
LSD <- signif(LSD, digits = 4)



ANOVA_results <- ANOVA_results_site %>%
  filter(year == b)


# barplot with letters from LSD
plot_LSD <- ANOVA_results %>%  
  ggplot( aes(x = factor(Descriptors), y = mean)) + 
  geom_bar(stat = "identity",  alpha = 0.5)  +
  geom_errorbar(aes(ymin=mean-std_error, ymax=mean+std_error), width = 0.1) +
  theme_classic() + 
  
  scale_y_continuous(breaks=seq(0,max_yld,by=0.5), limits = c(0, max_yld))+
  
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = c(0.1, 0.75)) +
  
  geom_text(aes(label=groups_LSD), 
            position = position_dodge(0.80), 
            size = 3,
            vjust=-0.5, hjust=-0.3, 
            colour = "gray25")+
  
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  labs(x="", 
       y="Yield t/ha", title = paste0(a,": ", b),
       subtitle = paste0("ANOVA, LSD = ", LSD)) 

### save the LSD
ggsave(plot_LSD,
       device = "png",
       filename = paste0("Plot_yield_", 
                         a,"_", b, "_ANOVA_Plot_LSD", ".png"),
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/stats_batch_output/Yield_Yrs_LSD_Plots/",
       width=8.62,
       height = 6.28,
       dpi=600
)

#################################################################################################################



# barplot with letters from dunnetts
plot_dun <- ANOVA_results %>%  
  ggplot( aes(x = factor(Descriptors), y = mean)) + 
  geom_bar(stat = "identity",  alpha = 0.5)  +
  geom_errorbar(aes(ymin=mean-std_error, ymax=mean+std_error), width = 0.1) +
  theme_classic() + 
  
  scale_y_continuous(breaks=seq(0,max_yld,by=0.5), limits = c(0, max_yld))+
  
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = c(0.1, 0.75)) +
  
  geom_text(aes(label=significance_control), 
            position = position_dodge(0.80), 
            size = 3,
            vjust=-0.5, hjust=-0.3, 
            colour = "gray25")+
  
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  labs(x="", 
       y="Yield t/ha", title = paste0(a,": ", b),
       subtitle = paste0("ANOVA, Dunnetts")) 

### save the plot
ggsave(plot_dun,
       device = "png",
       filename = paste0("Plot_yield_", 
                         a,"_", b, "_ANOVA_Plot_dun", ".png"),
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/stats_batch_output/Yield_Yrs_Dunnetts_Plots/",
       width=8.62,
       height = 6.28,
       dpi=600
)



rm(a,
   b,
   max_yld,
   data_file,
   order,
   site_and_yr,
   ANOVA_results,
   ANOVA_results_site,
   plot_LSD,
   plot_dun,
   LSD)

}


