library(shiny)
library(ggplot2)
library(readxl)
library(tidyverse)
library(rhandsontable)
library(shinyalert)
library(shinydashboard)
library(htmltools)
library(leaflet)
library(slickR) #new install on land and water linex

library(shinyWidgets)

library(vroom) #new i
###############################################################################################
#### This bit works already
###############################################################################################
trial_results <- read_csv("primary_data_all_v2.csv")
names(trial_results)

### arrange the data / Descriptor so the order reflect level of intervension

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

trial_results$Descriptors <- factor(trial_results$Descriptors,
                                    levels = order)
trial_results <- trial_results %>%
  arrange(site)

######## trial data ##### I am not sure what this step does?

file =
  trial_results_table <- read_csv("primary_data_all_v2.csv")
trial_results_table <- trial_results_table %>%
  arrange(site)
###############################################################################################


########################################################################################
#### code from my cum ANOVA plots step 8 ################################################
########################################################################################

# set the site name this will be a filter with R shiny 'input$site_selection'
site_in_app <- "Brooker"

### name of site selected

a <-  site_in_app #name of the site
b <-  trial_results %>% 
  filter(site == a) %>% 
  distinct(year)
b <- paste0(min(b$year), "to", max(b$year)) #the years of the trial


##############################################################################################################################################
################                 TRIAL DATA                    ################
###############################################################################################################################################

#filter the TRIAL data on site 
site_year_yld_summary_site <- trial_results_table %>%
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

site_year_yld_summary <- site_year_yld_summary_site 


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

#what is the max yield value on the y axis



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




### Plot

names(site_year_yld_summary)
site_year_yld_summary$year <- as.factor(site_year_yld_summary$year)

CumPlot <- site_year_yld_summary %>% 
  ggplot( aes(x = factor(Descriptors), y = mean, fill = year, colour = year)) + 
  geom_bar(stat = "identity",  alpha = 0.5)  +
  labs(x="", 
       y="Cumulative Yield (t/ha)", 
       title = paste0(a))+
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  
  theme(
    axis.text.x=element_text(angle=50,hjust=1, size = 10),
    axis.text.y=element_text(size = 10),
    plot.title = element_text(size = 20)) 
  
  
CumPlot

