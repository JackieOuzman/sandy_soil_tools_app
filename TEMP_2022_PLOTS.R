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


######################################################################################################
########    function for filtering the data - and creating a plot on the map page     ################
###################################################################################################### 
#-----output$trial_plot <- renderPlot({
  
#----  ## text that appears on the plot
  ## what is the decile year for our trial results?
  # decile_year0_temp <-filter(trial_results,
  #                            site == input$site_selection & yr_post_amelioration == 0 ) %>%
  #   select(decile) %>%
  #   distinct(decile)
  # decile_year1_temp <-filter(trial_results,
  #                            site == input$site_selection & yr_post_amelioration == 1 ) %>%
  #   select(decile) %>%
  #   distinct(decile)
  # 
  # decile_year0 = as.character(unique(decile_year0_temp$decile))
  # decile_year1 = as.character(unique(decile_year1_temp$decile))
  # 
  ## text that appears on the plot
  ## what is the year of amerolaition for the site / trial ?

  
  
  # year_amelioration <- trial_results %>%
  #   filter(site == input$site_selection) %>%
  #   dplyr::select(Amelioration_Year) %>%
  #   distinct(Amelioration_Year)
  
#---  #year_amelioration = as.character(unique(year_amelioration$Amelioration_Year))
# names(trial_results)  
# unique(trial_results$site)

  ## the plot
  site_plot_descriptors <-
    #--trial_results %>% filter(site == input$site_selection) %>%
    trial_results %>% filter(site == "Cummins") %>%
    dplyr::select(site, Descriptors, yr_post_amelioration, yield) %>%
    ggplot ( aes(x = Descriptors)) +
    geom_bar(
      aes(y = yield, fill = as.factor(yr_post_amelioration)),
      stat = "summary",
      fun.y = "mean",
      position = position_dodge(0.8),
      width = 0.7,
      show.legend = TRUE
    ) +
    
    #guides(fill = guide_legend(title = "Years post amelioration")) +
    scale_fill_grey() +
    theme_bw()+
    theme(plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 18),
          
          axis.title.y = element_text(size = 18),
          axis.title.x = element_text(size = 18),
          
          axis.text.x = element_text(angle = 90, hjust=1,size = 14),
          axis.text.y = element_text(size = 14))+
    
    labs(title= paste0("Site: ", 
                       #input$site_selection, 
                       ", Year amelioration: "#, 
                       #year_amelioration
                       ),
         subtitle = paste0("Deciles: year 0 = " , 
                           #decile_year0,  
                           " ,year 1 = "#, 
                           #decile_year1
                           ),
         x ="Trial", y = "Yield t/ha")
  
  
  site_plot_descriptors
  
  
#----####})
#########################################################################################################
############ v2 #####################################################################################
### The below code alreday exist and works in the app ##### 
  
  #yld_table
  
  yld_table <- read.csv("yield_table_av_v2.csv")
  
  
  yld_table <- yld_table %>% rename(
    "yield_unmodified" = 'yield...un.modified.',
    "yield_modified" =    'yield..modified.')
  
  yld_table <- yld_table %>%
    dplyr::select(grouping,
                  modification,
                  site,
                  year = yr_post_amelioration,
                  crop,
                  "yield_unmodified",
                  "yield_modified",
                  price,
                  "data source" = data.source,
    )
  
  yld_table <- ungroup(yld_table)
  
  yld_table <- yld_table %>%
    mutate(year = year +1)
  
  yld_table$grouping <- as.character(yld_table$grouping)
  yld_table$modification <- as.character(yld_table$modification)
  yld_table$site <- as.character(yld_table$site)
  yld_table$crop  <- as.character(yld_table$crop )
  yld_table$`data source` <- as.character(yld_table$`data source`)
  
  #code no trial data as such
  yld_table <- yld_table %>%
    dplyr::mutate(`data source` = case_when(
      is.na(crop) ~ "no trial data",
      TRUE   ~ `data source`
    ))
#########################################################################################
#### code from my cum ANOVA plots step 5 ################################################
##########################################
  site_and_yrs <- yld_table # Just remaning so I can hack the code 
names(site_and_yrs)

site_yrs_list = "BrookerX2019to2021"  #This will be from the drop down and will have the selectinput$site_selection

site_and_yrs <- as.data.frame(str_split(site_yrs_list, "X"),
                                col.names = "site_and_yrs" )
  
  site_and_yrs$site_and_yrs <- as.character(site_and_yrs$site_and_yrs)
  site_and_yrs
  a <- site_and_yrs[1,1]
  b <- site_and_yrs[2,1]
  b
  
  ######################################
  
  # bring in the data
  
  
  
  
  summary_data_all_1 <- trial_results
  
  
  ### brooker is a problem site I want to filter out these ones:
  
  summary_data_all_1 <- summary_data_all_1 %>%
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
  
  summary_data_all_1 <- summary_data_all_1 %>%
    
    filter(Descriptors  != "Unmodified+DeepTill.18_SE14.band_8") %>%
    filter(Descriptors  != "Unmodified+DeepTill.18_none") %>%
    filter(Descriptors  != "Unmodified+DeepTill.18_none") %>%
    filter(Descriptors  != "Unmodified+OnRow_none") 
  
  
summary_data_all_1 <- summary_data_all_1[!( summary_data_all_1$site == "Younghusband" & ( summary_data_all_1$Descriptors == "Control" )),] 
  
#site_plot_descriptors_v2 <-
