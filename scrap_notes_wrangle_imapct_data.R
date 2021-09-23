
##############################################################################################################
################## backrground info ################################################################################
##############################################################################################################
## I might need to play around with the data so I can plot it and its in the same format that the app is expecting.

### so I have 2 df to mess around with this is from pre_app_site.R

##### these files are used in the app one is mainly for trial data
#primary_data.csv

## one is for the map of trial sites
#site_location_plus_info.csv

##############################################################################################################
################## site info ################################################################################
##############################################################################################################
site_info <- read.csv(file = paste0("X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/App_working/data/","site_location_plus_info.csv"))
#remove sites with no coods
site_info <- 
  filter(site_info, latitude != "NA")


site_info <- site_info %>% 
  mutate( site_label = paste0("Site = ", site),
          non.wetting_label = paste0("non wetting score = ", non.wetting),
          acidic_label = paste0("acidic score = ", acidic),
          physical_label = paste0("physical score = ", physical))



##############################################################################################################
################## trial results ################################################################################
##############################################################################################################
trial_results <- read.csv(file = paste0("X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/App_working/data/","primary_data.csv"))
names(trial_results)
## remove any data that has chcek in descriptor
trial_results <- trial_results %>% 
  filter(Descriptors != "check")

###################################################################################################
## create some new clms
###################################################################################################

######## grouping #######
### I want to convert the decsriptors into something more generic - I will need some help here but lets make a start
unique(trial_results$Descriptors)

trial_results <- trial_results %>% 
  mutate(grouping = case_when(
    Descriptors == "Control" ~ "control",
    
    #no modification of soil but amendment applied
    Descriptors == "Unmodified_Cl.surface" ~ "amendment only",
    
    #Rip of soil deep and no amendment 
    
    Descriptors == "Rip.30_none" ~        "deep ripping no amendment",
    Descriptors == "Rip.35_none" ~        "deep ripping no amendment",
    Descriptors == "Rip.40_none" ~        "deep ripping no amendment",
    Descriptors == "Rip.45_none" ~         "deep ripping no amendment",
    Descriptors == "Rip.50_none" ~        "deep ripping no amendment",
    Descriptors == "Rip.60_none" ~        "deep ripping no amendment",
    
    
    Descriptors == "Rip.30IncRip_none" ~    "inclusion ripping no amendment",
    Descriptors == "Rip.40IncRip_none" ~     "inclusion ripping no amendment",
    Descriptors == "Rip.45IncRip_none" ~    "inclusion ripping no amendment",
    Descriptors == "Rip.50IncRip_none" ~    "inclusion ripping no amendment",
    Descriptors == "Rip.60IncRip_none" ~    "inclusion ripping no amendment",
    
    
    Descriptors == "Rip.45IncRip+Spade.30_none" ~ "inclusion ripping + spading no amendment",
    Descriptors == "Rip.50Spade.30_none" ~        "inclusion ripping + spading no amendment",
    Descriptors == "Rip.60Spade.30_none" ~        "inclusion ripping + spading no amendment",
    Descriptors == "Rip.60IncRip+Spade.30_none" ~        "inclusion ripping + spading no amendment",
    
    
    #Rip of soil deep and amendment applied 
    Descriptors == "Rip.30_Fert.surface" ~     "deeping rip amendment",
    Descriptors == "Rip.50_Cl.surface" ~       "deeping rip amendment",
    Descriptors == "Rip.50_Cl.deep" ~          "deeping rip amendment",
    
    
    Descriptors == "Rip.30IncRip_gypsum.mix" ~       "inclusion ripping amendment",
    Descriptors == "Rip.40IncRip_Lc.mix" ~           "inclusion ripping amendment",
    Descriptors == "Rip.45IncRip_Fert.deep" ~        "inclusion ripping amendment",
    Descriptors == "Rip.45IncRip_Fert.mix" ~         "inclusion ripping amendment",
    Descriptors == "Rip.50IncRip_Cl.mix" ~           "inclusion ripping amendment",
    
    
    #Spade of soil  and no amendment 
    Descriptors == "Spade.30_none" ~       "rotary spading no amendment",
    
    #Spade of soil  and  amendment 
    Descriptors == "Spade.30_gypsum.mix" ~     "rotary spading amendment",
    Descriptors == "Spade.30_Cl.mix" ~         "rotary spading amendment",
    Descriptors == "Spade.30_Cl.gypsum.mix" ~  "rotary spading amendment",
    Descriptors == "Spade.30_Fert.mix" ~       "rotary spading amendment",
    Descriptors == "Spade.30_clay@250" ~       "rotary spading amendment",
    Descriptors == "Spade.30_clay@500" ~       "rotary spading amendment",
    
    #Spade of soil deep and  amendment 
    Descriptors == "DiscInv.30_none" ~     "disc inversion no amendment",       
                 
    TRUE ~ "check")
  ) 

######## modification #######

unique(trial_results$grouping)

trial_results <- trial_results %>% 
  mutate(modification = case_when(
    grouping == "amendment only" ~ "no modification",
    grouping == "deep ripping no amendment" ~ "deep ripping",
    grouping == "deeping rip amendment" ~ "deep ripping",
    grouping == "inclusion ripping no amendment" ~ "inclusion ripping",
    grouping == "inclusion ripping amendment" ~ "inclusion ripping",
    grouping == "inclusion ripping + spading no amendment" ~ "inclusion ripping + spading",
    grouping == "rotary spading no amendment" ~ "rotary spading",
    grouping == "rotary spading amendment" ~ "rotary spading",
    grouping == "disc inversion no amendment" ~ "disc inversion",  
    grouping == "control" ~ "control",
    
      TRUE ~ "check")
  ) 


######## class of constraint #######              
names(trial_results)
unique(trial_results$Physical)
str(trial_results$Physical)

trial_results$Repellence <- as.double(trial_results$Repellence)
trial_results$Acidity <- as.double(trial_results$Acidity)
trial_results$Physical <- as.double(trial_results$Physical)

trial_results <- trial_results %>%
  mutate(non_wetting = case_when(Repellence == 0 ~ "green",
                                   Repellence == 1 ~ "red"))
trial_results <- trial_results %>%
  mutate(acidic = case_when(Acidity == 0 ~ "green",
                            Acidity == 1 ~ "red",
                            TRUE ~ "check"))
trial_results <- trial_results %>%
  mutate(physical = case_when(Physical == 0 ~ "green",
                              Physical == 1 ~ "red",
                              TRUE ~ "check"))

######## mean annual rainfall ####### 
#using station number in the analogue years cals
#http://www.bom.gov.au/jsp/ncc/cdio/weatherData/
unique(trial_results$met_name_number)
trial_results$met_name_number <- as.character(trial_results$met_name_number)
trial_results <- trial_results %>%
  mutate(rainfall_mean_annual = case_when(
    met_name_number=="BALRANALD (RSL)_049002" ~ "323.1",
    met_name_number=="OUYEN (POST OFFICE)_076047" ~ "328.4",
    met_name_number=="KAROONDA_025006" ~ "340.4",
    met_name_number=="HILLSTON AIRPORT_075032" ~ "369.8",
    met_name_number=="COOMANDOOK (MALINONG)_025508" ~ "458.3",
    met_name_number=="KEITH_025507" ~ "459.7",
    met_name_number=="CUMMINS_018023" ~ "422.5",
    met_name_number=="YEELANNA_018099" ~ "408.3",
    met_name_number=="KIMBA_018040" ~ "346.2",
    met_name_number=="MINNIPA AGRICULTURAL CENTRE_018052" ~ "327.3",
    met_name_number=="CLARE POST OFFICE_021014" ~ "633.7",
    met_name_number=="NUROM (RIVERSIDE FARM)_021102" ~ "374.8",
    met_name_number=="SERVICETON_078034" ~ "495.5",
    TRUE ~ "check"))
######## met station number ####### 
trial_results <- trial_results %>%
  mutate(site_numb = case_when(
    met_name_number=="BALRANALD (RSL)_049002" ~ "049002",
    met_name_number=="OUYEN (POST OFFICE)_076047" ~ "076047",
    met_name_number=="KAROONDA_025006" ~ "025006",
    met_name_number=="HILLSTON AIRPORT_075032" ~ "075032",
    met_name_number=="COOMANDOOK (MALINONG)_025508" ~ "025508",
    met_name_number=="KEITH_025507" ~ "025507",
    met_name_number=="CUMMINS_018023" ~ "018023",
    met_name_number=="YEELANNA_018099" ~ "018099",
    met_name_number=="KIMBA_018040" ~ "018040",
    met_name_number=="MINNIPA AGRICULTURAL CENTRE_018052" ~ "018052",
    met_name_number=="CLARE POST OFFICE_021014" ~ "021014",
    met_name_number=="NUROM (RIVERSIDE FARM)_021102" ~ "021102",
    met_name_number=="SERVICETON_078034" ~ "078034",
    TRUE ~ "check"))

temp <- trial_results %>%
  dplyr::select(
    site,
    Descriptors,
    grouping,
    modification,
    non_wetting,
    acidic,
    physical,
    rainfall_mean_annual,
    site_numb
  )


#### how to assign cost to fit the model I have

unique(temp$grouping)
