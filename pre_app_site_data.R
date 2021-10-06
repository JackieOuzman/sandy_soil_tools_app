## Pre app data ##
# some of this will be replicated in the final product that is on team #




library(ggplot2)
library(readxl)
library(tidyverse)
#################################################################################################################
####   Get the data #####
input_data_file <- "Jackie_working/2020 Impacts_Master data_template_20210222_jaxs_v21_09.xlsx"

input_data_file_rain <- "X:/Therese_Jackie/Sandy_soils/Sands weather/met_file2021/GS_rain_deciles_impact_sites.csv"



primary_2019_imapct <- read_excel(paste0("C:/Users/ouz001/CSIRO/GRDC_CSP00203 - ", input_data_file), 
                                                                      sheet = "Primary_data_2019", skip=1)
primary_2020_imapct <- read_excel(paste0("C:/Users/ouz001/CSIRO/GRDC_CSP00203 - ", input_data_file),  
                                  sheet = "Primary_data_2020", skip = 1)
#append the two datasets

primary_2019_2020_imapct <- rbind(primary_2019_imapct,primary_2020_imapct)
rm(primary_2019_imapct, primary_2020_imapct)


#################################################################################################################
####   Create the descriptors #####


primary_2019_2020_imapct$rip_depth <- as.double(primary_2019_2020_imapct$rip_depth)
primary_2019_2020_imapct <- primary_2019_2020_imapct %>% 
  mutate(Rip_depth_jax = rip_depth/10)
#primary_2019_2020_imapct$mix_depth_jax <- as.double(primary_2019_2020_imapct$mix_depth_jax)
#######################################################################################################################
# step 1 what depth was amendment applied?

primary_2019_2020_imapct <- primary_2019_2020_imapct %>% 
  mutate(placement_jax = case_when(
    placement  == "surface"  ~           "surface",
    placement  == "incorporated"  ~      "mix",
    placement  == "deep"  ~              "deep"))


#step 2 make a clm with what ameliorant was added if any
primary_2019_2020_imapct <- primary_2019_2020_imapct %>% 
  mutate(amendment = case_when(
    organic  == "chicken_compost" | organic  ==  "chicken_manure" &  other_ameliorant ==  "none"    ~        paste0("Cl",".", placement_jax),
    organic  == "chicken_compost" | organic  ==  "chicken_manure" &  other_ameliorant ==  "gypsum"       ~   paste0("Cl.gypsum",".", placement_jax),
    organic  == "chicken_compost" | organic  ==  "chicken_manure" &  other_ameliorant ==  "clay"       ~     paste0("Cl.clay",".", placement_jax),
    
    organic  == "lucerne"  &  other_ameliorant ==  "none"    ~           paste0("Lc",".", placement_jax),
    organic  == "lucerne"  &  other_ameliorant ==  "gypsum"       ~      paste0("Lc.gypsum",".", placement_jax),
    organic  == "lucerne"  &  other_ameliorant ==  "clay"       ~        paste0("Lc.clay",".", placement_jax),
    
    organic  == "none"  &  fertiliser == "none"   &    other_ameliorant ==  "none"    ~      "none",
    organic  == "none"  &  fertiliser == "none"   &    other_ameliorant ==  "gypsum"    ~    paste0("gypsum",".", placement_jax),
    #organic  == "none"  &  fertiliser == "none"   &    other_ameliorant ==  "clay"    ~      paste0("clay",".", placement_jax), # this was wrong
    organic  == "none"  &  fertiliser == "none"   &    other_ameliorant ==  "clay"    ~      paste0("clay"),
    
    organic  == "none"  &  fertiliser != "none"   &    other_ameliorant ==  "none"    ~      paste0("Fert",".", placement_jax),
    organic  == "none"  &  fertiliser != "none"   &    other_ameliorant ==  "gypsum"    ~    paste0("Fert.gypsum",".", placement_jax),
    organic  == "none"  &  fertiliser != "none"   &    other_ameliorant ==  "clay"    ~      paste0("Fert.clay",".", placement_jax),
    
    TRUE ~ as.character("check")
    
  ) )

  
    
primary_2019_2020_imapct <- primary_2019_2020_imapct %>% 
  mutate(Descriptors = case_when(
    rip  == "none"  &      mix == "none" & amendment == "none"     ~      "Control",
    rip  == "none"  &      mix == "spade"      ~     paste0("Spade.30", "_", amendment ),
    rip  == "none"  &      mix == "Plozza"    ~    paste0("DiscInv.30", "_",amendment ),
    rip  == "none"  &      mix == "none"  & amendment != "none"   ~    paste0("Unmodified", "_",amendment ),
    
    rip  == "rip"       &      mix == "none"        ~    paste0("Rip.", Rip_depth_jax, "_", amendment ),
    rip  == "rip"       &      mix == "inclusion"     ~  paste0("Rip.", Rip_depth_jax, "IncRip",  "_",amendment ),
    rip  == "rip"       &      mix == "spade_inclusion"~ paste0("Rip.", Rip_depth_jax, "IncRip+Spade.30", "_", amendment ),
    rip  == "rip"       &      mix == "spade"     ~      paste0("Rip.", Rip_depth_jax, "Spade.30", "_", amendment ),
    #rip  == "rip"      &      mix == "Plozza"     ~    paste0("Rip.",, Rip_depth_jax, "DiscInv.30", "_", amendment ),
    TRUE ~ as.character("check")
    
  ) )

str(primary_2019_2020_imapct)

primary_2019_2020_imapct <- primary_2019_2020_imapct %>% 
  mutate(Descriptors = case_when(
    site == "Telopea_Downs" & 
      other_ameliorant == "clay" &
      rip  == "none"  & 
      mix == "spade"  ~ paste0("Spade.30", "_", amendment, "@", other_rate), 
    TRUE ~ Descriptors))
      
###################################################################################################################
####  End of  Create the descriptors #####
###################################################################################################################

###################################################################################################################
####  metadata  #####


# metadata_imapct <- read_excel("C:/Users/ouz001/CSIRO/GRDC_CSP00203 - Jackie_working/2020 Impacts_Master data_template_20210222_jaxs_v23_08.xlsx", 
#                                   sheet = "Site_metadata")

metadata_imapct <-read_excel(paste0("C:/Users/ouz001/CSIRO/GRDC_CSP00203 - ", input_data_file), 
           sheet = "Site_metadata", col_types = c("text", 
                                                  "skip", "skip", "skip", "skip", "skip", 
                                                  "skip", "skip", "skip", "skip", "numeric", 
                                                  "numeric", "text", "numeric", "text", 
                                                  "date", "numeric", "text", "text", 
                                                  "text", "text", "date", "date", "date", 
                                                  "date", "date", "date"))

metadata_imapct <- metadata_imapct %>% 
  rename(site  = `Site Name`)

#################################################################################################

#Only keep the impact site data
# create a list of impact sites:
list_sites <- primary_2019_2020_imapct %>% 
  distinct(site)
list_sites  
metadata_imapct <- metadata_imapct %>% 
  filter(site %in% list_sites$site)
################################################################################################
### de identify the sites and change coords ###################################################


### Assign the location based on met station info. remove old coodinates
# metadata_imapct <- metadata_imapct %>% 
#   dplyr::select(-Latitude, -Longitude)
## assign new ones

metadata_imapct <- metadata_imapct %>%
  mutate(
    Latitude = case_when(
      site == "Buckleboo" ~     -33.14,
      site == "Cummins" ~       -34.26,
      site == "Karkoo" ~        -34.13,
      site == "Kooloonong_chickpea" ~    -34.64,
      site == "Kooloonong_lentil" ~      -34.64,
      site == "Kooloonong_lupin" ~       -34.64,
      site == "Kybunga" ~       -33.84,
      site == "Malinong" ~        -35.52,
      site == "Monia_Gap" ~       -33.49,
      site == "Mt_Damper" ~       -32.84,
      site == "Sherwood" ~        -36.10,
      site == "Telopea_Downs" ~   -36.37,
      site == "Tempy" ~           -35.07,
      site == "Warnertown" ~      -33.35,
      site == "Wynarka" ~         -35.09
    )
  )

metadata_imapct <- metadata_imapct %>%
  mutate(
    Longitude = case_when(
      site == "Buckleboo" ~   136.41,
      site == "Cummins" ~     135.73,
      site == "Karkoo" ~      135.73,
      site == "Kooloonong_chickpea" ~  143.56,
      site == "Kooloonong_lentil" ~  143.56,
      site == "Kooloonong_lupin" ~  143.56,
       site == "Kybunga" ~     138.61,
      site == "Malinong" ~      139.51,
      site == "Monia_Gap" ~     145.52,
      site == "Mt_Damper" ~     135.15,
      site == "Sherwood" ~      140.36,
      site == "Telopea_Downs" ~ 140.98,
      site == "Tempy" ~         142.31,
      site == "Warnertown" ~    138.1,
      site == "Wynarka" ~       139.9
    )
  )

#########################################################################################################################
## join metdata to primary data

primary_2019_2020_imapct <- left_join(primary_2019_2020_imapct, metadata_imapct) 
names(primary_2019_2020_imapct)

# primary_2019_2020_imapct <- primary_2019_2020_imapct %>%
#   dplyr::select(
#     -"Site identifiers_SS_#"  ,-"Site identifiers_Region",-"Trial Years_Year_01" ,-"Trial Years_Year_02" ,-"Trial Years_Year_03"
#   )
str(primary_2019_2020_imapct)

## new clm for years post Amelioration
primary_2019_2020_imapct <- primary_2019_2020_imapct %>%
  dplyr::mutate(yr_post_amelioration = year - Amelioration_Year)

### get the sowing date
primary_2019_2020_imapct <- primary_2019_2020_imapct %>%
  mutate (sowing_date = case_when(
    yr_post_amelioration == 0 ~ SowingDate_01,
    yr_post_amelioration == 1 ~ SowingDate_02,
    yr_post_amelioration == 2 ~ SowingDate_03,
  ))

### get the harvest date
primary_2019_2020_imapct <- primary_2019_2020_imapct %>%
  mutate (harvest_date = case_when(
    yr_post_amelioration == 0 ~ Harvest_Date_01,
    yr_post_amelioration == 1 ~ Harvest_Date_02,
    yr_post_amelioration == 2 ~ Harvest_Date_03,
  ))

### get the previous crop
primary_2019_2020_imapct <- primary_2019_2020_imapct %>%
  mutate (previous_crop = case_when(
    yr_post_amelioration == 0 ~ CropType_Prior,
    yr_post_amelioration == 1 ~ CropType_Crop_01,
    yr_post_amelioration == 2 ~ CropType_Crop_02,
  ))

## drop the clm that were used for crop harvest and sowing cals
names(primary_2019_2020_imapct)
primary_2019_2020_imapct <- primary_2019_2020_imapct %>%
  dplyr::select(
    -"CropType_Prior",
    -"CropType_Crop_01",
    -"CropType_Crop_02",
    -"CropType_Crop_03",
    -"SowingDate_01" ,
    -"SowingDate_02" ,
    -"SowingDate_03" ,
    -"Harvest_Date_01" ,
    -"Harvest_Date_02" ,
    -"Harvest_Date_03"
  )
#######################################################################################
### Climate data ########
#######################################################################################

# there is a script that bring in the climate data and processes it.
# select 'Station point datasets' with a APSIM format and start year of 1960.
# before the script can be run the climate data needs to be downloaded, with a certain format.
#X:\Therese_Jackie\Sandy_soils\App_development2021\sandy_soil_tools_app\decile_rainfall_cals.R
# the script produces a csv file that I will use here

GS_rain_deciles_impact_sites <- read_csv(input_data_file_rain, 
                                         col_types = cols(X1 = col_skip()))
unique(GS_rain_deciles_impact_sites$site)
#Kooloonong is listed as Kooloonong_lupins, Kooloonong_chickpea, Kooloonong_lentil
Kooloonong_CP_GS_rain <- GS_rain_deciles_impact_sites %>% 
  dplyr::filter(site == "Kooloonong") %>% 
  mutate(site = "Kooloonong_chickpea")

Kooloonong_Lup_GS_rain <- GS_rain_deciles_impact_sites %>% 
  dplyr::filter(site == "Kooloonong") %>% 
  mutate(site = "Kooloonong_lupin")

Kooloonong_Lentil_GS_rain <- GS_rain_deciles_impact_sites %>% 
  dplyr::filter(site == "Kooloonong") %>% 
  mutate(site = "Kooloonong_lentil")

Kooloonong_GS_rain <- rbind(Kooloonong_Lentil_GS_rain, Kooloonong_Lup_GS_rain, Kooloonong_CP_GS_rain)
rm(Kooloonong_Lentil_GS_rain, Kooloonong_Lup_GS_rain, Kooloonong_CP_GS_rain)

GS_rain_deciles_impact_sites <- rbind(GS_rain_deciles_impact_sites,Kooloonong_GS_rain)
rm(Kooloonong_GS_rain)

GS_rain_deciles_impact_sites <- GS_rain_deciles_impact_sites %>% 
  mutate(site_year =paste0(site,"_", year))%>% 
  dplyr::select(-met_name_year)
str(GS_rain_deciles_impact_sites)


## join to primary data based on year and site
str(primary_2019_2020_imapct)
primary_2019_2020_imapct <- primary_2019_2020_imapct %>% 
  mutate(site_year = paste0(site,"_", year)) 
primary_2019_2020_imapct<-left_join(primary_2019_2020_imapct, GS_rain_deciles_impact_sites)
#############################################################################################



## site information that will be used for location map pop ups
## I need to modify this but at the moment the app is looking for these clms
names(primary_2019_2020_imapct)

primary_site_data <- primary_2019_2020_imapct %>% 
  dplyr::select(site, 
                latitude = Latitude,
                longitude = Longitude,
                'non-wetting' = Repellence,
                acidic = Acidity,
                physical = Physical,
                met_station_number = met_name_number,
                Descriptors,
                Amelioration_Year)

step1_trials_info <- primary_2019_2020_imapct %>% 
  distinct(site, Descriptors)


trial_info <- step1_trials_info %>%
  group_by(site) %>%
  mutate(all_trial = paste(Descriptors, collapse = " & "))
## add to the primary site data
primary_site_data <- left_join(primary_site_data, trial_info) 


# primary_site_data <- primary_2019_2020_imapct %>% 
#   dplyr::select(site, 
#                 latitude = Latitude,
#                 longitude = Longitude,
#                 'non-wetting' = Repellence,
#                 acidic = Acidity,
#                 physical = Physical,
#                 met_station_number = met_name_number)
## add add in some place holders the shiny file is expecting - I will change this later.

# primary_site_data <-primary_site_data %>% 
#   mutate(Region = NA,
#          `trial type` = NA,
#          average_annual_rainfall = NA)
#remove the duplication
primary_site_data <- primary_site_data %>% 
  distinct(site, .keep_all = TRUE) %>% 
  dplyr::select(-Descriptors)
names(primary_site_data)




rm(list=ls()[! ls() %in% c("primary_site_data","primary_2019_2020_imapct")])






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


site_info <- primary_site_data 



#remove sites with no coods
site_info <- 
  filter(site_info, latitude != "NA")
names(site_info)

# site_info <- site_info %>% 
#   mutate( site_label = paste0("Site = ", site),
#           non.wetting_label = paste0("non wetting score = ", "non-wetting"),
#           acidic_label = paste0("acidic score = ", acidic),
#           physical_label = paste0("physical score = ", physical))

 

##############################################################################################################
################## trial results ################################################################################
##############################################################################################################
trial_results <- primary_2019_2020_imapct

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
    Descriptors == "Rip.30_Fert.surface" ~     "deep riping amendment",
    Descriptors == "Rip.50_Cl.surface" ~       "deep riping amendment",
    Descriptors == "Rip.50_Cl.deep" ~          "deep riping amendment",
    
    
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
    grouping == "deep riping amendment" ~ "deep ripping",
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
### for the site info df
names(site_info)
site_info <- site_info %>% 
  rename(Repellence = "non-wetting",
         Acidity = acidic,
         Physical =physical)


site_info$Repellence <- as.double(site_info$Repellence)
site_info$Acidity <- as.double(site_info$Acidity)
site_info$Physical <- as.double(site_info$Physical)

site_info <- site_info %>%
  mutate(non_wetting = case_when(Repellence == 0 ~ "green",
                                 Repellence == 1 ~ "red"))
site_info <- site_info %>%
  mutate(acidic = case_when(Acidity == 0 ~ "green",
                            Acidity == 1 ~ "red",
                            TRUE ~ "check"))
site_info <- site_info %>%
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


#########################################################################################
################## this script writes out two data file #################################

rm(list=ls()[! ls() %in% c("trial_results","site_info")])


## remove the _ in decile

trial_results <- trial_results %>% 
  dplyr::mutate(decile = str_replace_all(decile, "_", " "))


## these files are used in the app one is mainly for trial data
#"X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/App_working/data/primary_data.csv

## one is for the map of trial sites
#X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/App_working/data/site_location_plus_info.csv

write.csv(site_info,
          "X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/App_working/data/site_location_plus_info.csv",
          row.names = FALSE)

#   Write out files to be used in app.
#   all of the data
write.csv(trial_results,"X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/App_working/data/primary_data.csv")



####################################################################################################################################
### cost csv file #################################################################################################################

cost_table <- trial_results %>% 
  dplyr::select(grouping,
                modification,
                site,
                non_wetting,
                acidic,
                physical,
                rainfall_mean_annual,
                site_numb) %>% 
  distinct(site, grouping, .keep_all = TRUE)




## make some new clms for the cost csv file, these values will need to be changed
cost_table <- cost_table %>%
  dplyr::mutate(
    "operating ($/ha)" = 	72,
    "machinery($/ha)"	= 18,
    "labour ($/ha)"	= 16,
    "additional options ($/ha)"	= 10,
    "data source"	= "guess",
    "comments" = NA
  )
cost_table <- cost_table %>%
  dplyr::mutate(
    `additional options ($/ha)`= case_when(
     grouping == "deep riping amendment" ~ 15,
     TRUE                      ~ `additional options ($/ha)`))

### only keep the deep ripping sites
unique(cost_table$grouping)
unique(cost_table$modification)
cost_table <- cost_table %>% 
  filter(modification == "deep ripping")

### arrange this a bit better to match template
cost_table <- cost_table %>% 
  dplyr::select("grouping", "modification", "site",
                "operating ($/ha)" ,        
                "machinery($/ha)",
                "labour ($/ha)" ,
                "additional options ($/ha)",
                "comments" ,
                "data source")


cost_table <- cost_table %>% 
  pivot_longer(cols = c("operating ($/ha)", "machinery($/ha)", "labour ($/ha)" ,  "additional options ($/ha)"),
               names_to = "activity",
               values_to = "price")


cost_table <- cost_table %>% 
  dplyr::select("grouping", "modification", "site",
                "activity",
                "price",
                "comments" ,
                "data source")

write.csv(cost_table,"X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/App_working/data/cost_table.csv")



####################################################################################################################################
### extra table csv file #################################################################################################################


extra_table <- trial_results %>% 
  dplyr::select(grouping,
                modification,
                site,
                non_wetting,
                acidic,
                physical,
                rainfall_mean_annual,
                site_numb) %>% 
  distinct(site, grouping, .keep_all = TRUE)


#add some extra clms ther must be a better way!
extra_table_1 <- extra_table %>% 
     dplyr::mutate(activity  = "additional costs ($/ha)")
extra_table_2 <- extra_table %>% 
  dplyr::mutate(activity  = "additional savings ($/ha)")
extra_table_3 <- extra_table %>% 
  dplyr::mutate(activity  = "cost harvesting and handling extra grain $/t")

extra_table<- rbind(extra_table_1, extra_table_2, extra_table_3)

extra_table_1 <- extra_table %>% 
  dplyr::mutate(year  = 1)
extra_table_2 <- extra_table %>% 
  dplyr::mutate(year  = 2)
extra_table_3 <- extra_table %>% 
  dplyr::mutate(year  = 3)
extra_table_4 <- extra_table %>% 
  dplyr::mutate(year  = 4)
extra_table_5 <- extra_table %>% 
  dplyr::mutate(year  = 5)
extra_table<- rbind(extra_table_1, extra_table_2, extra_table_3, extra_table_4, extra_table_5)


extra_table <- extra_table %>%
  dplyr::mutate(value = 0,
                comments = "nil",
                "data source" = "source")

### only keep the deep ripping sites
unique(extra_table$grouping)
unique(extra_table$modification)
extra_table <- extra_table %>% 
  filter(modification == "deep ripping")

write.csv(extra_table,"X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/App_working/data/extra_table.csv")

rm(extra_table_1,
   extra_table_2,
   extra_table_3,
   extra_table_4,
   extra_table_5)

####################################################################################################################################
### yield table csv file ###########################################################################################################
names(trial_results)
yield_table <- trial_results %>% 
  dplyr::select(grouping,
                modification,
                Descriptors,
                site,
               # plot,#not everything has a plot
                rep_block,  
                year,
                yr_post_amelioration,
                yield,
                crop,
                non_wetting,
                acidic,
                physical,
                rainfall_mean_annual,
                site_numb) 
## for every site and year and descriptor what is the average yld?
str(yield_table)
yield_table$yield <- as.double(yield_table$yield)

#average the yield results per rep for each site Descriptors, and year 
yield_table_av_control <- yield_table %>% 
  filter(Descriptors == "Control") %>% 
  group_by(site, year, crop, Descriptors, grouping, modification, yr_post_amelioration) %>% 
  summarise(`yield  (un modified)` = mean(yield, na.rm = TRUE))

yield_table_av_other <- yield_table %>% 
  filter(Descriptors != "Control") %>% 
  group_by(site, year, crop, Descriptors, grouping, modification, yr_post_amelioration) %>% 
  summarise(`yield (modified)` = mean(yield, na.rm = TRUE))


## add 5 years of yield data 0 to 4 years post amelrioation
## first add the missing years so that all sites have 0 and 1 years post

unique(yield_table_av_control$site) # Telopea_Downs only has year 0
add_row_Telopea_Downs <- yield_table_av_control %>% 
  filter(site == "Telopea_Downs" & yr_post_amelioration ==0)
add_row_Telopea_Downs <- add_row_Telopea_Downs %>% mutate(yr_post_amelioration = 1) 

yield_table_av_other <- rbind(yield_table_av_other, add_row_Telopea_Downs)
yield_table_av_control <- rbind(yield_table_av_control, add_row_Telopea_Downs)

yield_table_av_other <- ungroup(yield_table_av_other) 
yield_table_av_control <- ungroup(yield_table_av_control)

## drop the extra clm I pick up -oops
yield_table_av_other <- dplyr::select(yield_table_av_other, -`yield  (un modified)`)

## add empty row for the years post amelriation 2,3,4 for the control
str(yield_table_av_control)
yield_table_av_control_yr2 <- yield_table_av_control %>%
  filter(yr_post_amelioration == 0) %>%
  mutate(yr_post_amelioration = 2,
         year = NA,
         crop = NA,
         `yield  (un modified)` = NA)  
  
yield_table_av_control_yr3 <- yield_table_av_control %>%
  filter(yr_post_amelioration == 0) %>%
  mutate(yr_post_amelioration = 3,
         year = NA,
         crop = NA,
         `yield  (un modified)` = NA) 

yield_table_av_control_yr4 <- yield_table_av_control %>%
  filter(yr_post_amelioration == 0) %>%
  mutate(yr_post_amelioration = 4,
         year = NA,
         crop = NA,
         `yield  (un modified)` = NA) 

yield_table_av_control <- rbind(yield_table_av_control,
              yield_table_av_control_yr2, 
              yield_table_av_control_yr3, 
              yield_table_av_control_yr4)


## add empty row for the years post amelriation 2,3,4 for the treatments
str(yield_table_av_other)
yield_table_av_other_yr2 <- yield_table_av_other %>%
  filter(yr_post_amelioration == 0) %>%
  mutate(yr_post_amelioration = 2,
         year = NA,
         crop = NA,
         `yield (modified)` = NA)  

yield_table_av_other_yr3 <- yield_table_av_other %>%
  filter(yr_post_amelioration == 0) %>%
  mutate(yr_post_amelioration = 3,
         year = NA,
         crop = NA,
         `yield (modified)` = NA) 

yield_table_av_other_yr4 <- yield_table_av_other %>%
  filter(yr_post_amelioration == 0) %>%
  mutate(yr_post_amelioration = 4,
         year = NA,
         crop = NA,
         `yield (modified)` = NA) 

yield_table_av_other <- rbind(yield_table_av_other,
                                yield_table_av_other_yr2, 
                                yield_table_av_other_yr3, 
                                yield_table_av_other_yr4)

#### now I can join the unmodified and modfied yield togther
str(yield_table_av_other)
str(yield_table_av_control)
#drop a few clms 
yield_table_av_control <- yield_table_av_control %>% 
  dplyr::select(site, yr_post_amelioration, `yield  (un modified)`)
  
yield_table_av <- left_join(yield_table_av_other, yield_table_av_control, 
                  by = c("site", "yr_post_amelioration"))

## add in the price and data source clms # i have got these numbers from pinion decile 5 price
unique(yield_table_av$crop)
yield_table_av <- yield_table_av %>% 
  dplyr:: mutate(`data source` = "trial data",
                 price = case_when(
                   crop == "wheat" ~ 283,
                   crop == "barley" ~ 223,
                   crop == "canola" ~ 545,
                   crop == "chickpea" ~ 1020,
                   crop == "lentil" ~ 580,
                   crop == "lupin" ~ 260,
                   crop == "beans" ~ 440, # Faba beans
                   crop == NA ~ 0))
#tidy up the mess!
rm(yield_table,yield_table_av_control, 
   yield_table_av_control_yr2, 
   yield_table_av_control_yr3, 
   yield_table_av_control_yr4,
   yield_table_av_other,
   yield_table_av_other_yr2,
   yield_table_av_other_yr3,
   yield_table_av_other_yr4,
   add_row_Telopea_Downs)

## lets just keep the ripping data

yield_table_av <- yield_table_av %>% 
  filter(modification == "deep ripping")


## we have a problem with what to average?
#average the yield results per grouping for each site year post amelioration 
str(yield_table_av)
yield_table_av <- yield_table_av %>%
  group_by(site,
           year,
           crop,
           grouping,
           modification,
           yr_post_amelioration,
           `data source`,
           price) %>%
  summarise(
    `yield (modified)` =     mean(`yield (modified)`,     na.rm = TRUE),
    `yield  (un modified)` = mean(`yield  (un modified)`, na.rm = TRUE)
  )


 
yield_table_av$`yield (modified)`[is.nan(yield_table_av$`yield (modified)`)] <- NA
yield_table_av$`yield  (un modified)`[is.nan(yield_table_av$`yield  (un modified)`)] <- NA


write.csv(yield_table_av,"X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/App_working/data/yield_table_av.csv")
