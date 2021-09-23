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

##   Write out files to be used in app.
##   all of the data
write.csv(primary_2019_2020_imapct,"X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/App_working/data/primary_data.csv")

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
write.csv(primary_site_data,
          "X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/App_working/data/site_location_plus_info.csv",
          row.names = FALSE)




#########################################################################################
################## this script writes out two data file #################################

## these files are used in the app one is mainly for trial data
#"X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/App_working/data/primary_data.csv

## one is for the map of trial sites
#X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/App_working/data/site_location_plus_info.csv