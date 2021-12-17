library(ggplot2)
library(readxl)
library(tidyverse)
#################################################################################################################
####   Get the data #####
input_data_file <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Murrays_sites.csv"

input_data_file_rain <- "X:/Therese_Jackie/Sandy_soils/Sands weather/met_file2021/GS_rain_deciles_Murray_sites.csv"

input_data_file_metadata <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Murrays_sites_metadata.csv"

primary_murray <- read_csv(input_data_file)



#################################################################################################################
####   Create the descriptors #####

## change units for rip and placement and pre drill to cm (it is a character so have do this the long way!)

str(primary_murray)

primary_murray<- primary_murray %>% 
  mutate(Rip_depth_jax = case_when(
    rip_depth == "300 & 600" ~ "60&30",
    rip_depth == "75" ~ "7.5",
    rip_depth == "200" ~ "20",
    rip_depth == "300" ~ "30",
    rip_depth == "400" ~ "40",
    rip_depth == "500" ~ "50",
    rip_depth == "600" ~ "60",
    rip_depth == "700" ~ "70",
    rip_depth == "800" ~ "80",
    
    TRUE ~ rip_depth
  ))


primary_murray<- primary_murray %>% 
  mutate(drill_depth = case_when(
    drill_depth == 75 ~ 7.5,
    drill_depth == 200 ~ 20,
    drill_depth == 300 ~ 30,
    drill_depth == 400 ~ 40,
    drill_depth == 500 ~ 50,
    drill_depth == 600 ~ 60,
    drill_depth == 700 ~ 70,
    drill_depth == 800 ~ 80,
    
    TRUE ~ drill_depth
  ))



primary_murray<- primary_murray %>% 
  mutate(placement = case_when(
    placement == "300 & 600" ~ "60&30",
    placement == "80" ~ "8",
    placement == "300" ~ "30",
    placement == "800" ~ "80",
    placement == "600" ~ "60",
    placement == "75" ~ "7.5",
    placement == "200" ~ "20",
    TRUE ~ placement
  ))

#######################################################################################################################

#######################################################################################################################
# step 1 what depth was amendment applied?

primary_murray <- primary_murray %>% 
  mutate(placement_jax = case_when(
    placement  == "surface"  ~           "surface",
    placement  == "incorporated"  ~      "mix",
    placement  == "deep"  ~              "deep",
    TRUE                  ~            placement)
    )

#unique(primary_murray$other_ameliorant)

#step 2a make a clm with what ameliorant was added if any
primary_murray <- primary_murray %>% 
  mutate(amendment = case_when(
    
    ## cl.with.depth - chicken litter
    organic  == "chicken_compost" | organic  ==  "chicken_manure" &  other_ameliorant ==  "none"    ~        paste0("Cl",".", placement_jax),
    organic  == "chicken_compost" | organic  ==  "chicken_manure" &  other_ameliorant ==  "gypsum"       ~   paste0("Cl.gypsum",".", placement_jax),
    organic  == "chicken_compost" | organic  ==  "chicken_manure" &  other_ameliorant ==  "clay"       ~     paste0("Cl.clay",".", placement_jax),
    
    # This is from Murray - chicken litter
    organic  == "chicken_litter"  &  other_ameliorant ==  "none"    ~        paste0("Cl",".", placement_jax),
    
    # This is from Murray - chicken litter
    organic  == "compost"   &  other_ameliorant ==  "none"       ~     paste0("Com",".", placement_jax),
    
    ## Lc.with.depth - lucerne
    organic  == "lucerne"  &  other_ameliorant ==  "none"    ~           paste0("Lc",".", placement_jax),
    organic  == "lucerne"  &  other_ameliorant ==  "gypsum"       ~      paste0("Lc.gypsum",".", placement_jax),
    organic  == "lucerne"  &  other_ameliorant ==  "clay"       ~        paste0("Lc.clay",".", placement_jax),
    
    # This is from Murray - lucerne
    organic  == "pelleted lucerne"  &  other_ameliorant ==  "none"    ~   paste0("Lc",".", placement_jax),
    
    #no amendment
    organic  == "none"  &  fertiliser == "none"   &    other_ameliorant ==  "none"    ~      "none",
    
    #other amendment
    organic  == "none"  &  fertiliser == "none"   &    other_ameliorant ==  "gypsum"    ~    paste0("gypsum",".", placement_jax),
    organic  == "none"  &  fertiliser == "none"   &    other_ameliorant ==  "clay"    ~      paste0("clay"),
    
    # This is from Murray - other amendment
    organic  == "none"  &  fertiliser == "none"   &    other_ameliorant ==  "cereal"    ~    paste0("cereal",".", placement_jax),
    organic  == "none"  &  fertiliser == "none"   &    other_ameliorant ==  "vetch"    ~     paste0("vetch",".", placement_jax),
    organic  == "none"  &  fertiliser == "none"   &    other_ameliorant ==  "vetch - cereal"    ~     paste0("vet_cer",".", placement_jax),
    organic  == "none"  &  fertiliser == "none"   &    other_ameliorant ==  "vetch - cereal - innoculant"    ~     paste0("vet_cer_in",".", placement_jax),
    
    #fertiliser amendment
    organic  == "none"  &  fertiliser != "none"   &    other_ameliorant ==  "none"    ~      paste0("Fert",".", placement_jax),
    organic  == "none"  &  fertiliser != "none"   &    other_ameliorant ==  "gypsum"    ~    paste0("Fert.gypsum",".", placement_jax),
    organic  == "none"  &  fertiliser != "none"   &    other_ameliorant ==  "clay"    ~      paste0("Fert.clay",".", placement_jax),
    
    
    
    TRUE ~ as.character("check")
    
  ) )

#step 2b make a clm with what disturbance was performed
str(primary_murray)

primary_murray <- primary_murray %>% 
  mutate(disturbance  = case_when(
    rip  == "none"      &      mix == "none"           ~      "Unmodified",
    rip  == "none"      &      mix == "spade"          ~       paste0("Spade.30" ),
    rip  == "none"      &      mix == "Plozza"         ~       paste0("DiscInv.30" ),
    rip  == "none"      &      mix == "pre-drill"      ~       paste0("pre_drill.", drill_depth ),
    
    
    rip  == "rip"       &      mix == "none"           ~    paste0("Rip.", Rip_depth_jax ),
    rip  == "rip"       &      mix == "inclusion"      ~    paste0("Rip.", Rip_depth_jax, "IncRip" ),
    rip  == "rip"       &      mix == "spade_inclusion"~    paste0("Rip.", Rip_depth_jax, "IncRip+Spade.30" ),
    rip  == "rip"       &      mix == "spade"          ~    paste0("Rip.", Rip_depth_jax, "Spade.30" ),
   
    
    
    TRUE ~ as.character("check")
    
  ) )

#step 2c make a clm with Descriptors which is disturbance _ ameliorant

primary_murray <- primary_murray %>% 
  mutate(Descriptors = paste0(disturbance,"_",amendment))

primary_murray <- primary_murray %>% 
  mutate(Descriptors = case_when(
    Descriptors == "Unmodified_none"   ~  "Control",
    TRUE ~ as.character(Descriptors)
    
  ) )
### something for Mel data
# primary_2019_2020_imapct <- primary_2019_2020_imapct %>% 
#   mutate(Descriptors = case_when(
#     site == "Telopea_Downs" & 
#       other_ameliorant == "clay" &
#       rip  == "none"  & 
#       mix == "spade"  ~ paste0("Spade.30", "_", amendment, "@", other_rate), 
#     TRUE ~ Descriptors))


###################################################################################################################
####  End of  Create the descriptors #####
###################################################################################################################

###################################################################################################################
####  metadata  #####

primary_murray_metadata <- read_csv(input_data_file_metadata)

primary_murray_metadata <- primary_murray_metadata %>% 
  rename(site  = `Site Name`)

#################################################################################################
##-------------------Jax------------------ Do this when I get all the metatdata
#Only keep the impact site data
# create a list of impact sites:

# list_sites <- primary_murray %>% 
#   distinct(site)
# list_sites  
# primary_murray_metadata <- primary_murray_metadata %>% 
#   filter(site %in% list_sites$site)

################################################################################################
### de identify the sites and change coords ###################################################


### Assign the location based on met station info. remove old coodinates

unique(primary_murray_metadata$site)
primary_murray_metadata <- primary_murray_metadata %>%
  mutate(
    Latitude = case_when(
      site == "Carwarp Amelioration" ~        -34.3075, #Red Cliffs is the closest met station
      site == "Waikerie" ~       -34.2159,
      site == "Lowalide" ~       -35.0459,
      site == "Ouyen" ~          -35.0681 
    )
  )
primary_murray_metadata <- primary_murray_metadata %>%
  mutate(
    Longitude = case_when(
      site == "Carwarp Amelioration" ~        142.1882, #Red Cliffs is the closest met station
      site == "Waikerie" ~       140.1860,
      site == "Lowalide" ~       139.9791 ,
      site == "Ouyen" ~          142.3125
    )
  )

unique(primary_murray_metadata$site)
primary_murray_metadata <- primary_murray_metadata %>% 
  mutate(site = case_when(
    site == "Carwarp Amelioration" ~  "Carwarp",
    site == "Lowalide" ~  "Lowaldie",
    TRUE ~ site))

unique(primary_murray$site)
unique(primary_murray_metadata$site)

#########################################################################################################################
## join metdata to primary data

primary_murray_join <- left_join(primary_murray, primary_murray_metadata) 
check <- anti_join(primary_murray, primary_murray_metadata)

names(primary_murray_join)
unique(primary_murray_join$Amelioration_Year)


## new clm for years post Amelioration _ this is complicated when we have Amelioration_Year = multiple
#a make Amelioration_Year as interger
primary_murray_join <- primary_murray_join %>%
  dplyr::mutate(Amelioration_Year_value = Amelioration_Year)

primary_murray_join$Amelioration_Year_value <- as.double(primary_murray_join$Amelioration_Year_value)

primary_murray_join <- primary_murray_join %>%
  dplyr::mutate(yr_post_amelioration = year - Amelioration_Year_value)

## if the Amelioration_Year says multiple when was it applied 
unique(primary_murray_join$timing)
unique(primary_murray_join$Amelioration_Year)

# for annual intervention we will report that year
primary_murray_join <- primary_murray_join %>%
  dplyr::mutate(yr_post_amelioration = case_when(
    Amelioration_Year == "multiple" & timing == "annual" ~ 0,
    Amelioration_Year == "multiple" & is.na(timing) ~ year - `Trial Years_Year_01`,
    TRUE ~ yr_post_amelioration)
  )

## I want to chcek that this has worked...
str(primary_murray_join)
unique(primary_murray_join$yr_post_amelioration)                
 
### check
# str(primary_murray_join)
# test <- primary_murray_join %>% 
#   dplyr::select(site:crop  ,Descriptors,timing,`Trial Years_Year_01`,Amelioration_Year, Amelioration_Year_value, yr_post_amelioration )
                
### get the sowing date
primary_murray_join <- primary_murray_join %>%
  mutate (sowing_date = case_when(
    yr_post_amelioration == 0 ~ SowingDate_01,
    yr_post_amelioration == 1 ~ SowingDate_02,
    yr_post_amelioration == 2 ~ SowingDate_03,
    yr_post_amelioration == 3 ~ SowingDate_04,
  ))

### get the harvest date
primary_murray_join <- primary_murray_join %>%
  mutate (harvest_date = case_when(
    yr_post_amelioration == 0 ~ Harvest_Date_01,
    yr_post_amelioration == 1 ~ Harvest_Date_02,
    yr_post_amelioration == 2 ~ Harvest_Date_03,
    yr_post_amelioration == 3 ~ Harvest_Date_04,
  ))

### get the previous crop
primary_murray_join <- primary_murray_join %>%
  mutate (previous_crop = case_when(
    yr_post_amelioration == 0 ~ CropType_Prior,
    yr_post_amelioration == 1 ~ CropType_Crop_01,
    yr_post_amelioration == 2 ~ CropType_Crop_02,
    yr_post_amelioration == 3 ~ CropType_Crop_03,
  ))

## drop the clm that were used for crop harvest and sowing cals
names(primary_murray_join_1)
primary_murray_join_1 <- primary_murray_join %>%
  dplyr::select(
    -"CropType_Prior",
    -"CropType_Crop_01",
    -"CropType_Crop_02",
    -"CropType_Crop_03",
    -"CropType_Crop_04",
    -"CropType_Crop_05",
    -"CropType_Crop_06",
    -"CropType_Crop_07",

    -"SowingDate_01" ,
    -"SowingDate_02" ,
    -"SowingDate_03" ,
    -"SowingDate_04" ,
    -"SowingDate_05" ,
    -"SowingDate_06" ,
    -"SowingDate_07" ,


    -"Harvest_Date_01" ,
    -"Harvest_Date_02" ,
    -"Harvest_Date_03",
    -"Harvest_Date_04" ,
    -"Harvest_Date_05" ,
    -"Harvest_Date_06",
    -"Harvest_Date_07",
  
     -"Site identifiers_SS_#",
     -"Site identifiers_Region",
     -"Site identifiers_Grower Name",
     -"PaddockName" ,

    -"Amelioration_Year_value",

    -"Rip_depth_jax" ,
    -"placement_jax" ,
    - "amendment" ,
    - "disturbance"  ,
 
  
    -"Trial Years_Year_01",
    -"Trial Years_Year_02",
    -"Trial Years_Year_03",
    -"Trial Years_Year_04",
    -"Trial Years_Year_05",
    -"Trial Years_Year_06",
    -"Trial Years_Year_07")

names(primary_murray_join_1)    # #  

#######################################################################################
### Climate data ########
#######################################################################################

# there is a script that bring in the climate data and processes it.
# select 'Station point datasets' with a APSIM format and start year of 1960.
# before the script can be run the climate data needs to be downloaded, with a certain format.
#X:\Therese_Jackie\Sandy_soils\App_development2021\sandy_soil_tools_app\decile_rainfall_cals.R
# the script produces a csv file that I will use here

GS_rain_deciles_murray_sites <- read_csv(input_data_file_rain)
GS_rain_deciles_murray_sites <- dplyr::select(GS_rain_deciles_murray_sites, -'X1')

unique(GS_rain_deciles_murray_sites$site)
## I Carwarp is splet wrong
GS_rain_deciles_murray_sites <- GS_rain_deciles_murray_sites %>% 
  mutate(site = case_when(
    site == "Carwap" ~ "Carwarp",
    TRUE ~site
  ))

#Kooloonong is listed as Kooloonong_lupins, Kooloonong_chickpea, Kooloonong_lentil
# Kooloonong_CP_GS_rain <- GS_rain_deciles_impact_sites %>% 
#   dplyr::filter(site == "Kooloonong") %>% 
#   mutate(site = "Kooloonong_chickpea")
# 
# Kooloonong_Lup_GS_rain <- GS_rain_deciles_impact_sites %>% 
#   dplyr::filter(site == "Kooloonong") %>% 
#   mutate(site = "Kooloonong_lupin")
# 
# Kooloonong_Lentil_GS_rain <- GS_rain_deciles_impact_sites %>% 
#   dplyr::filter(site == "Kooloonong") %>% 
#   mutate(site = "Kooloonong_lentil")
# 
# Kooloonong_GS_rain <- rbind(Kooloonong_Lentil_GS_rain, Kooloonong_Lup_GS_rain, Kooloonong_CP_GS_rain)
# rm(Kooloonong_Lentil_GS_rain, Kooloonong_Lup_GS_rain, Kooloonong_CP_GS_rain)
# 
# GS_rain_deciles_impact_sites <- rbind(GS_rain_deciles_impact_sites,Kooloonong_GS_rain)
# rm(Kooloonong_GS_rain)

GS_rain_deciles_murray_sites <- GS_rain_deciles_murray_sites %>% 
  mutate(site_year =paste0(site,"_", year))
str(GS_rain_deciles_murray_sites)


## join to primary data based on year and site
str(primary_murray_join_1)
primary_murray_join_1 <- primary_murray_join_1 %>% 
  mutate(site_year = paste0(site,"_", year)) 
primary_murray_join_2<-left_join(primary_murray_join_1, GS_rain_deciles_murray_sites)
#check <- anti_join(primary_murray_join_1, GS_rain_deciles_murray_sites)







#############################################################################################


### I am using pre_app_site_data as template I worked up to line 280.
## The rest looks like it is used for the app.

#############################################################################################
############                      Write out file                 ##########################
#############################################################################################

write.csv(primary_murray_join_2,
          "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Murrays_sites_step1_2.csv" ,
          row.names = FALSE)

names(primary_murray_join_2)
primary_murray_neat <- primary_murray_join_2 %>% 
  dplyr::select(ID:year, 
                site_sub,
                plot,
                rep_block,
                crop ,
                rip,rip_depth,
                timing,
                Descriptors,
                establishment:dry_biomass,
                Latitude, Longitude,
                Amelioration_Year,yr_post_amelioration,
                sowing_date,harvest_date,previous_crop,
                decile)
 
#View(primary_murray_neat)               
write.csv(primary_murray_neat,
          "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Murrays_sites_step1_2_neat.csv" ,
          row.names = FALSE)               


#############################################################################################
############                     control results only               ##########################
#############################################################################################
primary_murray_control <- primary_murray_join_2 %>% 
  dplyr::select(ID:crop ,
                site_sub,
                plot,
                rep_block,
                rip,rip_depth,
                timing,
                Descriptors,
                establishment:dry_biomass,
                Latitude, Longitude,
                Amelioration_Year,yr_post_amelioration,
                sowing_date,harvest_date,previous_crop,
                decile)
#str(primary_murray_control)
unique(primary_murray_control$Descriptors)

#we just want the control 

primary_murray_control <- primary_murray_control %>% 
  filter(Descriptors == "Control" )


## Looks like I want to match up the control with the site year and rep_block
## But be careful at Ouyen the site need to use site_sub

#a) make the dataset for control as small as possible and rename the yield biomass and established headings
str(primary_murray_control)

primary_murray_control <- primary_murray_control %>% 
  dplyr::select(site, year, rep_block,site_sub,
                establishment, yield, dry_biomass)

primary_murray_control <- primary_murray_control %>% 
  dplyr::rename(
    control_establishment = establishment,
    control_yield = yield ,
    control_dry_biomass = dry_biomass) %>% 
  mutate(for_join = paste0(site_sub, "_", year,"_", rep_block))

primary_murray_control <- primary_murray_control %>% 
  dplyr::select(for_join, control_establishment, control_yield, control_dry_biomass)

#b) make a join clm 

primary_murray_neat <- primary_murray_neat %>% 
  mutate(for_join = paste0(site_sub, "_", year,"_", rep_block))


##############################################################################################################
primary_murray_with_control <- left_join(primary_murray_neat, primary_murray_control)
#check <- anti_join(primary_murray_neat, primary_murray_control)

#a) remove the controls in the descriptors
unique(primary_murray_with_control$Descriptors)
primary_murray_with_control <- primary_murray_with_control %>% 
  filter(Descriptors != "Control")


write.csv(primary_murray_with_control,
          "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/Murrays_sites_step1_2_control.csv" ,
          row.names = FALSE) 

