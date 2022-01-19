library(ggplot2)
library(readxl)
library(tidyverse)
#################################################################################################################
####   Get the data #####

current.folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step1_collating_files/"

# find the files that you want
list.of.files <- list.files(current.folder, ".csv",full.names=T) #the trick is getting the full name - just the excel files
#list.of.files <- list.files(current.folder, full.names=T) #the trick is getting the full name - all the files
list.of.files

#####################################################################################################################
#### Get results and metadata file path

#1 and 2 Bute

# input_data_file <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step1_collating_files/Butes_results.csv"
# input_data_file_metadata <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step1_collating_files/Butes_sites_metadata.csv"
# 
# 
# list.files("X:/Therese_Jackie/Sandy_soils/Sands weather/met_file2021/", ".csv",full.names=T)
# 
# input_data_file_rain <- "X:/Therese_Jackie/Sandy_soils/Sands weather/met_file2021/GS_rain_deciles_Sam_sites.csv"


# 3. Yenda
input_data_file <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step1_collating_files/Yenda_results.csv"
input_data_file_metadata <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step1_collating_files/Yenda_site_metadata.csv"

#####################################################################################################################
#### Get rainfall / climate data file path

list.files("X:/Therese_Jackie/Sandy_soils/Sands weather/met_file2021/", ".csv",full.names=T)

#1 and 2 Bute
#input_data_file_rain <- "X:/Therese_Jackie/Sandy_soils/Sands weather/met_file2021/GS_rain_deciles_Sam_sites.csv"

#3 Yenda
input_data_file_rain <- "X:/Therese_Jackie/Sandy_soils/Sands weather/met_file2021/GS_rain_deciles_Racheal_sites.csv"

##################################################################################################################
## download the data using the specified file path above

primary <- read_csv(input_data_file)

primary <- read_csv(input_data_file, 
                          col_types = cols(drill_depth = col_double(), 
                                           dry_biomass = col_double(), timing = col_character()))

#################################################################################################################
####   Create the descriptors #####

## change units for rip and placement and pre drill to cm (it is a character so have do this the long way!)

str(primary)
unique(primary$rip_depth)
str(primary$rip_depth)
## some sites are a character so now I need to make everything character?
primary$rip_depth <- as.character(primary$rip_depth)

primary<- primary %>% 
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
    
    rip_depth == "500" ~ "50",
    rip_depth == "50 cm" ~ "50",
    rip_depth == "30 cm" ~ "30",
    
    TRUE ~ rip_depth
  ))


unique(primary$drill_depth)

primary<- primary %>% 
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

str(primary)
unique(primary$placement_organic)


primary<- primary %>% 
  mutate(placement_organic = case_when(
    # placement_organic == "300 & 600" ~ "60&30",
    # placement_organic == "80" ~ "8",
    # placement_organic == "300" ~ "30",
    # placement_organic == "800" ~ "80",
    # placement_organic == "600" ~ "60",
    # placement_organic == "75" ~ "7.5",
    # placement_organic == "200" ~ "20",
    
    placement_organic == "incorperated to 8 cm" ~         "incorp_8",
    placement_organic == "incorporated to 8 cm" ~         "incorp_8",
    placement_organic == "incorperated to 50 cm" ~        "incorp_50", 
    placement_organic == "incorporated to 50 cm" ~        "incorp_50",
    placement_organic == "incorporated to 30 cm" ~        "incorp_30", 
    
    placement_organic == "banded at 50 cm" ~              "band_50", 
    placement_organic == "banded at 30 cm" ~              "band_30", 
    
    placement_organic == "surface" ~                      "surface", 
    
    
    
    TRUE ~ placement_organic
  ))

unique(primary$placement_fertiliser)

primary<- primary %>% 
  mutate(placement_fertiliser = case_when(
    # placement_fertiliser == "300 & 600" ~ "60&30",
    # placement_fertiliser == "80" ~ "8",
    # placement_fertiliser == "300" ~ "30",
    # placement_fertiliser == "800" ~ "80",
    # placement_fertiliser == "600" ~ "60",
    # placement_fertiliser == "75" ~ "7.5",
    # placement_fertiliser == "200" ~ "20",
    
    placement_fertiliser == "incorperated to 8 cm" ~         "incorp_8",
    placement_fertiliser == "incorporated to 8 cm" ~         "incorp_8",
    placement_fertiliser == "incorperated to 50 cm" ~        "incorp_50", 
    placement_fertiliser == "incorporated to 50 cm" ~        "incorp_50", 
    
    placement_fertiliser == "banded at 50 cm" ~              "band_50", 
    placement_fertiliser == "banded at 30 cm" ~              "band_30", 
    
    placement_fertiliser == "surface" ~                      "surface", 
    placement_fertiliser == "foliar" ~                       "foliar", 
    
    TRUE ~ placement_fertiliser
  ))


unique(primary$placement_other)

primary<- primary %>% 
  mutate(placement_other = case_when(
    # placement_other == "300 & 600" ~ "60&30",
    # placement_other == "80" ~ "8",
    # placement_other == "300" ~ "30",
    # placement_other == "800" ~ "80",
    # placement_other == "600" ~ "60",
    # placement_other == "75" ~ "7.5",
    # placement_other == "200" ~ "20",
    
    placement_other == "incorperated to 8 cm" ~         "incorp_8",
    placement_other == "incorperated to 50 cm" ~        "incorp_50", 
    
    
    placement_other == "incorporated to 8 cm" ~         "incorp_8",
    placement_other == "incorporated to 50 cm" ~        "incorp_50", 
    placement_other == "incorporated to 30 cm" ~        "incorp_30",
    
    placement_other == "banded at 50 cm" ~              "band_50", 
    placement_other == "banded at 30 cm" ~              "band_30", 
    
    placement_other == "surface" ~                      "surface", 
    
    TRUE ~ placement_other
  ))

#######################################################################################################################

#######################################################################################################################



#make a clm with what ameliorant was added if any

unique(primary$organic)
unique(primary$fertiliser)
unique(primary$other_ameliorant)

primary <- primary %>% 
  mutate(amendment = case_when(
    
    ## cl.with.depth - chicken litter
    organic  == "chicken_compost" | organic  ==  "chicken_manure" | organic  ==  "chicken litter"| organic  ==  "chicken liitter"|organic  == "chicken_litter"
    &  fertiliser ==        "none"  
    &  other_ameliorant ==  "none"    ~        
    paste0("Cl",".", placement_organic),
    
    
    organic  == "chicken_compost" | organic  ==  "chicken_manure" | organic  ==  "chicken litter"| organic  ==  "chicken liitter"
    &  fertiliser ==        "none"  
    &  other_ameliorant ==  "gypsum"       ~   
    paste0("Cl",".", placement_organic,".","gypsum" ,".", placement_other),
    
    
    
    organic  == "chicken_compost" | organic  ==  "chicken_manure" | organic  ==  "chicken litter"| organic  ==  "chicken liitter" 
    &  fertiliser ==        "none"  
    &  other_ameliorant ==  "clay"       ~     
    paste0("Cl",".", placement_organic,".","clay" ,".", placement_other),
    
    
    # This is from Murray - compost
    organic  == "compost"   
    &  other_ameliorant ==  "none"       ~     
    paste0("Com",".", placement_organic),
    
    
    ## Lc.with.depth - lucerne
    organic  ==            "lucerne" 
    &  fertiliser ==        "none" 
    &  other_ameliorant ==  "none"    ~           
    paste0("Lc",".", placement_organic),
    
    
    
    organic  ==            "lucerne"  
    &  fertiliser ==        "none" 
    &  other_ameliorant ==  "gypsum"       ~      
    paste0("Lc",".", placement_organic,".", "gypsum", placement_other),
    
    
    
    organic  ==            "lucerne" 
    &  fertiliser ==        "none" 
    &  other_ameliorant ==  "clay"       ~        
    paste0("Lc",".", placement_organic, ".","clay", ".",  placement_other),
    
  
    
    # This is from Murray - lucerne
    organic  == "pelleted lucerne" 
    &  fertiliser ==        "none" 
    &  other_ameliorant ==  "none"    ~   
    paste0("Lc",".", placement_organic),
    
    
    
    #no amendment
    organic  ==           "none"  
    &  fertiliser ==      "none"   
    &    other_ameliorant ==  "none"    
    ~      "none",
    
    
    #other amendment
    organic  ==                "none"  
    &  fertiliser ==           "none"   
    & other_ameliorant ==      "gypsum"   
    ~ paste0("gypsum",".", placement_other),
    
    

    organic  ==                "none"  
    &  fertiliser ==           "none"   
    &    other_ameliorant ==   "clay"    
    ~ paste0("clay",".", placement_other),
    
    
    
    # This is from Murray - other amendment
    organic  ==           " cereal"  
    & fertiliser ==        "none"   
    & other_ameliorant ==  "none"    
    ~ paste0("cereal",".", placement_organic),
    
    
    organic  ==             "vetch"  
    &  fertiliser ==         "none"   
    &  other_ameliorant ==  "none"    
    ~  paste0("vetch",".", placement_organic),  
    
    
    
    organic  ==               "vetch - cereal"  
    &  fertiliser ==          "none"   
    &    other_ameliorant ==  "none"    
    ~  paste0("vet_cer",".", placement_organic),  
    
    
    
    organic  ==                 "vetch - cereal - innoculant"  
    &  fertiliser ==             "none"   
    &    other_ameliorant ==    "none"   
    ~ paste0("vet_cer_in",".", placement_organic),  
    
    
    
    #fertiliser amendment
    organic  ==                 "none"  
    &  fertiliser ==            "MAP; Urea" |   fertiliser == "MAP" |fertiliser == "Urea"|fertiliser == "Tes"
    &    other_ameliorant ==    "none"    ~      
    paste0("Fert",".", placement_fertiliser),
    
    
    
    organic  ==                 "none"  
    &  fertiliser ==            "MAP; Urea" |   fertiliser == "MAP" |fertiliser == "Urea"|fertiliser == "Tes"
    &    other_ameliorant ==    "gypsum"    ~    
    paste0("Fert",".", placement_fertiliser,".","gypsum",".", placement_other),
    
    
  
    organic  ==                 "none"  
    &  fertiliser ==            "MAP; Urea" |   fertiliser == "MAP" |fertiliser == "Urea"|fertiliser == "Tes"  
    &    other_ameliorant ==    "clay"    ~      
    paste0("Fert",".", placement_fertiliser,".","clay",".", placement_other),
    
    
    organic  == "chicken_compost" | organic  ==  "chicken_manure" | organic  ==  "chicken litter"| organic  ==  "chicken liitter"|organic  == "chicken_litter"
    &  fertiliser ==            "none"  
    &    other_ameliorant ==    "lime"    ~      
    paste0("Lc",".", placement_organic, ".","clay", ".",  placement_other),
    
    organic  ==                "none"  
    &  fertiliser ==           "none"   
    &    other_ameliorant ==   "lime"    
    ~ paste0("lime",".", placement_other),
    
    TRUE ~ as.character("check")
    
  ) )  
    
   

#step 2b make a clm with what disturbance was performed...
str(primary)
unique(primary$rip)
unique(primary$Rip_depth_jax)

unique(primary$mix)


primary <- primary %>% 
  mutate(disturbance  = case_when(
    rip  == "none"      &      mix == "none"           ~      "Unmodified",
    rip  == "none"      &      mix == "spade"          ~       paste0("Spade.30" ),
    rip  == "none"      &      mix == "Plozza"         ~       paste0("DiscInv.30" ),
    rip  == "none"      &      mix == "pre-drill"      ~       paste0("Pre_drill.", drill_depth ),
    rip  == "none"      &      mix == "inclusion"      ~       paste0("Inclusion.50", drill_depth ),#check that this is always 50cm
    rip  == "none"      &      mix == "sweep"      ~           paste0("Sweep.30"),
    
    rip  == "rip"       &      mix == "none"           ~    paste0("Rip.", Rip_depth_jax ),
    rip  == "rip"       &      mix == "inclusion"      ~    paste0("Rip.", Rip_depth_jax, "IncRip" ),
    rip  == "rip"       &      mix == "spade_inclusion"~    paste0("Rip.", Rip_depth_jax, "IncRip+Spade.30" ),
    rip  == "rip"       &      mix == "spade"          ~    paste0("Rip.", Rip_depth_jax, "Spade.30" ),
   
    
    
    TRUE ~ as.character("check")
    
  ) )

#step 2c make a clm with Descriptors which is disturbance _ ameliorant

primary <- primary %>% 
  mutate(Descriptors = paste0(disturbance,"_",amendment))

primary <- primary %>% 
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




primary_metadata <- read_csv(input_data_file_metadata, 
                                 col_types = cols(Amelioration_Date = col_date(format = "%Y-%m-%d"), 
                                                  CropType_Prior = col_character(), 
                                                  CropType_Crop_01 = col_character(), 
                                                  CropType_Crop_02 = col_character(), 
                                                  CropType_Crop_03 = col_character(), 
                                                  CropType_Crop_04 = col_character(), 
                                                  CropType_Crop_05 = col_character(),
                                                  CropType_Crop_06 = col_character(),
                                                  CropType_Crop_07 = col_character(),
                                                  
                                                  Harvest_Date_01 = col_date(format = "%Y-%m-%d"), 
                                                  Harvest_Date_02 = col_date(format = "%Y-%m-%d"), 
                                                  Harvest_Date_03 = col_date(format = "%Y-%m-%d"), 
                                                  Harvest_Date_04 = col_date(format = "%Y-%m-%d"), 
                                                  Harvest_Date_05 = col_date(format = "%Y-%m-%d"), 
                                                  Harvest_Date_06 = col_date(format = "%Y-%m-%d"), 
                                                  Harvest_Date_07 = col_date(format = "%Y-%m-%d"), 
                                                  Longitude = col_double(), PaddockName = col_skip(), 
                                                  `Site identifiers_Grower Name` = col_skip(), 
                                                  `Site identifiers_Region` = col_skip(), 
                                                  `Site identifiers_SS_#` = col_skip(), 
                                                  SowingDate_01 = col_date(format = "%Y-%m-%d"), 
                                                  SowingDate_02 = col_date(format = "%Y-%m-%d"), 
                                                  SowingDate_03 = col_date(format = "%Y-%m-%d"), 
                                                  SowingDate_04 = col_date(format = "%Y-%m-%d"), 
                                                  SowingDate_05 = col_date(format = "%Y-%m-%d"), 
                                                  SowingDate_06 = col_date(format = "%Y-%m-%d"), 
                                                  SowingDate_07 = col_date(format = "%Y-%m-%d"), 
                                                  other = col_character()))




names(primary_metadata)
# primary_metadata <- primary_metadata %>% 
#   dplyr::select(`Site Name`,Repellence:other, 
#                 `Trial Years_Year_01`:`Trial Years_Year_07`,
#                 
#                 
#                 Amelioration_Year)


primary_metadata <- primary_metadata %>% 
  rename(site  = `Site Name`)

#################################################################################################
##-------------------Jax------------------ Do this when I get all the metatdata
#Only keep the impact site data
# create a list of impact sites:

# list_sites <- primary %>% 
#   distinct(site)
# list_sites  
# primary_metadata <- primary_metadata %>% 
#   filter(site %in% list_sites$site)

################################################################################################
### de identify the sites and change coords ###################################################


### Assign the location based on met station info. remove old coodinates

unique(primary_metadata$site)
primary_metadata <- primary_metadata %>%
  mutate(
    Latitude = case_when(
      site == "Carwarp Amelioration" ~        -34.3075, #Red Cliffs is the closest met station
      site == "Waikerie" ~       -34.2159,
      site == "Lowalide" ~       -35.0459,
      site == "Ouyen" ~          -35.0681,
      site == "Bute_Trengrove" ~  -33.8612,
      site == "Bute_CSIRO" ~      -33.8612,
      site == "Yenda" ~           -34.2502
    )
  )
primary_metadata <- primary_metadata %>%
  mutate(
    Longitude = case_when(
      site == "Carwarp Amelioration" ~        142.1882, #Red Cliffs is the closest met station
      site == "Waikerie" ~        140.1860,
      site == "Lowalide" ~        139.9791 ,
      site == "Ouyen" ~           142.3125,
      site == "Bute_Trengrove" ~  138.0114,
      site == "Bute_CSIRO" ~      138.0114,
      site == "Yenda"~            146.1897
    )
  )

unique(primary_metadata$site)
primary_metadata <- primary_metadata %>% 
  mutate(site = case_when(
    site == "Carwarp Amelioration" ~  "Carwarp",
    site == "Lowalide" ~              "Lowaldie",
    # site == "Bute_Trengrove" ~        "Bute", #not sure if this is what I want to do ? ie call both Bute sites Bute 
    # site == "Bute_CSIRO" ~            "Bute",
    
    TRUE ~ site))

unique(primary$site)
unique(primary_metadata$site)

#########################################################################################################################
## join metdata to primary data

primary_join <- left_join(primary, primary_metadata) 
check <- anti_join(primary, primary_metadata)

names(primary_join)
unique(primary_join$Amelioration_Year)


## new clm for years post Amelioration _ this is complicated when we have Amelioration_Year = multiple
#a make Amelioration_Year as interger
primary_join <- primary_join %>%
  dplyr::mutate(Amelioration_Year_value = Amelioration_Year)

primary_join$Amelioration_Year_value <- as.double(primary_join$Amelioration_Year_value)

primary_join <- primary_join %>%
  dplyr::mutate(yr_post_amelioration = year - Amelioration_Year_value)

## if the Amelioration_Year says multiple when was it applied 
unique(primary_join$timing)
unique(primary_join$Amelioration_Year)

# for annual intervention we will report that year
primary_join <- primary_join %>%
  dplyr::mutate(yr_post_amelioration = case_when(
    Amelioration_Year == "multiple" & timing == "annual" ~ 0,
    Amelioration_Year == "multiple" & is.na(timing) ~ year - `Trial Years_Year_01`,
    TRUE ~ yr_post_amelioration)
  )

## I want to chcek that this has worked...
str(primary_join)
unique(primary_join$yr_post_amelioration)                
 
### check
# str(primary_join)
# test <- primary_join %>% 
#   dplyr::select(site:crop  ,Descriptors,timing,`Trial Years_Year_01`,Amelioration_Year, Amelioration_Year_value, yr_post_amelioration )
                
### get the sowing date
primary_join <- primary_join %>%
  mutate (sowing_date = case_when(
    yr_post_amelioration == 0 ~ SowingDate_01,
    yr_post_amelioration == 1 ~ SowingDate_02,
    yr_post_amelioration == 2 ~ SowingDate_03,
    yr_post_amelioration == 3 ~ SowingDate_04,
    yr_post_amelioration == 4 ~ SowingDate_05,
    yr_post_amelioration == 5 ~ SowingDate_06,
    yr_post_amelioration == 6 ~ SowingDate_07,
  ))

### get the harvest date
primary_join <- primary_join %>%
  mutate (harvest_date = case_when(
    yr_post_amelioration == 0 ~ Harvest_Date_01,
    yr_post_amelioration == 1 ~ Harvest_Date_02,
    yr_post_amelioration == 2 ~ Harvest_Date_03,
    yr_post_amelioration == 3 ~ Harvest_Date_04,
    yr_post_amelioration == 5 ~ Harvest_Date_05,
    yr_post_amelioration == 6 ~ Harvest_Date_06,
    yr_post_amelioration == 7 ~ Harvest_Date_07,
  ))

### get the previous crop

primary_join <- primary_join %>%
  mutate (previous_crop = case_when(
    yr_post_amelioration == 0 ~ CropType_Prior,
    yr_post_amelioration == 1 ~ CropType_Crop_01,
    yr_post_amelioration == 2 ~ CropType_Crop_02,
    yr_post_amelioration == 3 ~ CropType_Crop_03,
    yr_post_amelioration == 4 ~ CropType_Crop_04,
    yr_post_amelioration == 5 ~ CropType_Crop_05,
    yr_post_amelioration == 6 ~ CropType_Crop_06,
    yr_post_amelioration == 7 ~ CropType_Crop_07,
  ))

## drop the clm that were used for crop harvest and sowing cals
names(primary_join)
primary_join_1 <- primary_join %>%
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
  
     # -"Site identifiers_SS_#",
     # -"Site identifiers_Region",
     # -"Site identifiers_Grower Name",
     # -"PaddockName" ,

    -"Amelioration_Year_value",

    -"placement_organic",
    -"placement_fertiliser",
    -"placement_other" ,
    - "amendment" ,
    - "disturbance"  ,
 
  
    -"Trial Years_Year_01",
    -"Trial Years_Year_02",
    -"Trial Years_Year_03",
    -"Trial Years_Year_04",
    -"Trial Years_Year_05",
    -"Trial Years_Year_06",
    -"Trial Years_Year_07")

names(primary_join_1)    # #  

#######################################################################################
### Climate data ########
#######################################################################################

# there is a script that bring in the climate data and processes it.
# select 'Station point datasets' with a APSIM format and start year of 1960.
# before the script can be run the climate data needs to be downloaded, with a certain format.
#X:\Therese_Jackie\Sandy_soils\App_development2021\sandy_soil_tools_app\decile_rainfall_cals.R
# the script produces a csv file that I will use here

GS_rain_deciles <- read_csv(input_data_file_rain)
GS_rain_deciles <- dplyr::select(GS_rain_deciles, -'X1')

unique(GS_rain_deciles$site)
## I Carwarp is splet wrong
GS_rain_deciles <- GS_rain_deciles %>% 
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
GS_rain_deciles

GS_rain_deciles <- GS_rain_deciles %>% 
  mutate(site_year =paste0(site,"_", year))
str(GS_rain_deciles)


## join to primary data based on year and site
str(primary_join_1)
# primary_join_1 <- primary_join_1 %>% 
#   mutate(site = "Bute")

primary_join_1 <- primary_join_1 %>% 
  mutate(site_year = paste0(site,"_", year)) 
primary_join_2<-left_join(primary_join_1, GS_rain_deciles)
check <- anti_join(primary_join_1, GS_rain_deciles)#? not sure if I am expecting this to be I think zero







#############################################################################################


### I am using pre_app_site_data as template I worked up to line 280.
## The rest looks like it is used for the app.

#############################################################################################
############                      Write out file                 ##########################
#############################################################################################

# write.csv(primary_join_2,
#           "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2/Bute_sites_step1_2.csv" ,
#           row.names = FALSE)

write.csv(primary_join_2,
          "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2/Yenda_sites_step1_2.csv" ,
          row.names = FALSE)

names(primary_join_2)
primary_neat <- primary_join_2 %>% 
  dplyr::select(ID,
                Descriptors,
                site,
                site_sub,
                year, 
                crop ,
                rip,
                Rip_depth_jax,
                establishment:dry_biomass,
                Latitude, Longitude,
                Amelioration_Year,yr_post_amelioration,
                sowing_date,harvest_date,previous_crop,
                decile,
                plot,
                rep_block,
                timing,
                comments
                )
 
#View(primary_neat)               
# write.csv(primary_neat,
#           "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2/Bute_sites_step1_2_neat.csv" ,
#           row.names = FALSE)               

write.csv(primary_neat,
          "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2/Yenda_sites_step1_2_neat.csv" ,
          row.names = FALSE)  
#############################################################################################
############                     control results only               ##########################
#############################################################################################
primary_control <- primary_join_2 %>% 
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
#str(primary_control)
unique(primary_control$Descriptors)

#we just want the control 

primary_control <- primary_control %>% 
  filter(Descriptors == "Control" )


## Looks like I want to match up the control with the site year and rep_block
## But be careful at Ouyen the site need to use site_sub and same for Bute

#a) make the dataset for control as small as possible and rename the yield biomass and established headings
str(primary_control)

primary_control <- primary_control %>% 
  dplyr::select(site, year, rep_block,site_sub,
                establishment, yield, dry_biomass)

primary_control <- primary_control %>% 
  dplyr::rename(
    control_establishment = establishment,
    control_yield = yield ,
    control_dry_biomass = dry_biomass) %>% 
  mutate(for_join = paste0(site_sub, "_", year,"_", rep_block))

primary_control <- primary_control %>% 
  dplyr::select(for_join, control_establishment, control_yield, control_dry_biomass)

#b) make a join clm 

primary_neat <- primary_neat %>% 
  mutate(for_join = paste0(site_sub, "_", year,"_", rep_block))


##############################################################################################################
primary_with_control <- left_join(primary_neat, primary_control)
#check <- anti_join(primary_neat, primary_control)

#a) remove the controls in the descriptors
unique(primary_with_control$Descriptors)
primary_with_control <- primary_with_control %>% 
  filter(Descriptors != "Control")


# write.csv(primary_with_control,
#           "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2/Bute_sites_step1_2_control.csv" ,
#           row.names = FALSE) 
write.csv(primary_with_control,
          "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2/Yenda_sites_step1_2_control.csv" ,
          row.names = FALSE) 
