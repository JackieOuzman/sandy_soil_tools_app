library(ggplot2)
library(readxl)
library(tidyverse)
#################################################################################################################
####   Get the data #####

current.folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/step1_collating_files/"

# find the files that you want
list.of.files <- list.files(current.folder, ".csv",full.names=T) #the trick is getting the full name - just the excel files
#list.of.files <- list.files(current.folder, full.names=T) #the trick is getting the full name - all the files
list.of.files

#####################################################################################################################
#### Get results and metadata file path

#Specify the site

site_name <- "impact"


# using site name

 input_data_file <- paste0("X:/Therese_Jackie/Sandy_soils/Development_database/step1_collating_files/",
 site_name, "_results.csv")
 
 input_data_file_metadata <- paste0("X:/Therese_Jackie/Sandy_soils/Development_database/step1_collating_files/",
 site_name, "_sites_metadata.csv")


#####################################################################################################################
#### Get rainfall / climate data file path

list.files("X:/Therese_Jackie/Sandy_soils/Sands weather/met_file2022/", ".csv",full.names=T)

#impact rainfall 
input_data_file_rain <- "X:/Therese_Jackie/Sandy_soils/Sands weather/met_file2022/GS_rain_deciles_research_impact_sites.csv"



##################################################################################################################
## download the data using the specified file path above

primary <- read_csv(input_data_file, 
                    col_types = cols(drill_depth = col_character(), 
                                     rip_depth = col_character(), 
                                     sowing_strategy = col_character(), 
                                     timing = col_character()))


duplicated_ID <- primary %>% 
  group_by(ID) %>% 
  summarise(count_ID = n()) # I am aiming for a heap of one and nothing else!
  
duplicated_ID <- duplicated_ID %>% 
  filter(count_ID > 1)   # this should be empty 

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
    rip_depth == "350" ~ "35",
    rip_depth == "400" ~ "40",
    rip_depth == "450" ~ "45",
    rip_depth == "500" ~ "50",
    rip_depth == "600" ~ "60",
    rip_depth == "700" ~ "70",
    rip_depth == "800" ~ "80",
    
    rip_depth == "500" ~ "50",
    rip_depth == "50 cm" ~ "50",
    rip_depth == "30 cm" ~ "30",
    
    TRUE ~ rip_depth
  ))
unique(primary$Rip_depth_jax)

unique(primary$drill_depth)
primary$drill_depth <- as.double(primary$drill_depth)

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
primary$placement_organic <- as.character(primary$placement_organic)

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
    placement_organic == "incorporated to 40 cm" ~        "incorp_40",
    placement_organic == "incorporated to 41 cm" ~        "incorp_40", #therese wanted this changed
    placement_organic == "incorporated to 50 cm" ~        "incorp_50",
    placement_organic == "incorporated to 30 cm" ~        "incorp_30",
    placement_organic == "incorporated to 60 cm" ~        "incorp_60",
    
    placement_organic == "banded at 50 cm" ~              "band_50",
    placement_organic == "placed at 50cm" ~               "band_50",
    placement_organic == "banded to 50 cm" ~              "band_50", 
    placement_organic == "banded at 30 cm" ~              "band_30",
    placement_organic == "banded at 60 cm" ~              "band_60",
    placement_organic == "banded to 30+60 cm" ~           "band_30+60",
    
    
    placement_organic == "surface" ~                      "surface", 
    
    
    
    TRUE ~ placement_organic
  ))
unique(primary$placement_organic)

primary$placement_fertiliser <- as.character(primary$placement_fertiliser)
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
    placement_fertiliser == "incorporated to 41 cm" ~        "incorp_41",
    placement_fertiliser == "incorporated to 45 cm" ~        "incorp_45",
    placement_fertiliser == "incorperated to 50 cm" ~        "incorp_50", 
    placement_fertiliser == "incorporated to 50 cm" ~        "incorp_50", 
    placement_fertiliser == "incorporated to 30 cm" ~        "incorp_30",  
    
    placement_fertiliser == "banded at 8 cm" ~               "band_8", 
    placement_fertiliser == "banded at 50 cm" ~              "band_50", 
    placement_fertiliser == "banded at 30 cm" ~              "band_30", 
    placement_fertiliser == "banded at 60 cm" ~              "band_60",
    placement_fertiliser == "banded at 45 cm" ~              "band_45",
    
    placement_fertiliser == "surface" ~                      "surface", 
    placement_fertiliser == "foliar" ~                       "foliar", 
    
    TRUE ~ placement_fertiliser
  ))
unique(primary$placement_fertiliser)

primary$placement_other <- as.character(primary$placement_other)
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
    placement_other == "incorporated to 10 cm" ~         "incorp_10",
    placement_other == "incorporated to 50 cm" ~        "incorp_50", 
    placement_other == "incorporated to 30 cm" ~        "incorp_30",
    placement_other == "incorporate at 30 cm" ~        "incorp_30",
    
    placement_other == "band at 8 cm" ~                 "band_8", 
    placement_other == "banded at 50 cm" ~              "band_50", 
    placement_other == "banded at 30 cm" ~              "band_30",
    placement_other == "banded at 45 cm" ~              "band_45",
    
    
    placement_other == "surface + band at 8 cm" ~       "surface+band_8", 
    
    placement_other == "surface" ~                      "surface", 
    
    TRUE ~ placement_other
  ))
unique(primary$placement_other)
#######################################################################################################################

#######################################################################################################################



#make a clm with what ameliorant was added if any
str(primary)
unique(primary$organic)
unique(primary$fertiliser)
unique(primary$other_ameliorant)
unique(primary$placement_organic)
unique(primary$timing)
unique(primary$site)
unique(primary$site_sub)
##########

## Make a couple of temp clm one for the organic + placement need to specify which sites have rates 
# It seems that  this will work best when the sites are specified for each ifelse statment

primary <- primary %>% 
  mutate(amendment_organic = 
           
           ## chicken litter with rate
           # ifelse(site_sub           %in% c("impact site 1 that needs rates","impact site 2 that needs rates")   #site list 1
           #        & organic          %in% c( "chicken_compost","chicken_manure","chicken litter","chicken liitter",
           #                                    "chicken_litter","chicken litter"),    
           #        paste0("Cl","@", organic_rate,".", placement_organic ), 
             
            
           
            ## chicken litter with NO rate
                  ifelse(site_sub    %in% c("Kooloonong_chickpea","Kooloonong_lentil","Kooloonong_lupin", 
                                            "Tempy",  "Wynarka", "Monia_Gap","Malinong", "Sherwood", "Cummins",
                                             "Karkoo", "Buckleboo", "Mt_Damper", "Kybunga" , "Warnertown", 
                                             "Telopea_Downs","Mt Damper", "Kooloonong_canola" , "Younghusband")  #all of impact sites
                  & organic          %in% c( "chicken_compost","chicken_manure","chicken litter","chicken liitter",
                                              "chicken_litter","chicken litter"),  
                 paste0("Cl",".", placement_organic),
                         
            
            ## compost with NO rate    
                 ifelse(site_sub    %in% c("Kooloonong_chickpea","Kooloonong_lentil","Kooloonong_lupin", 
                                           "Tempy",  "Wynarka", "Monia_Gap","Malinong", "Sherwood", "Cummins",
                                           "Karkoo", "Buckleboo", "Mt_Damper", "Kybunga" , "Warnertown", 
                                           "Telopea_Downs","Mt Damper", "Kooloonong_canola", "Younghusband")  #all the sites
                        & organic          %in% c( "compost"),  
                        paste0("Com",".", placement_organic),
                        
            
            ## lucerne with NO rate    
                        ifelse(site_sub    %in% c("Kooloonong_chickpea","Kooloonong_lentil","Kooloonong_lupin", 
                                                  "Tempy",  "Wynarka", "Monia_Gap","Malinong", "Sherwood", "Cummins",
                                                  "Karkoo", "Buckleboo", "Mt_Damper", "Kybunga" , "Warnertown", 
                                                  "Telopea_Downs","Mt Damper", "Kooloonong_canola")  #set 1
                               & organic          %in% c( "lucerne", "pelleted lucerne"),  
                               paste0("Lc",".", placement_organic),  
            
            # ## lucerne with rate    
            #                    ifelse(site_sub    %in% c("site 1")  #set 2
            #                           & organic          %in% c( "lucerne", "pelleted lucerne"),  
            #                           paste0("Lc","@", organic_rate,".", placement_organic ),    
                 
            
            ## Cereal with NO rate   
                 ifelse(site_sub    %in% c("Kooloonong_chickpea","Kooloonong_lentil","Kooloonong_lupin", 
                                           "Tempy",  "Wynarka", "Monia_Gap","Malinong", "Sherwood", "Cummins",
                                           "Karkoo", "Buckleboo", "Mt_Damper", "Kybunga" , "Warnertown", 
                                           "Telopea_Downs","Mt Damper", "Kooloonong_canola", "Younghusband")  #all the sites
                                & organic %in% c("cereal"),   
                   paste0("Cereal",".", placement_organic),
                                
            ## vetch with NO rate   
                   ifelse(site_sub    %in% c("Kooloonong_chickpea","Kooloonong_lentil","Kooloonong_lupin", 
                                             "Tempy",  "Wynarka", "Monia_Gap","Malinong", "Sherwood", "Cummins",
                                             "Karkoo", "Buckleboo", "Mt_Damper", "Kybunga" , "Warnertown", 
                                             "Telopea_Downs","Mt Damper", "Kooloonong_canola", "Younghusband")  #all the sites
                          & organic %in% c("vetch"),   
                          paste0("Vetch",".", placement_organic),   
                          
           ## vetch - cereal with NO rate   
                          ifelse(site_sub    %in% c("Kooloonong_chickpea","Kooloonong_lentil","Kooloonong_lupin", 
                                                    "Tempy",  "Wynarka", "Monia_Gap","Malinong", "Sherwood", "Cummins",
                                                    "Karkoo", "Buckleboo", "Mt_Damper", "Kybunga" , "Warnertown", 
                                                    "Telopea_Downs","Mt Damper", "Kooloonong_canola", "Younghusband")  #all the sites
                                 & organic %in% c("vetch - cereal"),   
                                 paste0("Vet_Cer",".", placement_organic),
                                 
           ## vetch - cereal with innoculant   
                                 ifelse(site_sub    %in% c("Kooloonong_chickpea","Kooloonong_lentil","Kooloonong_lupin", 
                                                           "Tempy",  "Wynarka", "Monia_Gap","Malinong", "Sherwood", "Cummins",
                                                           "Karkoo", "Buckleboo", "Mt_Damper", "Kybunga" , "Warnertown", 
                                                           "Telopea_Downs","Mt Damper", "Kooloonong_canola", "Younghusband")  #all the sites
                                        & organic %in% c("vetch - cereal - innoculant"),   
                                        paste0("Vet_Cer_In",".", placement_organic),
                                 
                                "other"
                                
                         )))))))#bracket for the number of ifelse statements
  )# bracket for mutate function

### at some sites we may have another clm to use the annual application

primary <- primary %>% 
  
  mutate(amendment_organic = 
           ifelse(site_sub            %in% c("Kooloonong_chickpea","Kooloonong_lentil","Kooloonong_lupin", 
                                             "Tempy",  "Wynarka", "Monia_Gap","Malinong", "Sherwood", "Cummins",
                                             "Karkoo", "Buckleboo", "Mt_Damper", "Kybunga" , "Warnertown", 
                                             "Telopea_Downs","Mt Damper", "Kooloonong_canola", "Younghusband")  #all the sites but not Bute CSIRO
                 &   is.na(timing) ,
                paste0(amendment_organic),
                
                ifelse(site_sub            %in% c("Telopea_Downs")  #just Telopea_Downs
                       &   timing  %in% c("Yrs07, 20"), 
                       paste0(amendment_organic),
           
           
           ## annual
           ifelse(
             site_sub                %in% c("Kooloonong_chickpea","Kooloonong_lentil","Kooloonong_lupin", 
                                            "Tempy",  "Wynarka", "Monia_Gap","Malinong", "Sherwood", "Cummins",
                                            "Karkoo", "Buckleboo", "Mt_Damper", "Kybunga" , "Warnertown", 
                                            "Telopea_Downs","Mt Damper", "Kooloonong_canola")   #site list 1
              &    timing  %in% c("annual","Yrs17,18,19"),    
                  paste0(amendment_organic, "_Yr18,19,20"), 
                 
             
                  "check"
                   
            )))#bracket for the number of ifelse statements
)# bracket for mutate function


primary %>% 
  distinct(amendment_organic) %>% 
  arrange(desc(amendment_organic))




## Temp clm one for the fert + placement  

primary <- primary %>%
  mutate(amendment_fert =
           
           #code for most sites if its a fert it gets descriptor fert
           ifelse(
             site_sub   %in% c(
               "Kooloonong_chickpea",
               "Kooloonong_lentil",
               "Kooloonong_lupin",
               "Tempy",
               "Wynarka",
               "Monia_Gap",
               "Malinong",
               "Sherwood",
               "Cummins",
               "Karkoo",
               #"Buckleboo",# Not 
               "Mt_Damper",
               "Kybunga" ,
               "Warnertown",
               "Telopea_Downs",
               "Mt Damper",
               "Kooloonong_canola",
                "Younghusband"
             )  #all the sites expect Buckleboo
             & fertiliser  %in% c(
               "MAP; Urea",
               "NuPak",
               "MAP",
               "Urea",
               "Tes",
               "DAP, Urea, SOA and Muriate of Potash",
               "Urea and MAP",
               "K"   ,
               "nupak_1",
               "nupak_2",
               "nupak_1",
               "N_P",
               "Nupak_1",
               "Nupak_2"
             ),
             paste0("Fert", ".", placement_fertiliser),
             
             #code for Buckleboo  if its a fert it gets descriptor that describes fert Low
             ifelse(
               site_sub   %in% c(
                 "Buckleboo" )  #just one site for fert type 
               & fertiliser  %in% c( "Nupak_1", "nupak_1"),
             paste0("Fert_Low", ".", placement_fertiliser),
             
             #code for Buckleboo  if its a fert it gets descriptor that describes fert High
             ifelse(
               site_sub   %in% c(
                 "Buckleboo" )  #just one site for fert type 
               & fertiliser  %in% c( "Nupak_2", "nupak_2"),
               paste0("Fert_High", ".", placement_fertiliser),
               
               #code for Buckleboo  if its a fert it gets descriptor that describes fert App
               ifelse(
                 site_sub   %in% c(
                   "Buckleboo" )  #just one site for fert type 
                 & fertiliser  %in% c( "N_P"),
                 paste0("Fert_APP", ".", placement_fertiliser),
             
             "other"
             
           ))))#bracket for the number of ifelse statements
         )# bracket for mutate function
                
                


primary %>% 
  distinct(amendment_fert) %>% 
  arrange(desc(amendment_fert))


## Temp clm one for the other + placement  

primary <- primary %>% 
  mutate(amendment_other = 
           ifelse(other_ameliorant  == "lime",    paste0("Lime",".", placement_other ), 
           ifelse(other_ameliorant  == "gypsum",    paste0("Gypsum",".", placement_other ),      
           ifelse(other_ameliorant  == "clay",    paste0("Clay",".", placement_other ),
           ifelse(other_ameliorant  == "Muriate of Potash",    paste0("K_added",".", placement_other ),
           ifelse(other_ameliorant  == "Bi-Agra",    paste0("Bi_Agra",".", placement_other ),
           ifelse(other_ameliorant  == "SE14",    paste0("SE14",".", placement_other ),
                  "other"
                  
           ))))))#bracket for the number of ifelse statements
  )# bracket for mutate function


# "Telopea_Downs" site has a rate of clay
primary <- primary %>% 
  mutate(amendment_other = 
ifelse(
  site_sub   %in% c("Telopea_Downs" )  #just one site for fert type 
  & amendment_other  == "Clay.incorp_30"
  &   is.na(timing),
  paste0("Clay", "@", other_rate, ".", placement_other),
  
  ifelse(
    site_sub   %in% c("Telopea_Downs" )  #just one site for fert type 
    & amendment_other  == "Clay.incorp_30"
    &   timing  %in% c("Yrs07, 20"),
    paste0("Clay", "@", other_rate, ".", placement_other, "_Yr07,20"),
  
  amendment_other
  
))#bracket for the number of ifelse statements
)# bracket for mutate function




primary %>% 
  distinct(amendment_other) %>% 
  arrange(desc(amendment_other))



#########################################################################################

## Put the three clms togther 
primary %>% 
  distinct(amendment_organic) %>% 
  arrange(desc(amendment_organic))
primary %>% 
  distinct(amendment_fert) %>% 
  arrange(desc(amendment_fert))
primary %>% 
  distinct(amendment_other) %>% 
  arrange(desc(amendment_other))



primary <- primary %>% 
  mutate(amendment = 
           ifelse(amendment_organic  != "other"   
                  & amendment_fert   == "other"    
                  & amendment_other  == "other" ,
                  paste0(amendment_organic), 
                  
                  
                  
            ifelse(amendment_organic         != "other"   
                  & amendment_fert   != "other"    
                  & amendment_other  == "other", 
                 paste0(amendment_organic, ".",amendment_fert ), 
                         
                         
            ifelse(amendment_organic         != "other"   
                  & amendment_fert   != "other"    
                  & amendment_other  != "other", 
                  paste0(amendment_organic, ".",amendment_fert, ".", amendment_other ),
                                
                                
            ifelse(amendment_organic         != "other"   
                  & amendment_fert   == "other"    
                  & amendment_other  != "other", 
                  paste0(amendment_organic,  ".", amendment_other ),  
                                       
                                       
                                       
                                       
            ifelse(amendment_organic         == "other"   
                 & amendment_fert   != "other"    
                 & amendment_other  == "other", 
                 paste0(amendment_fert ), 
                                              
                                              
            ifelse(amendment_organic         == "other"   
            & amendment_fert   != "other"    
            & amendment_other  != "other", 
            paste0(amendment_fert, ".", amendment_other ),
                                                     
                                                     
            ifelse(amendment_organic         == "other"   
            & amendment_fert   == "other"    
            & amendment_other  != "other", 
            paste0(amendment_other ), 
                                                            
                                                            
                                                            
            ifelse(amendment_organic  == "other"   
            & amendment_fert   == "other"    
            & amendment_other  == "other",
            paste0("none"), 
            "check"
))))))))#bracket for the number of ifelse statements
  )# bracket for mutate function






primary %>% 
  distinct(amendment) %>% 
  arrange(desc(amendment))


#step 2b make a clm with what disturbance was performed...
str(primary)
unique(primary$rip)
unique(primary$Rip_depth_jax)

unique(primary$mix)


primary <- primary %>% 
  mutate(disturbance  = case_when(
    rip  == "none"      &      mix == "none"           ~      "Unmodified",
    rip  == "none"      &      mix == "spade"          ~       paste0("Spade.30" ),
    rip  == "none"      &      mix == "sapde"          ~       paste0("Spade.30" ),
    rip  == "none"      &      mix == "Plozza"         ~       paste0("DiscInv.30" ),
    rip  == "none"      &      mix == "pre-drill"      ~       paste0("Pre_drill.", drill_depth ),
    rip  == "none"      &      mix == "inclusion"      ~       paste0("Inc.50" ),#check that this is always 50cm
    rip  == "none"      &      mix == "sweep"      ~           paste0("Sweep.30"),
    rip  == "none"      &      mix == "delving"      ~         paste0("Delving.18"),#check that this is always 18cm it was supplied as a range I used the upper value
    
    rip  == "rip"       &      mix == "none"           ~    paste0("Rip.", Rip_depth_jax ),
    rip  == "rip"       &      mix == "inclusion"      ~    paste0("Rip.", Rip_depth_jax, "IncRip" ),
    rip  == "rip"       &      mix == "spade_inclusion"~    paste0("Rip.", Rip_depth_jax, "IncRip+Spade.30" ),
    rip  == "rip"       &      mix == "spade"          ~    paste0("Rip.", Rip_depth_jax, "Spade.30" ),
   
    
    
    TRUE ~ as.character("check")
    
  ) )


primary %>% 
  distinct(disturbance) %>% 
  arrange(desc(disturbance))

#step 2c make a clm with Descriptors which is disturbance _ ameliorant

primary <- primary %>% 
  mutate(Descriptors = paste0(disturbance,"_",amendment))

primary <- primary %>% 
  mutate(Descriptors = case_when(
    Descriptors == "Unmodified_none"   ~  "Control",
    TRUE ~ as.character(Descriptors)
    
  ) )

## check what Descriptors were made
primary %>% 
  distinct(Descriptors) %>% 
  arrange(desc(Descriptors))




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







primary_metadata <- primary_metadata %>% 
  dplyr::rename(site  = `Site Name`)



################################################################################################
### de identify the sites and change coords ###################################################


### Assign the location based on met station info. remove old coodinates

unique(primary_metadata$site)


primary_metadata <- primary_metadata %>%
  dplyr::mutate(
    Latitude = case_when(
      site == "Carwarp" ~        -34.3075,
      #Red Cliffs is the closest met station Carwarp Amelioration
      site == "Waikerie"         ~ -34.2159,
      site == "Lowalide"         ~ -35.0459,
      site == "Ouyen"            ~ -35.0681,
      site == "Bute_Trengrove"   ~ -33.8612,
      site == "Bute_CSIRO"       ~ -33.8612,
      site == "Yenda"            ~ -34.2502,
      site == "Brooker"          ~ -34.1317,
      site == "Younghusband"     ~ -34.9145,
      site == "Brimpton Lake"    ~ -34.0586,
      site == "Cadgee"           ~ -36.8297,
      site == "Karoonda"         ~ -35.0900,
      site == "Murlong"          ~ -33.6993,
      
      site == "Buckleboo"        ~ -33.1416,
      site == "Karkoo"           ~ -34.1317,
      site == "Cummins"          ~ -34.2644,
      site == "Mt_Damper"        ~ -32.8361,
      site == "Kooloonong_chickpea" ~ -34.6398,
      site == "Kooloonong_lentil" ~ -34.6398,
      site == "Kooloonong_lupin"  ~ -34.6398,
      
      #site =="Taplan"
      site == "Tempy"            ~ -35.0681,
      #site =="Walpeup"
      site == "Wynarka"          ~ -35.09,
      
      #site =="Younghusband"
      site == "Telopea_Downs"    ~ -36.3667,
      site == "Sherwood"         ~ -36.098,
      site == "Malinong"         ~ -35.5217,
      site == "Monia_Gap"        ~ -33.4915,
      site == "Warnertown"       ~ -33.35,
      site == "Kybunga"          ~ -33.8364,
      TRUE ~ as.numeric(Latitude)
    )
  )
     
primary_metadata <- primary_metadata %>%
  dplyr::mutate(
    Longitude = case_when(
      site == "Carwarp"          ~ 142.1882, #Red Cliffs is the closest met station
      site == "Waikerie"        ~ 140.1860,
      site == "Lowalide"        ~ 139.9791 ,
      site == "Ouyen"           ~ 142.3125,
      site == "Bute_Trengrove"  ~ 138.0114,
      site == "Bute_CSIRO"      ~ 138.0114,
      site == "Yenda"           ~ 146.1897,
      site == "Brooker"         ~ 135.7301,
      site == "Younghusband"    ~ 139.3010,
      site == "Brimpton Lake"   ~ 135.5038,
      site == "Cadgee"          ~ 140.5328,
      site == "Karoonda"        ~ 139.8972,
      site == "Murlong"         ~ 135.9238,
      
      site == "Buckleboo"        ~ 136.4126,
      site == "Karkoo"           ~ 135.7301,
      site == "Cummins"          ~ 135.7266,
      site == "Mt_Damper"        ~ 135.15,
      site == "Kooloonong_chickpea" ~ 143.561,
      site == "Kooloonong_lentil"~ 143.561,
      site == "Kooloonong_lupin" ~ 143.561,
      #site =="Taplan"
      site == "Tempy"            ~ 142.3125,
      #site =="Walpeup"
      site == "Wynarka"          ~ 139.8972,
      #site =="Younghusband"
      site == "Telopea_Downs"    ~ 140.9833,
      site == "Sherwood"         ~ 140.3556,
      site == "Malinong"         ~ 139.5133,
      site == "Monia_Gap"        ~ 145.5248,
      site == "Warnertown"       ~ 138.102,
      site == "Kybunga"          ~ 138.6125,
      TRUE ~ as.numeric(Longitude)
    )
  )



unique(primary$site)
unique(primary_metadata$site)

#########################################################################################################################
## join metdata to primary data

primary_join <- left_join(primary, primary_metadata) 
check <- anti_join(primary, primary_metadata)




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
 

### get the sowing date
primary_join <- primary_join %>%
  dplyr::mutate (sowing_date = case_when(
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
  dplyr::mutate (harvest_date = case_when(
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
  dplyr::mutate (previous_crop = case_when(
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


GS_rain_deciles <- GS_rain_deciles %>% 
  dplyr::mutate(site = case_when(
    site == "Brimpton_Lake" ~ "Brimpton Lake",
    site == "Mt_Damper"     ~ "Mt Damper",
    site == "Oyen_Spade"     ~ "Ouyen_Spade",
    
    TRUE ~ site
  ))





GS_rain_deciles <- GS_rain_deciles %>% 
  dplyr::mutate(site_year =paste0(site,"_", year))
str(GS_rain_deciles)


## join to primary data based on year and site
str(primary_join_1)


primary_join_1 <- primary_join_1 %>% 
  dplyr::mutate(site_year = paste0(site,"_", year)) 

primary_join_1 %>%  distinct(site_year)
GS_rain_deciles %>%  distinct(site_year)

primary_join_2<-left_join(primary_join_1, GS_rain_deciles)
check <- anti_join(primary_join_1, GS_rain_deciles)#? not sure if I am expecting this to be I think zero










#############################################################################################
######Write out file but before remove some treatments specific to certain sites################
#############################################################################################





# Remove the spare at Karoonda
primary_join_2 <- primary_join_2 %>%
  dplyr::filter(ID  != "Karoonda_2014_plot_6_rep_block_1") %>%
  dplyr::filter(ID  != "Karoonda_2014_plot_20_rep_block_2") %>%
  dplyr::filter(ID  != "Karoonda_2014_plot_37_rep_block_3") %>%
  dplyr::filter(ID  != "Karoonda_2014_plot_48_rep_block_4") %>%
  dplyr::filter(ID  != "Karoonda_2014_plot_64_rep_block_5") %>% 

  dplyr::filter(ID  != "Karoonda_2015_plot_6_rep_block_1") %>%
  dplyr::filter(ID  != "Karoonda_2015_plot_20_rep_block_2") %>%
  dplyr::filter(ID  != "Karoonda_2015_plot_37_rep_block_3") %>%
  dplyr::filter(ID  != "Karoonda_2015_plot_48_rep_block_4") %>%
  dplyr::filter(ID  != "Karoonda_2015_plot_64_rep_block_5") %>% 

  dplyr::filter(ID  != "Karoonda_2016_plot_6_rep_block_1") %>%
  dplyr::filter(ID  != "Karoonda_2016_plot_20_rep_block_2") %>%
  dplyr::filter(ID  != "Karoonda_2016_plot_37_rep_block_3") %>%
  dplyr::filter(ID  != "Karoonda_2016_plot_48_rep_block_4") %>%
  dplyr::filter(ID  != "Karoonda_2016_plot_64_rep_block_5") %>% 

  dplyr::filter(ID  != "Karoonda_2017_plot_6_rep_block_1") %>%
  dplyr::filter(ID  != "Karoonda_2017_plot_20_rep_block_2") %>%
  dplyr::filter(ID  != "Karoonda_2017_plot_37_rep_block_3") %>%
  dplyr::filter(ID  != "Karoonda_2017_plot_48_rep_block_4") %>%
  dplyr::filter(ID  != "Karoonda_2017_plot_64_rep_block_5") %>% 

  dplyr::filter(ID  != "Karoonda_2018_plot_6_rep_block_1") %>%
  dplyr::filter(ID  != "Karoonda_2018_plot_20_rep_block_2") %>%
  dplyr::filter(ID  != "Karoonda_2018_plot_37_rep_block_3") %>%
  dplyr::filter(ID  != "Karoonda_2018_plot_48_rep_block_4") %>%
  dplyr::filter(ID  != "Karoonda_2018_plot_64_rep_block_5")

# Remove the spare (control b) at Murlong
primary_join_2 <- primary_join_2 %>%
  dplyr::filter(ID  != "Murlong_2018_plot_4_rep_block_1") %>%
  dplyr::filter(ID  != "Murlong_2018_plot_19_rep_block_2") %>%
  dplyr::filter(ID  != "Murlong_2018_plot_28_rep_block_3") %>%
  dplyr::filter(ID  != "Murlong_2018_plot_43_rep_block_4") %>% 

  dplyr::filter(ID  != "Murlong_2019_plot_4_rep_block_1") %>%
  dplyr::filter(ID  != "Murlong_2019_plot_19_rep_block_2") %>%
  dplyr::filter(ID  != "Murlong_2019_plot_28_rep_block_3") %>%
  dplyr::filter(ID  != "Murlong_2019_plot_43_rep_block_4") %>% 
  
  dplyr::filter(ID  != "Murlong_2020_plot_4_rep_block_1") %>%
  dplyr::filter(ID  != "Murlong_2020_plot_19_rep_block_2") %>%
  dplyr::filter(ID  != "Murlong_2020_plot_28_rep_block_3") %>%
  dplyr::filter(ID  != "Murlong_2020_plot_43_rep_block_4") %>% 

  dplyr::filter(ID  != "Murlong_2021_plot_4_rep_block_1") %>%
  dplyr::filter(ID  != "Murlong_2021_plot_19_rep_block_2") %>%
  dplyr::filter(ID  != "Murlong_2021_plot_28_rep_block_3") %>%
  dplyr::filter(ID  != "Murlong_2021_plot_43_rep_block_4")  





## something extra for Waikerie. There is a treatment which has..


# Remove the spare (control b) at Waikerie
primary_join_2 <- primary_join_2 %>%
  dplyr::filter(ID  != "Waikerie_2018_plot_1_rep_block_1") %>%
  dplyr::filter(ID  != "Waikerie_2018_plot_20_rep_block_2") %>%
  dplyr::filter(ID  != "Waikerie_2018_plot_41_rep_block_3") %>%
  dplyr::filter(ID  != "Waikerie_2018_plot_52_rep_block_4") %>%

  dplyr::filter(ID  != "Waikerie_2019_plot_1_rep_block_1") %>%
  dplyr::filter(ID  != "Waikerie_2019_plot_20_rep_block_2") %>%
  dplyr::filter(ID  != "Waikerie_2019_plot_41_rep_block_3") %>%
  dplyr::filter(ID  != "Waikerie_2019_plot_52_rep_block_4") %>%
  
  dplyr::filter(ID  != "Waikerie_2020_plot_1_rep_block_1") %>%
  dplyr::filter(ID  != "Waikerie_2020_plot_20_rep_block_2") %>%
  dplyr::filter(ID  != "Waikerie_2020_plot_41_rep_block_3") %>%
  dplyr::filter(ID  != "Waikerie_2020_plot_52_rep_block_4") 





# New horizons Cadgee also has one to be excluded 2014 plot 10 rep 1
primary <- primary %>% 
  dplyr::filter(Descriptors != "Unmodified_Clay.NA")


# Remove the extra control at Warnertown 

primary_join_2 <- primary_join_2 %>%
  dplyr::filter(ID  != "Warnertown_2021_plot_19_rep_block_4")

site_name

write.csv(primary_join_2,
          paste0("X:/Therese_Jackie/Sandy_soils/Development_database/step2/",site_name, "_sites_step1_2.csv") ,
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
 



duplicated_ID <- primary_neat %>% 
  #group_by(ID, Descriptors) %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarise(count_ID = n()) %>% 
  dplyr::filter(count_ID >1)

duplicated_ID <- primary_neat %>% 
  dplyr::group_by(ID, Descriptors) %>% 
  #group_by(ID) %>% 
  dplyr::summarise(count_ID = n()) %>% 
  filter(count_ID >1)



write.csv(primary_neat,
          paste0("X:/Therese_Jackie/Sandy_soils/Development_database/step2/",site_name, "_sites_step1_2_neat.csv") ,
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
  dplyr::filter(Descriptors == "Control" )


## Looks like I want to match up the control with the site year and rep_block


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
  dplyr::mutate(for_join = paste0(site_sub, "_", year,"_", rep_block))

primary_control <- primary_control %>% 
  dplyr::select(for_join, control_establishment, control_yield, control_dry_biomass)

#b) make a join clm 

primary_neat <- primary_neat %>% 
  dplyr::mutate(for_join = paste0(site_sub, "_", year,"_", rep_block))


##############################################################################################################
primary_with_control <- left_join(primary_neat, primary_control)
#check <- anti_join(primary_neat, primary_control)

#a) remove the controls in the descriptors
unique(primary_with_control$Descriptors)
primary_with_control <- primary_with_control %>% 
  filter(Descriptors != "Control")




write.csv(primary_with_control,
          paste0("X:/Therese_Jackie/Sandy_soils/Development_database/step2/",site_name, "_sites_step1_2_control.csv") ,
          row.names = FALSE)

