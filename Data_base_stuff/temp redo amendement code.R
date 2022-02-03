library(ggplot2)
library(readxl)
library(tidyverse)

input_data_file <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step3_check_etc/test_descriptor_code.csv"

primary <- read_csv(input_data_file)

primary <- read_csv(input_data_file, 
                    col_types = cols(drill_depth = col_double(), 
                                     dry_biomass = col_double(), timing = col_character()))
names(primary)


#make a clm with what ameliorant was added if any
str(primary)
unique(primary$organic)
unique(primary$fertiliser)
unique(primary$other_ameliorant)
unique(primary$placement_organic)
unique(primary$timing)
unique(primary$site)







## Make a couple of temp clm one for the organic + placement need to specify which sites have rates 
# It seems that  this will work best when the sites are specified for each ifelse statment

primary <- primary %>% 
  mutate(amendment_organic = 
           ifelse(site  %in% c("Yenda","Bute_Trengrove")   
                  & organic  == "chicken litter",    paste0("Cl","@", organic_rate,".", placement_organic ), 
                  
           ifelse(site  %in% c("Bute_CSIRO","Ouyen_spade")  
                  
                  & organic  == "chicken litter",   paste0("Cl",".", placement_organic),
                  
            ifelse(site  %in% c("Ouyen_spade")  
                         
                         & organic  == "cereal",   paste0("Cereal",".", placement_organic),
                 
             
                   "other"
                  
                   )))#bracket for the number of ifelse statements
  )# bracket for mutate function

primary %>% 
  distinct(amendment_organic) %>% 
  arrange(desc(amendment_organic))

## Temp clm one for the fert + placement  

primary <- primary %>% 
  mutate(amendment_fert = 
           ifelse(fertiliser  == "MAP; Urea",    paste0("Fert",".", placement_fertiliser ), 
                  
                  
                                "other"
                                
                         )#bracket for the number of ifelse statements
  )# bracket for mutate function

primary %>% 
  distinct(amendment_fert) %>% 
  arrange(desc(amendment_fert))


## Temp clm one for the other + placement  

primary <- primary %>% 
  mutate(amendment_other = 
           ifelse(other_ameliorant  == "lime",    paste0("Lime",".", placement_other ), 
                  
                  
                  "other"
                  
           )#bracket for the number of ifelse statements
  )# bracket for mutate function

primary %>% 
  distinct(amendment_other) %>% 
  arrange(desc(amendment_other))


#########################################################################################

## Put the three clms togther 
primary <- primary %>% 
  mutate(amendment_all =
           paste0(amendment_organic , ".", amendment_fert, ".", amendment_other)
         )
         

primary <- primary %>% 
  mutate(amendment_all = 
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
                                 
                                
