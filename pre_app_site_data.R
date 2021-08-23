## Pre app data ##
# some of this will be replicated in the final product that is on team #



library(shiny)
library(ggplot2)
library(readxl)
library(tidyverse)

primary_2019_imapct <- read_excel("C:/Users/ouz001/CSIRO/GRDC_CSP00203 - Jackie_working/2020 Impacts_Master data_template_20210222_jaxs_v23_08.xlsx", 
                                                                      sheet = "Primary_data_2019", skip=1)
primary_2020_imapct <- read_excel("C:/Users/ouz001/CSIRO/GRDC_CSP00203 - Jackie_working/2020 Impacts_Master data_template_20210222_jaxs_v23_08.xlsx", 
                                  sheet = "Primary_data_2020", skip = 1)
#append the two datasets

primary_2019_2020_imapct <- rbind(primary_2019_imapct,primary_2020_imapct)
rm(primary_2019_imapct, primary_2020_imapct)
str(primary_2019_2020_imapct)
#Create the descriptors
primary_2019_2020_imapct <- primary_2019_2020_imapct %>% 
  mutate(Rip_jax = case_when(
    rip == "none" ~ "unmodified",
    rip == "rip" ~ "Rip"
  ) )
primary_2019_2020_imapct$rip_depth <- as.double(primary_2019_2020_imapct$rip_depth)
primary_2019_2020_imapct <- primary_2019_2020_imapct %>% 
  mutate(Rip_depth_jax = rip_depth/10)

unique(primary_2019_2020_imapct$mix)
primary_2019_2020_imapct <- primary_2019_2020_imapct %>% 
  mutate(mix_jax = case_when(
    mix  == "none" ~            "unmodified",
    mix  == "inclusion" ~       "InRip",
    mix  == "spade" ~           "Spade",
    mix  == "spade_inclusion" ~ "IncRip+Spade",
    mix  == "Plozza" ~           "DiscInv"
  ) )

primary_2019_2020_imapct <- primary_2019_2020_imapct %>% 
  mutate(mix_depth_jax = case_when(
    mix  == "none" ~            "",
    mix  == "inclusion" ~       "",
    mix  == "spade" ~           "30",
    mix  == "spade_inclusion" ~ "30",
    mix  == "Plozza" ~           "30"
  ) )
primary_2019_2020_imapct$mix_depth_jax <- as.double(primary_2019_2020_imapct$mix_depth_jax)
#str(primary_2019_2020_imapct)
