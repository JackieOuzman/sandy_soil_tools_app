

library(ggplot2)
library(readxl)
library(tidyverse)

### step 1 geeting the DB files into a format that the app will like:)

# These are the output files I am wanting to create
#1.  \\FSSA2-ADL\clw-share1\mallee_mod\Therese_Jackie\Sandy_soils\App_development2021\sandy_soil_tools_app\sandy_soil_tools_app\site_location_plus_info_v2.csv
#2.  \\FSSA2-ADL\clw-share1\mallee_mod\Therese_Jackie\Sandy_soils\App_development2021\sandy_soil_tools_app\sandy_soil_tools_app\primary_data_all_v2.csv 


#1. is the metdata and I have done this manually for now - but I will need to change this to automatc and run off the DB files
#2. is the DB trial results but there are a few extra column that I may or may not need. I will try and create them here so the app will run.
# these are grouping, modification (control ripping etc), 	non_wetting,	acidic,	physical,	rainfall_mean_annual,	site_numb. 


#2
DB_df <- read.csv("X:/Therese_Jackie/Sandy_soils/Development_database/completeDB/sites_merged_all_messy.csv")

