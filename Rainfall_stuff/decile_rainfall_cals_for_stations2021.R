


library(ggplot2)
library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)

###########################################################################################
###          Background   ####
### Rainfall expressed as decile and the yield trial reuslts to have a matching decile.
#https://www.longpaddock.qld.gov.au/silo/point-data/

# Station point datasets
# Station point datasets are a time series of data at a station location, consisting of station records which have been supplemented by interpolated estimates when observed data are missing. 
# Station point datasets are available at approximately 8,000 station locations around Australia. These datasets were formerly known as SILO Patched Point datasets.
# Grid point datasets
# Grid point datasets are a time series of data at a grid point location consisting entirely of interpolated estimates. The data are taken from our gridded datasets and are available at any grid point over the land area of Australia (including some islands). The nominal grid location (where the interpolated surface is evaluated) is the centre of the corresponding grid cell. These datasets were formerly known as SILO Data Drill datasets.
# The data are taken from our gridded datasets and are available at any grid point over the land area of Australia (including some islands). 
# The nominal grid location (where the interpolated surface is evaluated) is the centre of the corresponding grid cell. These datasets were formerly known as SILO Data Drill datasets.
# "How are gridded rainfall datasets created?
#  "
# Daily rainfall gridded datasets are derived from interpolated monthly rainfall by partitioning the monthly total onto individual days. Partitioning requires estimation of the daily distribution throughout the month. 
# The distribution is obtained by direct interpolation of daily rainfall data throughout the month. At the end of the month, the interpolated monthly rainfall is then partitioned onto individual days according to the computed distribution.


## This script will automate the calulation of decile year ##
## point it to the folder the point data set has been downloaded
## I have selected a APSIM format starting with 1/1/1960 and saved as a .csv file


########################################################################################
# identify the folders for the met files
current.folder <- "X:/Therese_Jackie/Sandy_soils/Sands weather/met_file2021/station_download/"

# find the files that you want
list.of.files <- list.files(current.folder, ".csv",full.names=T) #the trick is getting the full name
list.of.files #with path
########################################################################################

########################################################################################
## decile years for rainfal ##
########################################################################################

for (list.of.files in list.of.files){

# day the met file was downloaded


download_date <- read_csv(list.of.files, 
                          col_names = FALSE, skip = 7)
download_date <-download_date[1,1] #just the row with the download date
download_date <-stringr::str_extract(download_date, "[[:digit:]]+") #just the numbers
download_date <- as.Date(as.character(download_date),format="%Y%m%d")

#minus one day from download
download_date <- lubridate::ymd(download_date) - days(1)

## station number
station_number <- read_csv(list.of.files,
                           col_names = FALSE, skip = 1)
station_number <-station_number[1,1] #just the row with the download date
station_number <-stringr::str_extract(station_number, "[[:digit:]]+") #just the numbers

## station name
station_name <- read_csv(list.of.files, 
                         col_names = FALSE, skip = 2)
station_name <-station_name[1,1]
station_name <- station_name %>% stringr::str_replace("!station name =", "")
station_name<-str_trim(station_name) #remove the white spaces

### Download all of the cliamte data
Cliamte <- read.table(list.of.files, 
                      skip = 21, header = TRUE, sep ="")
Cliamte <- Cliamte [-1,]

### need to make a clm that has all the dates not just day of year and year
str(Cliamte)
Cliamte$rain <- as.character(Cliamte$rain)
Cliamte$rain <- as.double(Cliamte$rain)

#create a date clm # note you need to be aweare of the last day of downlaoded met data

#date range


Cliamte <- Cliamte %>% 
  mutate(date = seq(as.Date("1960/1/1"), download_date, "days"))
# set date as a date
Cliamte <- Cliamte %>% 
  mutate(year = year(date),
         month =month(date),
         day = day(date),
         month_name = lubridate::month(date, label = TRUE),
         site = paste0(station_name,"_", station_number))
str(Cliamte)

#how much rain for each month and year
Summary_rain_month_yr <- Cliamte %>% 
  group_by(year, month, month_name, site) %>% 
  summarise(sum_rain_month = sum(rain))
str(Summary_rain_month_yr)
Summary_rain_month_yr <- ungroup(Summary_rain_month_yr)

# define summer month and GS months
Summary_rain_month_yr <- Summary_rain_month_yr %>% 
  mutate(season = case_when(
    month >= 1 & month <= 3 ~ "summer" ,
    month >= 11 & month <= 12 ~ "summer" ,
    month >= 4 & month <= 10 ~ "gs" ,
    TRUE ~ "no_class"
  ))

summer_GS_rain <- Summary_rain_month_yr %>% 
  group_by(year, season, site) %>% 
  summarise(season_rain = sum(sum_rain_month))

## for each year what is the sume of GS + 0.25 summer rain



str(summer_GS_rain)
summer_GS_rain <- ungroup(summer_GS_rain)

summer <- summer_GS_rain  %>% 
  filter(season == "summer") %>% 
  mutate(summer_rain0.25 = season_rain * 0.25) %>% 
  select(year,summer_rain0.25 )
gs <- summer_GS_rain  %>% 
  filter(season == "gs") %>%  
select(year,gs_rain =season_rain, site )

gs_rain_with_summer <- left_join(gs, summer)

gs_rain_with_summer <- gs_rain_with_summer %>% 
  mutate(rain = gs_rain+ summer_rain0.25 ) %>% 
  select(year, site, rain)

#remove the unwated files
rm(Cliamte, gs, summer, Summary_rain_month_yr, summer_GS_rain)

#the percentiles for each year.

decile_max_rain <- quantile(gs_rain_with_summer$rain, c(.1, .2, .3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
deciles_names <- c("decile_1", "decile_2", "decile_3", "decile_4", "decile_5", "decile_6", "decile_7", "decile_8", "decile_9", "decile_10")

decile_table <- data.frame(decile_max_rain, deciles_names, row.names = NULL)
decile_table <- mutate(decile_table, decile_min_rain = lag(decile_max_rain+0.1))


#add in the min value to table
decile_table[1,3] <-  min(gs_rain_with_summer$rain)
decile_table <- decile_table %>%  select (deciles_names, decile_min_rain, decile_max_rain)





#################### look up type table  #################################################################


str(gs_rain_with_summer$rain)
decile_table[1,2]
decile_table[1,3]

gs_rain_with_summer <- gs_rain_with_summer %>%
    mutate(
      decile = case_when(
      between(round(rain, 2), round(decile_table[1,2],2) ,round(decile_table[1,3],2) ) ~ "decile_1",
      between(round(rain, 2), round(decile_table[2,2],2) ,round(decile_table[2,3],2) ) ~ "decile_2",
      between(round(rain, 2), round(decile_table[3,2],2) ,round(decile_table[3,3],2) ) ~ "decile_3",
      between(round(rain, 2), round(decile_table[4,2],2) ,round(decile_table[4,3],2) ) ~ "decile_4",
      between(round(rain, 2), round(decile_table[5,2],2) ,round(decile_table[5,3],2) ) ~ "decile_5",
      between(round(rain, 2), round(decile_table[6,2],2) ,round(decile_table[6,3],2) ) ~ "decile_6",
      between(round(rain, 2), round(decile_table[7,2],2) ,round(decile_table[7,3],2) ) ~ "decile_7",
      between(round(rain, 2), round(decile_table[8,2],2) ,round(decile_table[8,3],2) ) ~ "decile_8",
      between(round(rain, 2), round(decile_table[9,2],2) ,round(decile_table[9,3],2) ) ~ "decile_9",
      between(round(rain, 2), round(decile_table[10,2],2) ,round(decile_table[10,3],2) ) ~ "decile_10",
      
      TRUE                      ~ "other"
    )
  )

## save as met station number

#name <- paste0("decile_station_numb_", met_station_numb) 
name <- paste0(station_name,"_", station_number, "gs_rain_with_summer")
assign(name,gs_rain_with_summer)


rm(decile_table, gs_rain_with_summer)

}


####################################################################################
## end of function bring all the df into one ###

#List of files that have summer rain - this should be better!

sites_for_binding <- ls(pattern="summer")
print(sites_for_binding)
head()
# GS_rain_deciles <- rbind(
#   `BALRANALD (RSL)_049002gs_rain_with_summer`,
#   `CLARE POST OFFICE_021014gs_rain_with_summer`,
#   `COOMANDOOK (MALINONG)_025508gs_rain_with_summer`,
#   `CUMMINS_018023gs_rain_with_summer`,
#   `HILLSTON AIRPORT_075032gs_rain_with_summer`,
#   `KAROONDA_025006gs_rain_with_summer`,
#   `KEITH_025507gs_rain_with_summer`,
#   `KIMBA_018040gs_rain_with_summer`,
#   `MINNIPA AGRICULTURAL CENTRE_018052gs_rain_with_summer`,
#   `NUROM (RIVERSIDE FARM)_021102gs_rain_with_summer`,
#   `OUYEN (POST OFFICE)_076047gs_rain_with_summer`,
#   `SERVICETON_078034gs_rain_with_summer`,
#   `YEELANNA_018099gs_rain_with_summer`
# )

#############################################################################################################
############### These are Murrays site ######################################################################
#############################################################################################################
# `OUYEN (POST OFFICE)_076047gs_rain_with_summer`
# `LOWALDIE_025039gs_rain_with_summer
# `WAIKERIE (EREMOPHILA PARK)_024029gs_rain_with_summer`
# `RED CLIFFS (POST OFFICE)_076052gs_rain_with_summer`

# GS_rain_deciles <- rbind(
#   `OUYEN (POST OFFICE)_076047gs_rain_with_summer`,
#   `LOWALDIE_025039gs_rain_with_summer`,
#   `WAIKERIE (EREMOPHILA PARK)_024029gs_rain_with_summer`,
#   `RED CLIFFS (POST OFFICE)_076052gs_rain_with_summer`
# )

#############################################################################################################
############### These are Mels / Nigel  site ######################################################################
#############################################################################################################
# GS_rain_deciles <- rbind(
#   YEELANNA_018099gs_rain_with_summer,
#   `MURDINGA (MUNGALA)_018164gs_rain_with_summer`
# )

#############################################################################################################
############### This are Racheal  site ######################################################################
#############################################################################################################
GS_rain_deciles <- rbind(
  `YENDA (HENRY STREET)_075079gs_rain_with_summer`
)


#################################################################################################################
names(GS_rain_deciles)
GS_rain_deciles <- GS_rain_deciles %>% 
  rename( met_name_number = site )
unique(GS_rain_deciles$met_name_number)

# GS_rain_deciles <- GS_rain_deciles %>% 
#   mutate(site = case_when(
#     met_name_number == "BALRANALD (RSL)_049002" ~ "Kooloonong",
#     met_name_number == "KIMBA_018040" ~ "Buckleboo",
#     met_name_number == "CUMMINS_018023" ~ "Cummins",
#     met_name_number == "YEELANNA_018099" ~ "Karkoo",
#     met_name_number == "CLARE POST OFFICE_021014" ~ "Kybunga",
#     met_name_number == "COOMANDOOK (MALINONG)_025508" ~ "Malinong",
#     met_name_number == "HILLSTON AIRPORT_075032" ~ "Monia_Gap",
#     met_name_number == "MINNIPA AGRICULTURAL CENTRE_018052" ~ "Mt_Damper",
#     met_name_number == "KEITH_025507" ~ "Sherwood",
#     met_name_number == "SERVICETON_078034" ~ "Telopea_Downs",
#     met_name_number == "OUYEN (POST OFFICE)_076047" ~ "Tempy",
#     met_name_number == "NUROM (RIVERSIDE FARM)_021102" ~ "Warnertown",
#     met_name_number == "KAROONDA_025006" ~ "Wynarka",
#     TRUE ~ as.character("check")
#   ))

# GS_rain_deciles <- GS_rain_deciles %>% 
#   mutate(site = case_when(
#     met_name_number == "OUYEN (POST OFFICE)_076047" ~ "Ouyen",
#     met_name_number == "LOWALDIE_025039" ~ "Lowaldie",
#     met_name_number == "WAIKERIE (EREMOPHILA PARK)_024029" ~ "Waikerie",
#     met_name_number == "RED CLIFFS (POST OFFICE)_076052" ~ "Carwap",
#     TRUE ~ as.character("check")
#   ))

# GS_rain_deciles <- GS_rain_deciles %>% 
#   mutate(site = case_when(
#     met_name_number == "YEELANNA_018099" ~ "Brooker",
#     met_name_number == "MURDINGA (MUNGALA)_018164" ~ "Murlong",
#     TRUE ~ as.character("check")
#   ))

GS_rain_deciles <- GS_rain_deciles %>% 
  mutate(site = case_when(
    met_name_number == "YENDA (HENRY STREET)_075079" ~ "Yenda",
      TRUE ~ as.character("check")
  ))

write.csv(GS_rain_deciles, "X:/Therese_Jackie/Sandy_soils/Sands weather/met_file2021/GS_rain_deciles_Racheal_sites.csv")
                    

                   

                   
#############################################################################################                   
     
################################################################################################################################
########################################################################################
## annual rainfal ##
########################################################################################
list.of.files                   

# find the files that you want
list.of.files <- list.files(current.folder, ".csv",full.names=T) #the trick is getting the full name
list.of.files #with path


for (list.of.files in list.of.files){
  
download_date <- read_csv(list.of.files, 
                            col_names = FALSE, skip = 7)
download_date <-download_date[1,1] #just the row with the download date
download_date <-stringr::str_extract(download_date, "[[:digit:]]+") #just the numbers
download_date <- as.Date(as.character(download_date),format="%Y%m%d")
  
  #minus one day from download
download_date <- lubridate::ymd(download_date) - days(1)
  
  ## station number
station_number <- read_csv(list.of.files,
                             col_names = FALSE, skip = 1)
station_number <-station_number[1,1] #just the row with the download date
station_number <-stringr::str_extract(station_number, "[[:digit:]]+") #just the numbers
  
  ## station name
station_name <- read_csv(list.of.files, 
                           col_names = FALSE, skip = 2)
station_name <-station_name[1,1]
station_name <- station_name %>% stringr::str_replace("!station name =", "")
station_name<-str_trim(station_name) #remove the white spaces
  
  ### Download all of the cliamte data
Cliamte <- read.table(list.of.files, 
                        skip = 21, header = TRUE, sep ="")
Cliamte <- Cliamte [-1,]
  

### need to make a clm that has all the dates not just day of year and year
str(Cliamte)
Cliamte$rain <- as.character(Cliamte$rain)
Cliamte$rain <- as.double(Cliamte$rain)
#create a date clm # note you need to be aweare of the last day of downlaoded met data

Cliamte <- Cliamte %>% 
  mutate(date = seq(as.Date("1960/1/1"), download_date, "days"))
# set date as a date
Cliamte <- Cliamte %>% 
  mutate(year = year(date),
         month =month(date),
         day = day(date),
         month_name = lubridate::month(date, label = TRUE),
         site = paste0(station_name,"_", station_number))
str(Cliamte)

sum_month_rain_yr <- Cliamte %>% 
  group_by(site, year, month) %>% 
  summarise(sum_rain_month = sum(rain, na.rm = TRUE))
#remove incomplete year 2021

sum_month_rain_yr <- sum_month_rain_yr %>% 
  filter( year != 2021) 

## sum for each month

sum_year_rain <- sum_month_rain_yr %>% 
  group_by(site, year) %>% 
  summarise(sum_rain_year = sum(sum_rain_month))


## average annual rain 
annual_rain <- sum_year_rain %>% 
  group_by(site) %>% 
  summarise(annual_rain = mean(sum_rain_year))

name_annual_rain <- paste0(station_name,"_", station_number, "annual_rain")

assign(name_annual_rain,annual_rain)

}




 # annual_rain_2020 <- rbind(`OUYEN (POST OFFICE)_076047annual_rain`,
 #                           `LOWALDIE_025039annual_rain`,
 #                           `WAIKERIE (EREMOPHILA PARK)_024029annual_rain`,
 #                           `RED CLIFFS (POST OFFICE)_076052annual_rain`)

# annual_rain_2020 <- rbind(YEELANNA_018099annual_rain,
#                           `MURDINGA (MUNGALA)_018164annual_rain`)

annual_rain_2020 <- rbind(`YENDA (HENRY STREET)_075079annual_rain`)

 write.csv(annual_rain_2020, "X:/Therese_Jackie/Sandy_soils/Sands weather/met_file2021/annual_rain_2021_Racheal_sites.csv")
 #"X:\Therese_Jackie\Sandy_soils\Sands weather\met_file2021"