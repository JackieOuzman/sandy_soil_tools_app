


library(ggplot2)
library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)

### Lynne would like the rainfall expressed as decile and the yield trial reuslts to have a matching decile.
# to do this I need to download the daily historial rainfall for all the sites and run some cals.

#"\\FSSA2-ADL\clw-share1\mallee_mod\Therese_Jackie\Sandy_soils\Sands weather\met_file\25006.csv"
#"\\FSSA2-ADL\clw-share1\mallee_mod\Therese_Jackie\Sandy_soils\Sands weather\met_file2021\18014.txt"

# Read in the daily cilmate data.
#met_station_numb <- "Karoonda_25006"
#met_station_numb <- "Waikerie_24018"
#met_station_numb <- "Bute_21012"

# file_name <- "check_Cleve_18014"
# getwd()

# identify the folders for the met files
current.folder <- "X:/Therese_Jackie/Sandy_soils/Sands weather/met_file2021/station_download/"

# find the files that you want
 list.of.files <- list.files(current.folder, ".csv",full.names=T) #the trick is getting the full name
 list.of.files #with path


file_list <- list.files("X:/Therese_Jackie/Sandy_soils/Sands weather/met_file2021/station_download/")
file_list #just names




#for (file_list in file_list){
for (list.of.files in list.of.files){

# day the met file was downloaded

# download_date <- read_csv(paste0("X:/Therese_Jackie/Sandy_soils/Sands weather/met_file2021/", file_name ), 
#                           col_names = FALSE, skip = 7)
download_date <- read_csv(list.of.files, 
                          col_names = FALSE, skip = 7)
download_date <-download_date[1,1] #just the row with the download date
download_date <-stringr::str_extract(download_date, "[[:digit:]]+") #just the numbers
download_date <- as.Date(as.character(download_date),format="%Y%m%d")
#download_date <- as.Date(download_date) -1
#minus one day from download
download_date <- lubridate::ymd(download_date) - days(1)

## station number
# station_number <- read_csv(paste0("X:/Therese_Jackie/Sandy_soils/Sands weather/met_file2021/", file_name), 
#                           col_names = FALSE, skip = 1)
station_number <- read_csv(list.of.files,
                           col_names = FALSE, skip = 1)
station_number <-station_number[1,1] #just the row with the download date
station_number <-stringr::str_extract(station_number, "[[:digit:]]+") #just the numbers
station_number
## station name
# station_name <- read_csv(paste0("X:/Therese_Jackie/Sandy_soils/Sands weather/met_file2021/", file_name), 
#                            col_names = FALSE, skip = 2)
station_name <- read_csv(list.of.files, 
                         col_names = FALSE, skip = 2)
station_name <-station_name[1,1]
station_name <- station_name %>% stringr::str_replace("!station name =", "")
station_name<-str_trim(station_name) #remove the white spaces
station_name



# Cliamte <- read.table(paste0("X:/Therese_Jackie/Sandy_soils/Sands weather/met_file2021/", file_name),
#                              skip = 21, header = TRUE, sep ="")
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

#List of files that have summer rain
sites_for_binding <- ls(pattern="summer")
sites_for_binding
all_sites <- rbind(CLEVE_018014gs_rain_with_summer)

write.csv(all_sites, "X:/Therese_Jackie/Sandy_soils/Sands weather/met_file2021/met_station_decile_year_impact_sites.csv")
                   names(all_sites)

                   

                   
#############################################################################################                   
                   
                   
##### put all the decile data into one file and save

decile_2020 <- rbind(decile_station_numb_Bute_21012, 
                     decile_station_numb_Karoonda_25006,
                     decile_station_numb_Waikerie_24018)


write.csv(decile_2020, "X:/Therese_Jackie/Sandy_soils/Sands weather/met_file_2020/decile_2020.csv")



################################################################################################################################
#### annual rain ####
met_station_numb <- "Waikerie_24018"
#met_station_numb <- "Bute_21012"
#met_station_numb <- "Karoonda_25006"

# day the met file was downloaded
download_date <-  as.Date(today()-2)


Cliamte <- read.table(paste0("X:/Therese_Jackie/Sandy_soils/Sands weather/met_file_2020/", met_station_numb, ".csv"), skip = 21, header = TRUE, sep ="",)


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
         site = met_station_numb)
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


## aveage annual rain 
annual_rain <- sum_year_rain %>% 
  group_by(site) %>% 
  summarise(annual_rain = mean(sum_rain_year))

name <- paste0("annual_rain_", met_station_numb) 

assign(name,annual_rain)




annual_rain_2020 <- rbind(annual_rain_Bute_21012,
                     annual_rain_Karoonda_25006,
                     annual_rain_Waikerie_24018)
                     


write.csv(annual_rain_2020, "X:/Therese_Jackie/Sandy_soils/Sands weather/met_file_2020/annual_rain_2020.csv")
