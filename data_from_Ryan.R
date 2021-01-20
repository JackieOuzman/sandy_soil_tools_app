

### load in Ryans data and have a play



library(readxl)
library(readr)
library(dplyr)
library(tidyverse)

primary_data <- read_excel("C:/Users/ouz001/CSIRO/GRDC_CSP00203 - Documents/Data management/data_template_20201221.xlsx", 
                                     sheet = "Primary_data", skip = 1)

site_data <- read_excel("C:/Users/ouz001/CSIRO/GRDC_CSP00203 - Documents/Data management/data_template_20201221.xlsx", 
                           sheet = "Site_metadata", skip = 1)
names(site_data)
# I want to select a few column and turn it into a long df not wide

# test_site <- site_data %>% 
#   select("Site_Name" = "Site Name",
#          "Latitude",
#          "Longitude",
#          "Year_01",
#          "Year_02",
#          "Year_03")
test_site <- rename(site_data ,
                    "Site_Name" = "Site Name")

test_site_longer <- pivot_longer(test_site,
                                 cols = Year_01: Year_03,
                                 names_to ="Year_trial",
                                 values_to = "Year",)


#lets try and add the decile data...

# unique(test_site_longer$Site_Name)
# names(test_site_longer)

test_site_longer <- test_site_longer %>% 
  mutate( Site_Name_yr = paste0(Site_Name, Year))

############################################################################
####################     Rainfall data             #########################
############################################################################

annual_rain_2020 <- read_csv("X:/Therese_Jackie/Sandy_soils/Sands weather/met_file_2020/annual_rain_2020.csv")
### annual rainfall - this is from my calulation - but I could also get this from the BOM??


annual_rain_2020 <- as.data.frame(annual_rain_2020) %>% separate(site, into = paste("V", 1:2, sep = "_"))
annual_rain_2020 <- rename(annual_rain_2020 ,
                           Site_Name = V_1 ,
                           Station_number =V_2   )


### bring in the decile work note that there is a script that runs these cals and then outputs a csv file###
decile_2020 <- read_csv("X:/Therese_Jackie/Sandy_soils/Sands weather/met_file_2020/decile_2020.csv")
## make a site name from the site colum (so I can join Ryns data)

decile_2020 <- as.data.frame(decile_2020) %>% separate(site, into = paste("V", 1:2, sep = "_"))
decile_2020 <- rename(decile_2020 ,
       Site_Name = V_1 ,
       Station_number =V_2   )

# unique(decile_2020$Site_Name)
# unique(annual_rain_2020$Site_Name)


decile_2020 <- left_join(decile_2020, annual_rain_2020, by = "Site_Name")
names(decile_2020)
decile_2020 <- decile_2020 %>% 
  select(-"X1.x"  ,
         -"X1.y",
         -"Station_number.y") %>% 
  rename("Station_number" = "Station_number.x")




## make a site name from the site colum (so I can join Ryns data)
# need to make a new clm with yr and site
decile_2020 <- decile_2020 %>% 
  mutate( Site_Name_yr = paste0(Site_Name, year))

## join the two data sets togther based on Site_Name_yr

test_site_longer <- left_join(test_site_longer, decile_2020, by = "Site_Name_yr" )

names(test_site_longer)
test_site_longer <- test_site_longer %>% 
  select( -"Site_Name.y")%>% 
  #rename("Station_number" = "Station_number.x") %>% 
  rename("Site_Name" = "Site_Name.x")


### The user might select a site and we want to display some info about this site.
#names(test_site_longer)

unique(test_site_longer$Site_Name)

site_selection <- "Karoonda"

test_site_longer <- test_site_longer %>% 
  filter(Site_Name == site_selection)

print(test_site_longer)


### site information ### Not sure how this will be used - as a df, tabel or individual inputs?
#1. what is the site name
#2. what region does it belong to?
#3. what is the average rainfall and station number used?
#4. what are the constaints at this site?


#1.
site_name <- test_site_longer[1,6]
#2.
region <- test_site_longer[1,2]
#3. rainfall details
met_station <- test_site_longer[1,28]
annual_rain <- test_site_longer[1,31]
#4. constaints
soil_constraints <- test_site_longer[1,8:10]
soil_constraints %>% mutate_if(is.double, as.character)

soil_constraints <-soil_constraints %>% str_replace_all( "0", "not an issue")
soil_constraints <-soil_constraints %>% str_replace_all( "1", "moderate")
soil_constraints <-soil_constraints %>% str_replace_all( "1", "severe")
soil_constraints <-as.data.frame(soil_constraints)
soil_constraints <- as.character(soil_constraints$soil_constraints)
soil_constraints_repllency <- soil_constraints[1]
soil_constraints_acidity <- soil_constraints[2]
soil_constraints_physical <- soil_constraints[3]





##### more detailed information
#1. what was implemented at this site?
#2. when was that done?
#3. what years did the trial run for?
#4. what were the yields for the treatment and the control?
#5. plot the results
#6. how do these compare to other site and treatments?


names(primary_data) #"old descriptors - to be removed", "site"

primary_data_site <- primary_data %>% 
  filter(site == site_selection) %>% 
  select(site,
         year,
         crop,
         yield,
         "old descriptors - to be removed") %>% 
  mutate(Site_Name_yr = paste0(site, year))

#join the decile year data
primary_data_site <- left_join(primary_data_site, decile_2020)

#1. what was implemented?

list_treatments <- data.frame(treatmnets = unique(primary_data_site$`old descriptors - to be removed`)) 
list_treatments <- list_treatments %>% 
  filter(treatmnets != "Control")

#2. when was it implemented? Look out for multiple years!!!
implemented <- unique(test_site_longer$Date) # NA not defined in datbase 

#3. what years did the trial run for? and what were the deciles?
#str(test_site_longer)
years          <- unique(test_site_longer$year)
decile_years   <- test_site_longer$decile            
print(decile_years)
years_decile_trial <- data.frame(years = years,
                                 decile= decile_years)  

years_decile_trial <- years_decile_trial %>% 
  filter(years != "NA")

#4. what were the yields for the treatment and the control?
primary_data_site$yield <- as.numeric(primary_data_site$yield)

str(primary_data_site)
ylds <- primary_data_site %>% 
  group_by( `old descriptors - to be removed`, site, year, decile ) %>% 
  summarise(mean_yld = mean(yield, na.rm = TRUE),
            sd_yld = sd(yield))

ylds <- ungroup(ylds)

ylds_control <- primary_data_site %>% 
  filter(`old descriptors - to be removed` ==  "Control") %>% 
  group_by(   year ) %>% 
  summarise(control_mean_yld = mean(yield, na.rm = TRUE))
  
##append this to the other summary yield data (this is so I can plot it with more control)          

ylds <- left_join(ylds,ylds_control, by = "year" )
str(ylds)

##Add a clm to help with the labels on facet wrap
ylds <- ylds %>% 
  mutate(yr_decile = paste0("year ", year, " is ", decile))

subtitle_label = paste0("Trials implemented: ",implemented,
                 "\n 
                 Soil constraints; ",
                 " Repellency = ", soil_constraints_repllency,
                 ", Acidity = ", soil_constraints_acidity,
                 ", Physical = ", soil_constraints_physical)



ggplot(data = ylds, mapping = aes(x = mean_yld , y = `old descriptors - to be removed`,
       fill = ifelse(`old descriptors - to be removed` == "Control", "Control", "others"))) + 
  geom_bar(stat = "identity") +
  theme(legend.position = "none", axis.title.y = element_blank()) +
  coord_flip(expand = FALSE) +
  facet_wrap(.~ yr_decile)+
  theme(axis.text.x = element_text(angle = 90))+
  
  theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))+
        
  labs(title = paste0("Site: ",site_selection, 
                      ". Region: ",region),
               subtitle = subtitle_label ,          
               caption = paste0("Annual rainfall : ",round(annual_rain,0), ". Station number: ", met_station), 
       x = "yield t/ha" ,
       y = "treatments")+
  
  geom_vline(data = ylds, aes(xintercept = control_mean_yld)) #might need to add a filter for year?


