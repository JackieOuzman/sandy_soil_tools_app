library(ggplot2)
library(readxl)
library(tidyverse)
library(multcompView)
library(scales)
library(dplyr)
library(FSA) 
library(agricolae)
library(multcomp)
library(lsmeans)
library(Rmisc)
library(car)
library(DescTools)


# am not running this as a function but as a four loop that runs through the list

#sites <- read_csv("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/sites_merged.csv",
#                               col_types = cols(rep_block = col_character()))
# #
# list_of_sites <- sites %>%  distinct(site) %>% dplyr::arrange(site)
# tail(list_of_sites,15)
# # ## what years do I have?
# a <-  "Younghusband"
#  sites_names <- sites %>%   filter(site == a) %>% distinct(year)
#  min(sites_names)
#  max(sites_names)

 
 ## test 
# site_yrs_list <- c("BucklebooX2019",
#                    "BucklebooX2020",
#                    "BucklebooX2021")
 

### List of sites I want to run analysis for:
site_yrs_list <- c(
                   "Brimpton LakeX2014",
                   "Brimpton LakeX2015",
                   "Brimpton LakeX2016",
                   "Brimpton LakeX2017",
                   "Brimpton LakeX2018",
                   
                   "BrookerX2019",
                   "BrookerX2020",
                   "BrookerX2021",

                   "BucklebooX2019",
                   "BucklebooX2020",
                   "BucklebooX2021",

                   "Bute_CSIROX2018",
                   "Bute_CSIROX2019",
                   "Bute_CSIROX2020",
                   "Bute_CSIROX2021",

                   "Bute_TrengroveX2015",
                   "Bute_TrengroveX2016",
                   "Bute_TrengroveX2017",
                   "Bute_TrengroveX2018",
                   "Bute_TrengroveX2019",
                   "Bute_TrengroveX2020",
                   "Bute_TrengroveX2021",

                   "CadgeeX2014",
                   "CadgeeX2015",
                   #"CadgeeX2016", #no yield data
                   "CadgeeX2017",
                   "CadgeeX2018",

                   "Carwarp_AmeliorationX2018",
                   "Carwarp_AmeliorationX2019",
                   "Carwarp_AmeliorationX2020",

                   "CumminsX2019",
                   "CumminsX2020",
                   "CumminsX2021",

                   "KarkooX2019",
                   "KarkooX2020",
                   "KarkooX2021",

                   "KaroondaX2014",
                   "KaroondaX2015",
                   "KaroondaX2016",
                   "KaroondaX2017",
                   "KaroondaX2018",

                   "Kooloonong_canolaX2021",

                   "Kooloonong_chickpeaX2019",
                   "Kooloonong_chickpeaX2020",
                   "Kooloonong_chickpeaX2021",

                   "Kooloonong_lentilX2019",
                   "Kooloonong_lentilX2020",
                   "Kooloonong_lentilX2021",

                   "Kooloonong_lupinX2019",
                   "Kooloonong_lupinX2020",
                   "Kooloonong_lupinX2021",

                   "KybungaX2019",
                   "KybungaX2020",
                   "KybungaX2021",

                   "Lowaldie_CrestX2019",
                   "Lowaldie_CrestX2020",
                   #"Lowaldie_CrestX2021", #no data 

                   "Lowaldie_Deep sandX2019",
                   "Lowaldie_Deep sandX2020",
                   #"Lowaldie_Deep sandX2021",#no data

                   "MalinongX2019",
                   "MalinongX2020",
                   "MalinongX2021",

                   "Monia_GapX2019",
                   "Monia_GapX2020",
                   "Monia_GapX2021",

                   "Mt DamperX2019",
                   "Mt DamperX2020",
                   "Mt DamperX2021", # 20 sites
                   
                   "MurlongX2018",
                   "MurlongX2019",
                   "MurlongX2020",
                   "MurlongX2021",
                   
                   "Ouyen_SpadeX2017",
                   "Ouyen_SpadeX2018",
                   "Ouyen_SpadeX2019",
                   "Ouyen_SpadeX2020",
                   
                   "SherwoodX2019",
                   "SherwoodX2020",
                   "SherwoodX2021",
                   
                   "Telopea_DownsX2020",
                   "Telopea_DownsX2021",
                   
                   "TempyX2019",
                   "TempyX2020",
                   #"TempyX2021", #no yield
                   
                   "WaikerieX2018",
                   "WaikerieX2019",
                   "WaikerieX2020",
                   
                   "WarnertownX2019",
                   "WarnertownX2020",
                   "WarnertownX2021",
                   
                   "WynarkaX2019",
                   "WynarkaX2020",
                   "WynarkaX2021",
                   
                   "YendaX2017",
                   "YendaX2018",
                   "YendaX2019",
                   "YendaX2020",
                   "YendaX2021",
                   
                   "YounghusbandX2020"
                   )



#######################################################################################################################################################
##########                                                    As a loop                                                                     ##########
#######################################################################################################################################################



for (site_yrs_list in site_yrs_list){

  
##########################################
  site_and_yr <- as.data.frame(str_split(site_yrs_list, "X"),
                               col.names = "site_and_yr" )
  
  site_and_yr$site_and_yr <- as.character(site_and_yr$site_and_yr)
  site_and_yr
  a <- site_and_yr[1,1]
  b <- site_and_yr[2,1]
  b <- as.double(b)

######################################
# bring in the data
data_file <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/sites_merged.csv"
summary_data_all_1 <- read_csv(data_file, 
                               col_types = cols(rep_block = col_character()))


### brooker is a problem site I want to filter out these ones:

summary_data_all_1 <- summary_data_all_1 %>%
  filter(Descriptors  != "Spade.30_Lc@1.incorp_30") %>%
  filter(Descriptors  != "Spade.30_Lc@1.incorp_30.K_added.surface") %>%
  filter(Descriptors  != "Spade.30_Lc@2.incorp_30") %>%
  filter(Descriptors  != "Spade.30_Lc@2.incorp_30.K_added.surface") %>%
  filter(Descriptors  != "Spade.30_Lc@6.incorp_30") %>%
  filter(Descriptors  != "Spade.30_Lc@6.incorp_30.K_added.surface") %>%
  filter(Descriptors  != "Spade.30_Lc@10.incorp_30") %>%
  filter(Descriptors  != "Spade.30_Lc@10.incorp_30.K_added.surface") %>%
  filter(Descriptors  != "Spade.30_Lc@20.incorp_30") %>% 
  filter(Descriptors  != "Spade.30_Lc@20.incorp_30.K_added.surface")





##### order the Descriptors
order <- c(
  "Control",
  "Unmodified_SE14.band_8",
  "Unmodified_Bi_Agra.surface+band_8",
  "Unmodified_Lc.surface",
  "Unmodified_Cl.surface",
  "Unmodified_Cl@2.5.surface_Yr18,19,20",
  "Unmodified_Cl@3.incorp_8",
  "Unmodified_Cl@5.incorp_8",
  "Unmodified_Cl@5.incorp_8.Fert.surface",
  "Unmodified_Cl@5.incorp_8.Clay.incorp_8",
  "Unmodified_Cl@5.incorp_8.Fert.surface.Clay.incorp_8",
  "Unmodified_Cl@7.5.surface",
  "Unmodified_Cl@20.incorp_8",
  "Unmodified_Cl@20.incorp_8.Fert.surface",
  "Unmodified_Cl@20.incorp_8.Clay.incorp_8",
  "Unmodified_Cl@20.incorp_8.Fert.surface.Clay.incorp_8",
  "Unmodified_Fert.foliar",
  "Unmodified_Fert.surface",
  "Unmodified_Fert.incorp_8",
  "Unmodified_Fert.band_8",
  "Unmodified_K_added.surface",
  "Unmodified_Fert.band_30",
  "Unmodified_Fert.surface.Clay.incorp_8",
  "Unmodified_Fert.band_30.Clay.incorp_10",
  "Unmodified_Clay.check",
  "Unmodified_Clay.incorp_8",
  "Unmodified_Clay.incorp_10",
  
  "Spade.30_none",
  "Spade.30_Lc@1.incorp_30",
  "Spade.30_Lc@2.incorp_30",
  "Spade.30_Lc@4.incorp_30",
  "Spade.30_Lc@6.incorp_30",
  "Spade.30_Lc@8.incorp_30",
  "Spade.30_Lc@15.incorp_30",
  "Spade.30_Lc@10.incorp_30",
  "Spade.30_Lc@20.incorp_30",
  "Spade.30_Lc@1.incorp_30.K_added.surface",
  "Spade.30_Lc@2.incorp_30.K_added.surface",
  "Spade.30_Lc@4.incorp_30.K_added.surface",
  "Spade.30_Lc@6.incorp_30.K_added.surface",
  "Spade.30_Lc@8.incorp_30.K_added.surface",
  "Spade.30_Lc@10.incorp_30.K_added.surface",
  "Spade.30_Lc@15.incorp_30.K_added.surface",
  "Spade.30_Lc@20.incorp_30.K_added.surface",
  "Spade.30_Lc.incorp_30",
  "Spade.30_Lc.incorp_30.Fert.incorp_30",
  "Spade.30_Lc.incorp_30.Clay.incorp_30",
  "Spade.30_Lc.incorp_30.Fert.incorp_30.Clay.incorp_30",
  "Spade.30_Cl.incorp_30",
  "Spade.30_Cl.incorp_30.Gypsum.incorp_30",
  "Spade.30_Fert.incorp_30.Clay.incorp_30",
  "Spade.30_Com.incorp_30",
  "Spade.30_Cereal.incorp_30",
  "Spade.30_Vetch.incorp_30",
  "Spade.30_Vet_Cer.incorp_30",
  "Spade.30_Vet_Cer_In.incorp_30",
  "Spade.30_K_added.surface",
  "Spade.30_Fert.incorp_30",
  "Spade.30_Fert.incorp_30.K_added.incorp_30",
  
  "Spade.30_Clay@250.incorp_30",
  "Spade.30_Clay@500.incorp_30_Yr07,20",
  "Spade.30_Clay.check",
  "Spade.30_Clay.incorp_30",
  "Spade.30_Gypsum.incorp_30",
  
  "Rip.30_none",
  "Rip.35_none",
  "Rip.30_Cl.surface",
  "Rip.30_Cl@7.5.surface",
  "Rip.30_Cl.band_30",
  "Rip.30_Cl@7.5.band_30",
  "Rip.30_Lc.incorp_30",
  "Rip.30_Lc.band_30",
  "Rip.30_Fert.surface",
  "Rip.30_Fert.incorp_30",
  "Rip.30_Fert.band_8",
  "Rip.30_Fert.band_30",
  "Rip.30IncRip_none",
  "Rip.30+60_none",
  "Rip.30IncRip_Gypsum.incorp_30",
  "Rip.30+60_Lc.band_30+60",
  
  "Sweep.30_none",
  "Sweep.30_Lime.incorp_30",
  "Sweep.30_Cl@9.incorp_30",
  "Sweep.30_Cl@6.incorp_30_Yr18,19,20",
  "Sweep.30_Cl@6.incorp_30",
  "Sweep.30_Cl@3.incorp_30.Lime.incorp_8",
  "Sweep.30_Cl@3.incorp_30",
  
  "Rip.40_none",
  "Rip.40IncRip_none",
  "Rip.40IncRip_Lc.incorp_30",
  "Rip.40IncRip_Lc.incorp_40",
  
  
  "Rip.40_Lc.incorp_40",
  "Rip.40_Fert.incorp_40",
  
  "Rip.45_none",
  "Rip.45IncRip_none",
  "Rip.45IncRip+Spade.30_none",
  "Rip.45IncRip_Fert.incorp_45",
  "Rip.45IncRip_Fert_Low.band_45",
  "Rip.45IncRip_Fert_High.band_45",
  "Rip.45IncRip_Fert_APP.band_45",
  
  "Rip.50_none",
  "Rip.50_Cl.surface",
  "Rip.50_Cl@2.5.surface_Yr18,19,20",
  "Rip.50_Cl@7.5.surface",
  "Rip.50_Cl@5.incorp_20",#changed
  "Rip.50_Cl@7.5.band_50",
  "Rip.50_Cl@20.incorp_20",#changed
  "Rip.50_Cl.deep",
  "Rip.50_Cl.band_50",
  "Rip.50_Cl@5.incorp_20.Fert.surface", #changed
  "Rip.50_Cl@5.incorp_20.Clay.incorp_20",#changed
  "Rip.50_Cl@5.incorp_20.Fert.surface.Clay.incorp_20",#changed
  "Rip.50_Cl@20.incorp_20.Fert.surface", #changed
  "Rip.50_Cl@20.incorp_20.Clay.incorp_20",#changed
  "Rip.50_Cl@20.incorp_20.Fert.surface.Clay.incorp_20",#changed
  "Rip.50_Fert.surface",
  "Rip.50_Clay.incorp_20",#changed
  "Rip.50_Fert.surface.Clay.incorp_20",#changed
  "Rip.50Spade.30_none",
  "Inc.50_none",
  "Inc.50_Cl@7.5.incorp_50",
  "Rip.50IncRip_none",
  "Rip.50IncRip_Cl.incorp_50",
  "Rip.50IncRip_Cl.surface",
  
  "Rip.60_none",
  "Rip.60_Cl.surface",
  "Rip.60_Cl.band_60",
  "Rip.60_Lc.incorp_60",
  "Rip.60_Lc.band_60",
  "Rip.60_Fert.band_8",
  "Rip.60_Fert.band_60",
  "Rip.60Spade.30_none",
  "Rip.60Spade.30_Lc.band_30+60",
  "Rip.60Spade.30_Lc.incorp_30+band_60",
  "Rip.60IncRip_none",
  "Rip.60IncRip+Spade.30_none",
  
  "Delving.18_none",
  "Delving.18_SE14.band_8",
  "DiscInv.30_none"
)







summary_data_all_1$Descriptors <- factor(summary_data_all_1$Descriptors,
                                       levels = order)

#filter the data
summary_data_site <- summary_data_all_1 %>%
  filter(site == a) %>% 
  filter(year == b)

## count yld values
count_missing_yld_values <- summary_data_site %>% 
  dplyr::group_by(Descriptors) %>% 
  dplyr::summarise(count = sum(!is.na(yield)))


## summaries data
data_summary_all_analysis <- summary_data_site %>% 
  dplyr::group_by(Descriptors) %>%
  dplyr::summarise(mean=mean(yield, na.rm = TRUE), 
                   sd=sd(yield, na.rm = TRUE),
                   #count = n(),
                   #std_error = sd/(sqrt(count)),
                   
  ) %>%
  arrange(desc(mean))
## add in the count values (not the number of rows)
data_summary_all_analysis <- left_join(data_summary_all_analysis,count_missing_yld_values )


data_summary_all_analysis <- data_summary_all_analysis %>% 
  mutate(site =  a,
         year =  b,
         std_error = sd/(sqrt(count)))




data_summary_all_analysis <- as.data.frame(ungroup(data_summary_all_analysis))




########################################################################################################################################################
#### ANOVA #####

model = lm( yield ~ Descriptors,
                data=summary_data_site)

anova_yld <- Anova(model, type="II") # Can use type="III"

p_value_ANOVA <- anova_yld[1,4]
F_value_ANOVA <- anova_yld[1,3]

### add these values into the summary data

data_summary_all_analysis <- data_summary_all_analysis %>% 
  mutate(p_value_ANOVA  = p_value_ANOVA,
         F_value_ANOVA  = F_value_ANOVA)

#Add in the significance ***
data_summary_all_analysis <- data_summary_all_analysis %>%
  mutate(ANOVA_sign = case_when(
    p_value_ANOVA < 0.001 ~ "***",
    p_value_ANOVA <= 0.01 ~  "**",
    p_value_ANOVA <= 0.05 ~  "*",
    p_value_ANOVA >  0.05 ~  "ns",
    TRUE ~ "check"
    
  ))


########################################################################################################################################################
#####homoscedasticity vs equal variance test we can use Bartlett test

bartlett.test <- bartlett.test(yield ~ Descriptors,
              data = summary_data_site) 

p_value_barlett <- bartlett.test[[3]]

### add these values into the summary data
data_summary_all_analysis <- data_summary_all_analysis %>% 
  mutate(p_value_barlett  = p_value_barlett)


########################################################################################################################################################
#### agricolae_LSD
model_sand = lm( yield ~ Descriptors,
                 data=summary_data_site)

agricolae_LSD_output_sand <- (LSD.test(model_sand, "Descriptors",   # outer parentheses print result
                                       alpha = 0.05,      
                                       p.adj="none"))      # see ?p.adjust for options"none" is t-student.




#Extract the LSD value from the anlsysis and add it to the summary data

LSD_value_1 <- agricolae_LSD_output_sand$statistics$LSD #this becomes NULL if there is not values


#get the 'max value' aka make it a value and make a df 
LSD_value_1 <- max(LSD_value_1)
LSD_value_df <- data.frame(LSD_value_1)

LSD_value_df <- LSD_value_df %>% 
  dplyr::mutate(
    LSD_max = case_when(
      LSD_value_1 > 0 ~ as.character(LSD_value_1),
      TRUE ~ "not reported"
    ))


LSD <- LSD_value_df[1,2]
LSD
#Extract the LSD letters from the anlsysis and add it to the summary data

agricolae_LSD_output_sand_df <- as.data.frame(agricolae_LSD_output_sand[[5]]) #get the fith item in the list
agricolae_LSD_output_sand_df$Descriptors <- rownames(agricolae_LSD_output_sand_df) #move rwo names into a clm for joining

agricolae_LSD_output_sand_df <- agricolae_LSD_output_sand_df %>% 
  mutate(LSD = LSD)

data_summary_all_analysis <- left_join(data_summary_all_analysis,agricolae_LSD_output_sand_df)

data_summary_all_analysis <- dplyr::select(data_summary_all_analysis, -yield) %>% 
  dplyr::rename(groups_LSD = groups)


########################################################################################################################################################
#### agricolae_HSD.test

tukey_agricolae <- (HSD.test(model_sand, "Descriptors"))

# I want to access the groups but its part of a list


tukey_agricolae_df <- as.data.frame(tukey_agricolae[[5]]) #get the fith item in the list
tukey_agricolae_df$Descriptors <- rownames(tukey_agricolae_df) #move row names into a clm for joining

data_summary_all_analysis <- left_join(data_summary_all_analysis,tukey_agricolae_df)
data_summary_all_analysis <- dplyr::select(data_summary_all_analysis, -yield) %>% 
  dplyr::rename(groups_HSD_Tukey = groups)







########################################################################################################################################################
### Dunnet test
str(summary_data_site)
DunnettTest <- DunnettTest(yield ~ Descriptors,
                           data = summary_data_site,
                           control = "Control")

DunnettTest_df <- as.data.frame(DunnettTest[[1]]) #get the fith item in the list
DunnettTest_df$Descriptors_control <- rownames(DunnettTest_df) #move rwo names into a clm for joining

## strip the control from the descriptor name
DunnettTest_df <- DunnettTest_df %>%
  mutate(Descriptors = str_replace(Descriptors_control, "(-Control)", ""))


#Add in the significance ***

DunnettTest_df <- DunnettTest_df %>%
  mutate(significance_control = case_when(
    pval < 0.001 ~ "***",
    pval <= 0.01 ~  "**",
    pval <= 0.05 ~  "*",
    pval >  0.05 ~  "ns",
    TRUE ~ "check"

  ))
DunnettTest_df <- DunnettTest_df %>%
  dplyr::select(Descriptors, pval, significance_control)


#join it the summary data

data_summary_all_analysis <- left_join(data_summary_all_analysis, DunnettTest_df)


#rename the clms so they are more reflect the test
data_summary_all_analysis <- data_summary_all_analysis %>%
  dplyr::rename("pval_Dunnets" = "pval")



name <- paste0(a,"_", b, "_ANOVA")
assign(name,data_summary_all_analysis)

rm(site_and_yr,
   summary_data_all_1,
   summary_data_site,
   model_sand,
   agricolae_LSD_output_sand,
   agricolae_LSD_output_sand_df,
   tukey_agricolae,
   tukey_agricolae_df,
   data_summary_all_analysis,
   DunnettTest_df,
   DunnettTest,
   a,
   b,
   data_file,
   LSD,
   name,
   order,
   site_yrs_list,
   anova_yld,
   bartlett.test,
   p_value_barlett,
   F_value_ANOVA,
   p_value_ANOVA,
   model,
   LSD_value_1,
   LSD_value_df,
   count_missing_yld_values
   
   )


}




#### merge the files run
# ANOVA_sites_yr <- rbind(Buckleboo_2019_ANOVA,
#                         Buckleboo_2020_ANOVA,
#                         Buckleboo_2021_ANOVA)

ANOVA_sites_yr <- rbind(       
                               `Brimpton Lake_2014_ANOVA`,
                               `Brimpton Lake_2015_ANOVA`,
                               `Brimpton Lake_2016_ANOVA`,
                               `Brimpton Lake_2017_ANOVA`,
                               `Brimpton Lake_2018_ANOVA`,
                               
                               
                               Brooker_2019_ANOVA,
                               Brooker_2020_ANOVA,
                               Brooker_2021_ANOVA ,

                               Buckleboo_2019_ANOVA,
                               Buckleboo_2020_ANOVA,
                               Buckleboo_2021_ANOVA,

                               Bute_CSIRO_2018_ANOVA,
                               Bute_CSIRO_2019_ANOVA,
                               Bute_CSIRO_2020_ANOVA,
                               Bute_CSIRO_2021_ANOVA,

                               Bute_Trengrove_2015_ANOVA,
                               Bute_Trengrove_2016_ANOVA,
                               Bute_Trengrove_2017_ANOVA,
                               Bute_Trengrove_2018_ANOVA,
                               Bute_Trengrove_2019_ANOVA,
                               Bute_Trengrove_2020_ANOVA,
                               Bute_Trengrove_2021_ANOVA,

                               Cadgee_2014_ANOVA,
                               Cadgee_2015_ANOVA,
                               #Cadgee_2016_ANOVA, #this has no yield data
                               Cadgee_2017_ANOVA,
                               Cadgee_2018_ANOVA,

                               Carwarp_Amelioration_2018_ANOVA,
                               Carwarp_Amelioration_2019_ANOVA,
                               Carwarp_Amelioration_2020_ANOVA,

                               Cummins_2019_ANOVA,
                               Cummins_2020_ANOVA,
                               Cummins_2021_ANOVA,

                               Karkoo_2019_ANOVA,
                               Karkoo_2020_ANOVA,
                               Karkoo_2021_ANOVA,

                               Karoonda_2014_ANOVA,
                               Karoonda_2015_ANOVA,
                               Karoonda_2016_ANOVA,
                               Karoonda_2017_ANOVA,
                               Karoonda_2018_ANOVA,

                               Kooloonong_canola_2021_ANOVA,

                               Kooloonong_chickpea_2019_ANOVA,
                               Kooloonong_chickpea_2020_ANOVA,
                               Kooloonong_chickpea_2021_ANOVA,

                               Kooloonong_lentil_2019_ANOVA,
                               Kooloonong_lentil_2020_ANOVA,
                               Kooloonong_lentil_2021_ANOVA,

                               Kooloonong_lupin_2019_ANOVA,
                               Kooloonong_lupin_2020_ANOVA,
                               Kooloonong_lupin_2021_ANOVA,

                               Kybunga_2019_ANOVA,
                               Kybunga_2020_ANOVA,
                               Kybunga_2021_ANOVA,

                               `Lowaldie_Crest_2019_ANOVA`,
                               `Lowaldie_Crest_2020_ANOVA`,
                               #`Lowaldie_Crest_2021_ANOVA`, # no data

                               `Lowaldie_Deep sand_2019_ANOVA`,
                               `Lowaldie_Deep sand_2020_ANOVA`,
                               #`Lowaldie_Deep sand_2021_ANOVA`, # no data
                                
                               Malinong_2019_ANOVA,
                               Malinong_2020_ANOVA,
                               Malinong_2021_ANOVA,

                               Monia_Gap_2019_ANOVA,
                               Monia_Gap_2020_ANOVA,
                               Monia_Gap_2021_ANOVA,

                               `Mt Damper_2019_ANOVA`,
                               `Mt Damper_2020_ANOVA`,
                               `Mt Damper_2021_ANOVA`,
                               
                               
                               Murlong_2018_ANOVA,
                               Murlong_2019_ANOVA,
                               Murlong_2020_ANOVA,
                               Murlong_2021_ANOVA,
                               
                               Ouyen_Spade_2017_ANOVA,
                               Ouyen_Spade_2018_ANOVA,
                               Ouyen_Spade_2019_ANOVA,
                               Ouyen_Spade_2020_ANOVA,
                               
                               Sherwood_2019_ANOVA,
                               Sherwood_2020_ANOVA,
                               Sherwood_2021_ANOVA,
                               
                               Telopea_Downs_2020_ANOVA,
                               Telopea_Downs_2021_ANOVA,
                               
                               Tempy_2019_ANOVA,
                               Tempy_2020_ANOVA,
                               #"TempyX2021", #no yield
                               
                               Waikerie_2018_ANOVA,
                               Waikerie_2019_ANOVA,
                               Waikerie_2020_ANOVA,
                               
                               Warnertown_2019_ANOVA,
                               Warnertown_2020_ANOVA,
                               Warnertown_2021_ANOVA,
                               
                               Wynarka_2019_ANOVA,
                               Wynarka_2020_ANOVA,
                               Wynarka_2021_ANOVA,
                               
                               Yenda_2017_ANOVA,
                               Yenda_2018_ANOVA,
                               Yenda_2019_ANOVA,
                               Yenda_2020_ANOVA,
                               Yenda_2021_ANOVA,
                               
                               Younghusband_2020_ANOVA
                       )






rm( `Brimpton Lake_2014_ANOVA`,
    `Brimpton Lake_2015_ANOVA`,
    `Brimpton Lake_2016_ANOVA`,
    `Brimpton Lake_2017_ANOVA`,
    `Brimpton Lake_2018_ANOVA`,
    
    
    Brooker_2019_ANOVA,
    Brooker_2020_ANOVA,
    Brooker_2021_ANOVA,

    Buckleboo_2019_ANOVA,
    Buckleboo_2020_ANOVA,
    Buckleboo_2021_ANOVA,

    Bute_CSIRO_2018_ANOVA,
    Bute_CSIRO_2019_ANOVA,
    Bute_CSIRO_2020_ANOVA,
    Bute_CSIRO_2021_ANOVA,

    Bute_Trengrove_2015_ANOVA,
    Bute_Trengrove_2016_ANOVA,
    Bute_Trengrove_2017_ANOVA,
    Bute_Trengrove_2018_ANOVA,
    Bute_Trengrove_2019_ANOVA,
    Bute_Trengrove_2020_ANOVA,
    Bute_Trengrove_2021_ANOVA,

    Cadgee_2014_ANOVA,
    Cadgee_2015_ANOVA,
    #Cadgee_2016_ANOVA, #no yield data
    Cadgee_2017_ANOVA,
    Cadgee_2018_ANOVA,

    Carwarp_Amelioration_2018_ANOVA,
    Carwarp_Amelioration_2019_ANOVA,
    Carwarp_Amelioration_2020_ANOVA,

    Cummins_2019_ANOVA,
    Cummins_2020_ANOVA,
    Cummins_2021_ANOVA,

    Karkoo_2019_ANOVA,
    Karkoo_2020_ANOVA,
    Karkoo_2021_ANOVA,

    Karoonda_2014_ANOVA,
    Karoonda_2015_ANOVA,
    Karoonda_2016_ANOVA,
    Karoonda_2017_ANOVA,
    Karoonda_2018_ANOVA,

    Kooloonong_canola_2021_ANOVA,

    Kooloonong_chickpea_2019_ANOVA,
    Kooloonong_chickpea_2020_ANOVA,
    Kooloonong_chickpea_2021_ANOVA,

    Kooloonong_lentil_2019_ANOVA,
    Kooloonong_lentil_2020_ANOVA,
    Kooloonong_lentil_2021_ANOVA,

    Kooloonong_lupin_2019_ANOVA,
    Kooloonong_lupin_2020_ANOVA,
    Kooloonong_lupin_2021_ANOVA,

    Kybunga_2019_ANOVA,
    Kybunga_2020_ANOVA,
    Kybunga_2021_ANOVA,

    `Lowaldie_Crest_2019_ANOVA`,
    `Lowaldie_Crest_2020_ANOVA`,
    #`Lowaldie_Crest_2021_ANOVA`, # no data
     
    `Lowaldie_Deep sand_2019_ANOVA`,
    `Lowaldie_Deep sand_2020_ANOVA`,
    #`Lowaldie_Deep sand_2021_ANOVA`, # no data
     
    Malinong_2019_ANOVA,
    Malinong_2020_ANOVA,
    Malinong_2021_ANOVA,

    Monia_Gap_2019_ANOVA,
    Monia_Gap_2020_ANOVA,
    Monia_Gap_2021_ANOVA,

    `Mt Damper_2019_ANOVA`,
    `Mt Damper_2020_ANOVA`,
    `Mt Damper_2021_ANOVA`,
    
    Murlong_2018_ANOVA,
    Murlong_2019_ANOVA,
    Murlong_2020_ANOVA,
    Murlong_2021_ANOVA,
    
    Ouyen_Spade_2017_ANOVA,
    Ouyen_Spade_2018_ANOVA,
    Ouyen_Spade_2019_ANOVA,
    Ouyen_Spade_2020_ANOVA,
    
    Sherwood_2019_ANOVA,
    Sherwood_2020_ANOVA,
    Sherwood_2021_ANOVA,
    
    Telopea_Downs_2020_ANOVA,
    Telopea_Downs_2021_ANOVA,
    
    Tempy_2019_ANOVA,
    Tempy_2020_ANOVA,
    #"TempyX2021", #no yield
    
    Waikerie_2018_ANOVA,
    Waikerie_2019_ANOVA,
    Waikerie_2020_ANOVA,
    
    Warnertown_2019_ANOVA,
    Warnertown_2020_ANOVA,
    Warnertown_2021_ANOVA,
    
    Wynarka_2019_ANOVA,
    Wynarka_2020_ANOVA,
    Wynarka_2021_ANOVA,
    
    Yenda_2017_ANOVA,
    Yenda_2018_ANOVA,
    Yenda_2019_ANOVA,
    Yenda_2020_ANOVA,
    Yenda_2021_ANOVA,
    
    Younghusband_2020_ANOVA
)




write.csv(ANOVA_sites_yr,"X:/Therese_Jackie/Sandy_soils/Development_database/stats_batch_output/ANOVA_by_Yr_sites_merged.csv" ,
          row.names = FALSE)








