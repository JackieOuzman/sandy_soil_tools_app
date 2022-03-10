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


a = "Carwarp_Amelioration"
b = 2018


#######################################################################################################################################################
##########                                                    function                                                                       ##########
#######################################################################################################################################################
function_anova <- function(a, b){

# bring in the data
data_file <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/sites_merged.csv"
summary_data_all_1 <- read_csv(data_file, 
                               col_types = cols(rep_block = col_character()))
# set the site name and year
site_name        <- a
site_name_output <- a
year             <- b


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
  "Rip.50_Cl@5.incorp_50",
  "Rip.50_Cl@7.5.band_50",
  "Rip.50_Cl@20.incorp_50",
  "Rip.50_Cl.deep",
  "Rip.50_Cl.band_50",
  "Rip.50_Cl@5.incorp_50.Fert.surface",
  "Rip.50_Cl@5.incorp_50.Clay.incorp_50",
  "Rip.50_Cl@5.incorp_50.Fert.surface.Clay.incorp_50",
  "Rip.50_Cl@20.incorp_50.Fert.surface",
  "Rip.50_Cl@20.incorp_50.Clay.incorp_50",
  "Rip.50_Cl@20.incorp_50.Fert.surface.Clay.incorp_50",
  "Rip.50_Fert.surface",
  "Rip.50_Clay.incorp_50",
  "Rip.50_Fert.surface.Clay.incorp_50",
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


data_summary_all_analysis <- summary_data_site %>% 
  dplyr::group_by(Descriptors) %>%
  dplyr::summarise(mean=mean(yield, na.rm = TRUE), 
                   sd=sd(yield, na.rm = TRUE),
                   count = n(),
                    std_error = sd/(sqrt(count)),
                   
  ) %>%
  arrange(desc(mean))



data_summary_all_analysis <- data_summary_all_analysis %>% 
  mutate(site =  site_name,
          year =  year)



data_summary_all_analysis <- as.data.frame(ungroup(data_summary_all_analysis))




########################################################################################################################################################
#### agricolae_LSD
model_sand = lm( yield ~ Descriptors,
                 data=summary_data_site)

agricolae_LSD_output_sand <- (LSD.test(model_sand, "Descriptors",   # outer parentheses print result
                                       alpha = 0.05,      
                                       p.adj="none"))      # see ?p.adjust for options"none" is t-student.




#Extract the LSD value from the anlsysis and add it to the summary data
LSD <- agricolae_LSD_output_sand$statistics$LSD


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
tukey_agricolae_df$Descriptors <- rownames(tukey_agricolae_df) #move rwo names into a clm for joining

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



return(data_summary_all_analysis)
}

#######################################################################################################################################################


### what sites do I have?
sites <- read_csv("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/sites_merged.csv", 
                             col_types = cols(rep_block = col_character()))

list_of_sites <- sites %>%  distinct(site) %>% dplyr::arrange(site)
list_of_sites

#### site to analyse
a = "Carwarp_Amelioration"

## what years do I have?
sites_names <- sites %>%   filter(site == a) %>% distinct(year)
min(sites_names)
max(sites_names)

### Run function
Carwarp_Amelioration_2018 <- function_anova(a, 2018)
Carwarp_Amelioration_2019 <- function_anova(a, 2019)
Carwarp_Amelioration_2020 <- function_anova(a, 2020)

### Merge df

Carwarp_Amelioration_ANOVA <- rbind(Carwarp_Amelioration_2018,
                                    Carwarp_Amelioration_2019,
                                    Carwarp_Amelioration_2020)

rm(Carwarp_Amelioration_2018, Carwarp_Amelioration_2019, Carwarp_Amelioration_2020)
