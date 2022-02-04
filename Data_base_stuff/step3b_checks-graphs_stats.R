#install.packages("multcompView")

library(ggplot2)
library(readxl)
library(tidyverse)
library(multcompView)



#################################################################################################################
####   Get the data #####



#####################################################################################################################

data_file <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/sites_merged.csv"

## download the data using the specified file path above



summary_data_all <- read_csv(data_file)
unique(summary_data_all$site_sub)

#site_name <- "Bute_Trengrove" #"Bute_CSIRO"
#site_name <- "Bute_CSIRO"
site_name <-"Ouyen_spade"

list_of_descriptors<- summary_data_all %>% 
  #filter(site == site_name) %>% 
  filter(site == "Ouyen") %>%
  distinct(Descriptors) %>% 
  arrange(desc(Descriptors))

head(list_of_descriptors,25)

##### order the Descriptors
order <- c("Control" ,
"Unmodified_SE14.band_8" ,
"Unmodified_Bi-Agra.surface+band_8" ,
"Unmodified_Lc.surface" ,
"Unmodified_Cl.surface" ,

"Unmodified_Cl.incorp_8" ,

"Unmodified_Cl@2.5.surface",

"Unmodified_Cl@5.incorp_8",
"Unmodified_Cl@5.incorp_8.Fert.surface",#10
"Unmodified_Cl@5.incorp_8.Clay.incorp_8",
"Unmodified_Cl@5.incorp_8.Fert.surface.Clay.incorp_8", 

"Unmodified_Cl@7.5.surface",
"Unmodified_Cl@2.5.surface_Yr18,19,20",

"Unmodified_Cl@20.incorp_8",
"Unmodified_Cl@20.incorp_8.Fert.surface",
"Unmodified_Cl@20.incorp_8.Clay.incorp_8",
"Unmodified_Cl@20.incorp_8.Fert.surface.Clay.incorp_8",

"Unmodified_Cl@3.incorp_8" ,

"Unmodified_Fert.surface" ,
"Unmodified_Fert.surface.Clay.incorp_8", 
"Unmodified_Fert.foliar" ,  
"Unmodified_Fert.incorp_8" , 
"Unmodified_Fert.band_8" , #20

"Unmodified_Fert.band_30" ,
"Unmodified_Clay.incorp_10" ,
"Unmodified_Clay.incorp_8" ,
"Unmodified_K_added.surface" ,
"Unmodified_Fert.band_30.Clay.incorp_10" ,


"Spade.30_Lc@1.incorp_30" ,
"Spade.30_Lc@2.incorp_30" ,
"Spade.30_Lc@4.incorp_30" ,
"Spade.30_Lc@6.incorp_30" ,
"Spade.30_Lc@8.incorp_30" ,#30
"Spade.30_Lc@10.incorp_30" ,
"Spade.30_Lc@15.incorp_30" ,
"Spade.30_Lc@20.incorp_30" ,
"Spade.30_Lc@1.incorp_30.K_added.surface" ,
"Spade.30_Lc@2.incorp_30.K_added.surface" ,
"Spade.30_Lc@4.incorp_30.K_added.surface" ,
"Spade.30_Lc@6.incorp_30.K_added.surface", 
"Spade.30_Lc@8.incorp_30.K_added.surface" ,
"Spade.30_Lc@10.incorp_30.K_added.surface", 
"Spade.30_Lc@15.incorp_30.K_added.surface", #40
"Spade.30_Lc@20.incorp_30.K_added.surface" ,
"Spade.30_none" ,
"Spade.30_Lc.incorp_30", 
"Spade.30_Cl.incorp_30" ,
"Spade.30_Com.incorp_30" ,
"Spade.30_Cereal.incorp_30", 
"Spade.30_K_added.surface" ,
"Spade.30_Fert.incorp_30.K_added.incorp_30", 
"Spade.30_Fert.incorp_30" ,
"Spade.30_Fert.incorp_30.Clay.incorp_30", #50
"Spade.30_Clay.incorp_30" ,
"Spade.30_Lc.incorp_30.Clay.incorp_30" ,
"Spade.30_Lc.incorp_30.Fert.incorp_30" ,
"Spade.30_Lc.incorp_30.Fert.incorp_30.Clay.incorp_30" ,
"Spade.30_Vetch.incorp_30" ,
"Spade.30_Vet_Cer_In.incorp_30" ,
"Spade.30_Vet_Cer.incorp_30" ,
"Rip.30_none" ,
"Rip.30_Cl.surface" ,
"Rip.30_Cl@7.5.surface",    
"Rip.30_Cl@7.5.band_30",
"Rip.30_Cl.band_30" ,#60
"Rip.30_Fert.band_8" ,
"Rip.30_Fert.band_30" ,
"Rip.30_Fert.incorp_30" ,
"Rip.30_Lc.incorp_30" ,
"Rip.40_none" ,
"Rip.41_none" ,
"Rip.41_Lc.incorp_41" ,
"Rip.41_Fert.incorp_41" ,

"Rip.50_none",
"Rip.50_Cl@2.5.surface ",
"Rip.50_Cl@2.5.surface_Yr18,19,20",
"Rip.50_Cl@7.5.surface",
"Rip.50_Cl@7.5.band_50",

"Rip.50_Cl@5.incorp_50",#70
"Rip.50_Cl@20.incorp_50",
"Rip.50_Fert.surface",
"Rip.50_Clay.incorp_50",
"Rip.50_Fert.surface.Clay.incorp_50",
"Rip.50_Cl@5.incorp_50.Fert.surface",
"Rip.50_Cl@5.incorp_50.Clay.incorp_50",
"Rip.50_Cl@5.incorp_50.Fert.surface.Clay.incorp_50",
"Rip.50_Cl@20.incorp_50.Fert.surface",
"Rip.50_Cl@20.incorp_50.Clay.incorp_50",
"Rip.50_Cl@20.incorp_50.Fert.surface.Clay.incorp_50",#80

"Rip.60_none" ,
"Rip.60_Lc.incorp_60" ,
"Rip.60_Fert.band_8" ,
"Rip.60_Cl.surface" ,
"Rip.60_Cl.band_60" ,
"Rip.60_Fert.band_60" ,
"Rip.30+60_none" ,
"Rip.60Spade.30_none" ,
"Rip.60Spade.30_Lc.band_30+60", 
"Rip.30+60_Lc.band_30+60" ,#90

"Rip.50_Cl.surface" ,
"Rip.50_Cl.incorp_50" ,
"Rip.50_Cl.band_50" ,
 
"Inc.50_none" ,
"Inc.50_Cl.incorp_50" ,
"Inc.50_Cl@7.5.incorp_50",
"Rip.50IncRip_none" ,
"Delving.18_none" ,
"Delving.18_SE14.band_8" ,
"Sweep.30_none" ,
"Sweep.30_Cl.incorp_30" ,
"Sweep.30_Cl@3.incorp_30",
"Sweep.30_Cl@6.incorp_30",
"Sweep.30_Cl@6.incorp_30.Yrs17,18,19",
"Sweep.30_Cl@9.incorp_30",             
             
"Sweep.30_Cl@3.incorp_30.Lime.incorp_8",
"Sweep.30_Lime.incorp_30" ,
"Sweep.30_Cl.incorp_30.Clay.incorp_8" )





      

summary_data_all$Descriptors <- factor(summary_data_all$Descriptors,
                                       levels = order)


str(summary_data_all)
summary_data_all %>% distinct(site)
summary_data_all %>% distinct(site_sub)


####################################################################################################################################
####################################################################################################################################
###https://statdoe.com/two-way-anova-in-r/
####################################################################################################################################
####################################################################################################################################

#site_name <- "Yenda"
#site_name <- "Bute_Trengrove"
#site_name <- "Bute_CSIRO"
site_name <- "Ouyen_spade"

## for what years
# summary_data_all %>% filter(site == site_name) %>% 
#   distinct(year)
 
summary_data_all %>% filter(site_sub == site_name) %>% 
  distinct(year)

##################################################################################################################################

##################################################################################################################################


##################################################################################################################################
rm(anova, cld, data_summary, plot, summary_data, tukey, tukey.cld, year_selected)

#year_selected <- 2021

#year_selected <- 2020
#year_selected <- 2019
#year_selected <- 2018
year_selected <- 2017

#year_selected <- 2016
#year_selected <- 2015

# Compute the analysis of variance
summary_data <- summary_data_all %>%
  #filter(site == site_name) %>% 
  filter(site_sub == site_name) %>%
  filter(year==year_selected)

anova <- aov(yield ~ Descriptors, data = summary_data)
# Summary of the analysis
summary(anova)

str(summary_data)
data_summary <- summary_data %>% 
  group_by(Descriptors) %>%
  summarise(mean=mean(yield, na.rm = TRUE), 
            sd=sd(yield, na.rm = TRUE),
            count = n(),
            std_error = sd/(sqrt(count))
            ) %>%
  arrange(desc(mean))
print(data_summary)


tukey <- TukeyHSD(anova)
print(tukey)


tukey.cld <- multcompLetters4(anova, tukey) # default is threshold = 0.05
print(tukey.cld)


#adding the compact letter display to the table with means and sd
cld <- as.data.frame.list(tukey.cld$Descriptors)
data_summary$Tukey <- cld$Letters
print(data_summary)

## add in some details 
data_summary <- data_summary %>% 
  mutate(site = site_name,
         year = year_selected)

data_summary <- data_summary %>% 
  arrange(Descriptors)

data_summary


output_folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/"

write.csv(data_summary, paste0(output_folder,"Yield_",site_name,year_selected,".csv"))

###############################################################################################################
### Now for the plots ###


#https://statdoe.com/barplot-for-two-factors-in-r/

output_folder <- "X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/"



data_summary <- read_csv(paste0(output_folder,"Yield_",site_name,year_selected,".csv"))
print(data_summary)

data_summary$Descriptors <- factor(data_summary$Descriptors,
                                       levels = order)

# barplot with letters
plot <- data_summary %>%  
ggplot( aes(x = factor(Descriptors), y = mean)) + 
  geom_bar(stat = "identity",  alpha = 0.5)  +
  geom_errorbar(aes(ymin=mean-std_error, ymax=mean+std_error), width = 0.1) +
  labs(x="", y="Yield t/ha", title = paste(site_name,": ", year_selected),
       subtitle = "ANOVA with Tukey, threshold 0.05") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = c(0.1, 0.75)) +
  geom_text(aes(label=Tukey), position = position_dodge(0.80), size = 4, 
            vjust=-0.5, hjust=-0.5, colour = "gray25")+
  theme(axis.text.x=element_text(angle=45,hjust=1))


plot
ggsave(plot,
       device = "png",
       filename = paste0("Plot_yield", site_name,"_", year_selected, ".png"),
       path= output_folder,
       width=8.62,
       height = 6.28,
      dpi=600
       )




###############################################################################################################################
