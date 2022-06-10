

### Try a few things out..


library(stats)
library(tidyr)
library(ggplot2)
library(ggplot)
library(tidyverse)
library(dplyr)
library(magrittr)
library(cluster)
library(cluster.datasets)
library(cowplot)
library(corrplot)
library(NbClust)
library(clValid)
library(ggfortify)
library(dendextend)
library(factoextra)
library(FactoMineR)
library(corrplot)
library(GGally)
library(ggiraphExtra)
library(knitr)
library(kableExtra)

summary_control_data_all <- read.csv( "X:Therese_Jackie/Sandy_soils/Development_database/for_presentations/temp_summary_control_data_all.csv")
messy_all <- read.csv( "X:Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/sites_merged_all_messy.csv")

names(summary_control_data_all)
names(messy_all)

messy_all<-messy_all %>% 
  select(ID, Repellence:Nutrient, rain)

summary_control_data_all <- summary_control_data_all %>% 
  dplyr::select(ID,
                site,
                yield_gain,
                amendment_code1,
                soil_modification,
                yr_post_amelioration,
                decile,
                for_join
  )


join <- left_join(summary_control_data_all, messy_all, by = "ID")

write.csv(join, "X:Therese_Jackie/Sandy_soils/Development_database/for_presentations/maybe_PCA_input.csv",
                    row.names = FALSE)

names(join)
#remove the missing data 

qual <- na.omit(join)

#select just a few clms

qual <- qual %>% 
  select(Repellence:rain,  yield_gain)
names(qual)



glimpse(qual)

####################################################################################
####    Scale DATA    ##############################################################


qual_scaled<- scale(qual)

####################################################################################
####    PCA analysis    ##############################################################

res.pca<-PCA(qual_scaled)

fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0,50))  # I have used this in PCA analysis to work out how many PCA I should use

var<-get_pca_var(res.pca)

fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE #Avoid text overlapping
) + theme_minimal() + ggtitle("Variables - PCA")

