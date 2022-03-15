#site_yrs_list = "Carwarp_AmeliorationX2018to2020"



### List of sites I want to run analysis for:
site_yrs_list <- c(
                   "Brimpton LakeX2014to2018",
                   "BrookerX2019to2021",
                   "BucklebooX2019to2021",
                   "Bute_CSIROX2018to2021",
                   "Bute_TrengroveX2015to2021",
                   "CadgeeX2014to2018",
                   #"CadgeeX2016", #no yield data
                   "Carwarp_AmeliorationX2018to2020",
                   "CumminsX2019to2021",
                   "KarkooX2019to2021",
                   "KaroondaX2014to2018",
                   "Kooloonong_canolaX2021",
                   "Kooloonong_chickpeaX2019to2021",
                   "Kooloonong_lentilX2019to2021",
                   "Kooloonong_lupinX2019to2021",
                   "KybungaX2019to2021",
                   "Lowaldie_CrestX2019to2020",
                   #"Lowaldie_CrestX2021", #no data 
                   "Lowaldie_Deep sandX2019to2020",
                   #"Lowaldie_Deep sandX2021",#no data
                   "MalinongX2019to2021",
                   "Monia_GapX2019to2021",
                   "Mt DamperX2019to2021",
                   
                   "MurlongX2018to2021",
                   "Ouyen_SpadeX2017to2020",
                   "SherwoodX2019to2021",
                   "Telopea_DownsX2020to2021",
                   "TempyX2019to2020", #"TempyX2021", #no yield
                   "WaikerieX2018to2020",
                   "WarnertownX2019to2021",
                   "WynarkaX2019to2021",
                   "YendaX2017to2021",
                   "YounghusbandX2020"
                   
                   )



#######################################################################################################################################################
##########                                                    loop                                                                       ##########
#######################################################################################################################################################

for (site_yrs_list in site_yrs_list){
  
  
##########################################
  site_and_yrs <- as.data.frame(str_split(site_yrs_list, "X"),
                               col.names = "site_and_yrs" )
  
  site_and_yrs$site_and_yrs <- as.character(site_and_yrs$site_and_yrs)
  site_and_yrs
  a <- site_and_yrs[1,1]
  b <- site_and_yrs[2,1]
  b
  
######################################
  # bring in the data




summary_data_all_1 <- read_csv("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/stats_working/sites_merged.csv", 
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
  filter(site == a) 


########################################################################################################################################

#keep the rep but not the year and group the data This is for the ANOVA
cumulative_yld <- summary_data_site %>% 
  dplyr::group_by(Descriptors,rep_block) %>% 
  dplyr::summarise(sum_yld = sum(yield, na.rm = TRUE))


###################################################################################################################################

#This is for the summary table  (you should be checking the mean cum yield here make sure they are sesnisble)

### this is to get the mean value of the cumlative yield
cumulative_yld_table <- cumulative_yld %>% 
  dplyr::group_by(Descriptors) %>%
  dplyr::summarise(mean_cum_yld=mean(sum_yld, na.rm = TRUE), 
            sd=sd(sum_yld, na.rm = TRUE),
            count = n(),
            std_error = sd/(sqrt(count))
            
  ) %>%
  arrange(desc(mean_cum_yld))

cumulative_yld_table <- as.data.frame(ungroup(cumulative_yld_table))

########################################################################################################################################################
#### ANOVA #####

model_cum = lm( sum_yld ~ Descriptors,
                 data=cumulative_yld)

anova_cum_yld <- Anova(model_cum, type="II") # Can use type="III"

### If you use type="III", you need the following line before the analysi
### options(contrasts = c("contr.sum", "contr.poly"))
anova_cum_yld

p_value_ANOVA_cum <- anova_cum_yld[1,4]
F_value_ANOVA_cum <- anova_cum_yld[1,3]

### add these values into the summary data

cumulative_yld_table <- cumulative_yld_table %>% 
  mutate(yrs = b, 
         site = a,
         p_value_ANOVA_cum  = p_value_ANOVA_cum,
         F_value_ANOVA_cum  = F_value_ANOVA_cum)

########################################################################################################################################################
#####homoscedasticity vs equal variance test we can use Bartlett test

bartlett.test <- bartlett.test(yield ~ Descriptors,
                               data = summary_data_site) 

p_value_barlett <- bartlett.test[[3]]

### add these values into the summary data
cumulative_yld_table <- cumulative_yld_table %>% 
  mutate(p_value_barlett  = p_value_barlett)



########################################################################################################################################################
#### agricolae_LSD
model_cum_LSD = lm( sum_yld ~ Descriptors,
                 data=cumulative_yld)

agricolae_LSD_output_sand_cum <- (LSD.test(model_cum_LSD, "Descriptors",   # outer parentheses print result
                                       alpha = 0.05,      
                                       p.adj="none"))      # see ?p.adjust for options"none" is t-student.



#Extract the LSD value from the anlsysis and add it to the summary data

LSD_value_1 <- agricolae_LSD_output_sand_cum$statistics$LSD #this becomes NULL if there is not values


#get the 'max value' aka make it a value and make a df 
LSD_value_1 <- max(LSD_value_1)
LSD_value_df <- data.frame(LSD_value_1)

LSD_value_df <- LSD_value_df %>% 
  dplyr::mutate(
    LSD_max = case_when(
      LSD_value_1 > 0 ~ as.character(LSD_value_1),
      TRUE ~ "not reported"
    ))


LSD_cum <- LSD_value_df[1,2]

#Extract the LSD letters from the anlsysis and add it to the summary data

agricolae_LSD_output_sand_df_cum <- as.data.frame(agricolae_LSD_output_sand_cum[[5]]) #get the fith item in the list
agricolae_LSD_output_sand_df_cum$Descriptors <- rownames(agricolae_LSD_output_sand_df_cum) #move rwo names into a clm for joining

agricolae_LSD_output_sand_df_cum <- agricolae_LSD_output_sand_df_cum %>% 
  mutate(LSD_cum = LSD_cum)

agricolae_LSD_output_sand_df_cum$Descriptors
cumulative_yld_table$Descriptors 


cumulative_yld_table <- left_join(cumulative_yld_table,agricolae_LSD_output_sand_df_cum)

cumulative_yld_table <- dplyr::select(cumulative_yld_table, -sum_yld) %>% 
  dplyr::rename(groups_LSD_cum = groups)

########################################################################################################################################################
#### agricolae_HSD.test

tukey_agricolae_cum <- (HSD.test(model_cum_LSD, "Descriptors"))

# I want to access the groups but its part of a list


tukey_agricolae_df_cum <- as.data.frame(tukey_agricolae_cum[[5]]) #get the fith item in the list
tukey_agricolae_df_cum$Descriptors <- rownames(tukey_agricolae_df_cum) #move rwo names into a clm for joining

cumulative_yld_table <- left_join(cumulative_yld_table,tukey_agricolae_df_cum)
cumulative_yld_table <- dplyr::select(cumulative_yld_table, -sum_yld) %>% 
  dplyr::rename(groups_HSD_Tukey = groups)

########################################################################################################################################################
### Dunnet test

DunnettTest_cum <- DunnettTest(sum_yld ~ Descriptors,
                           data = cumulative_yld,
                           control = "Control")

DunnettTest_df_cum <- as.data.frame(DunnettTest_cum[[1]]) #get the fith item in the list
DunnettTest_df_cum$Descriptors_control <- rownames(DunnettTest_df_cum) #move rwo names into a clm for joining

## strip the control from the descriptor name
DunnettTest_df_cum <- DunnettTest_df_cum %>%
  mutate(Descriptors = str_replace(Descriptors_control, "(-Control)", ""))


#Add in the significance ***

DunnettTest_df_cum <- DunnettTest_df_cum %>%
  mutate(significance_control = case_when(
    pval < 0.001 ~ "***",
    pval <= 0.01 ~  "**",
    pval <= 0.05 ~  "*",
    pval >  0.05 ~  "ns",
    TRUE ~ "check"
    
  ))
DunnettTest_df_cum <- DunnettTest_df_cum %>%
  dplyr::select(Descriptors, pval, significance_control)


#join it the summary data

cumulative_yld_table <- left_join(cumulative_yld_table, DunnettTest_df_cum)


#rename the clms so they are more reflect the test
cumulative_yld_table <- cumulative_yld_table %>%
  dplyr::rename("pval_Dunnets" = "pval")




name <- paste0(a,"_", b, "_Cum_ANOVA")
assign(name,cumulative_yld_table)

rm(site_and_yrs,
   summary_data_all_1,
   summary_data_site,
   model_sand_cum,
   agricolae_LSD_output_sand_cum,
   agricolae_LSD_output_sand_df_cum,
   tukey_agricolae_cum,
   tukey_agricolae_df_cum,
   data_summary_all_analysis,
   DunnettTest_df_cum,
   DunnettTest_cum,
   a,
   b,
   LSD_cum,
   anova_cum_yld,
   bartlett.test,
   p_value_barlett_cum,
   F_value_ANOVA_cum,
   p_value_ANOVA_cum,
   model_cum,
   cumulative_yld,
   cumulative_yld_table,
   model_cum_LSD
)

}





#### merge the files run


Cum_ANOVA_sites_yr <- rbind(
  `Brimpton Lake_2014to2018_Cum_ANOVA`,
  Brooker_2019to2021_Cum_ANOVA,
  Buckleboo_2019to2021_Cum_ANOVA,
  Bute_CSIRO_2018to2021_Cum_ANOVA,
  Bute_Trengrove_2015to2021_Cum_ANOVA,
  Cadgee_2014to2018_Cum_ANOVA,
  Carwarp_Amelioration_2018to2020_Cum_ANOVA,
  Cummins_2019to2021_Cum_ANOVA,
  Karkoo_2019to2021_Cum_ANOVA,
  Karoonda_2014to2018_Cum_ANOVA,
  Kooloonong_canola_2021_Cum_ANOVA,
  Kooloonong_chickpea_2019to2021_Cum_ANOVA,
  Kooloonong_lentil_2019to2021_Cum_ANOVA,
  Kooloonong_lupin_2019to2021_Cum_ANOVA,
  Kybunga_2019to2021_Cum_ANOVA,
  Lowaldie_Crest_2019to2020_Cum_ANOVA,
  `Lowaldie_Deep sand_2019to2020_Cum_ANOVA`,
  Malinong_2019to2021_Cum_ANOVA,
  Monia_Gap_2019to2021_Cum_ANOVA,
  `Mt Damper_2019to2021_Cum_ANOVA`,
  
  Murlong_2018to2021_Cum_ANOVA,
  Ouyen_Spade_2017to2020_Cum_ANOVA,
  Sherwood_2019to2021_Cum_ANOVA,
  Telopea_Downs_2020to2021_Cum_ANOVA,
  Tempy_2019to2020_Cum_ANOVA, #"TempyX2021", #no yield
  Waikerie_2018to2020_Cum_ANOVA,
  Warnertown_2019to2021_Cum_ANOVA,
  Wynarka_2019to2021_Cum_ANOVA,
  Yenda_2017to2021_Cum_ANOVA,
  Younghusband_2020_Cum_ANOVA
  
  )
  
  

rm( `Brimpton Lake_2014to2018_Cum_ANOVA`,
    Brooker_2019to2021_Cum_ANOVA,
    Buckleboo_2019to2021_Cum_ANOVA,
    Bute_CSIRO_2018to2021_Cum_ANOVA,
    Bute_Trengrove_2015to2021_Cum_ANOVA,
    Cadgee_2014to2018_Cum_ANOVA,
    Carwarp_Amelioration_2018to2020_Cum_ANOVA,
    Cummins_2019to2021_Cum_ANOVA,
    Karkoo_2019to2021_Cum_ANOVA,
    Karoonda_2014to2018_Cum_ANOVA,
    Kooloonong_canola_2021_Cum_ANOVA,
    Kooloonong_chickpea_2019to2021_Cum_ANOVA,
    Kooloonong_lentil_2019to2021_Cum_ANOVA,
    Kooloonong_lupin_2019to2021_Cum_ANOVA,
    Kybunga_2019to2021_Cum_ANOVA,
    Lowaldie_Crest_2019to2020_Cum_ANOVA,
    `Lowaldie_Deep sand_2019to2020_Cum_ANOVA`,
    Malinong_2019to2021_Cum_ANOVA,
    Monia_Gap_2019to2021_Cum_ANOVA,
    `Mt Damper_2019to2021_Cum_ANOVA`,
    Murlong_2018to2021_Cum_ANOVA,
    Ouyen_Spade_2017to2020_Cum_ANOVA,
    Sherwood_2019to2021_Cum_ANOVA,
    Telopea_Downs_2020to2021_Cum_ANOVA,
    Tempy_2019to2020_Cum_ANOVA, #"TempyX2021", #no yield
    Waikerie_2018to2020_Cum_ANOVA,
    Warnertown_2019to2021_Cum_ANOVA,
    Wynarka_2019to2021_Cum_ANOVA,
    Yenda_2017to2021_Cum_ANOVA,
    Younghusband_2020_Cum_ANOVA)




write.csv(Cum_ANOVA_sites_yr,"X:/Therese_Jackie/Sandy_soils/Development_database/stats_batch_output/Cum_ANOVA_sites_merged.csv" ,
          row.names = FALSE)






















