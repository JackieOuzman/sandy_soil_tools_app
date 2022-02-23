

impact_sites_step1_2_all <- 
  read_csv("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2/impact_sites_step1_2.csv", 
           col_types = cols(yield = col_double(),
                            control_yield = col_double()))


impact_sites_step1_2_all <- impact_sites_step1_2_all %>% 
  select(site,
  site_sub,
  Latitude,
  Longitude,
  Repellence,
  Acidity,
  Physical,
  Nutrient,
  other,
  Amelioration_Date,Amelioration_Year)  

research_sites_sites_step1_2_all <-
  read.csv("X:/Therese_Jackie/Sandy_soils/Development_database/other_sites_working/step2/research_sites_sites_step1_2.csv")

research_sites_sites_step1_2_all <- research_sites_sites_step1_2_all %>% 
  select(site,
         site_sub,
         Latitude,
         Longitude,
         Repellence,
         Acidity,
         Physical,
         Nutrient,
         other,
         Amelioration_Date,Amelioration_Year)  



sites_merged_all <- rbind(
  impact_sites_step1_2_all,
  research_sites_sites_step1_2_all
)

metedata_sites <- sites_merged_all %>% 
  distinct(site_sub, .keep_all = TRUE) 



list_sites_ready <- c("Brimpton Lake",
                      "Brooker",
                      "Buckleboo",
                      "Bute_CSIRO",
                      "Bute_Trengrove",
                      "Cadgee",
                      "CarwarpAmelioration",
                      "Cummins",
                      "Karkoo",
                      "Karoonda",
                      #"Kooloonong_canola",
                      "Kooloonong_chickpea",
                      "Kooloonong_lentil",
                      "Kooloonong_lupin",
                      "Lowaldie_Crest",
                      "Lowaldie_Deep sand",
                      "Mt_Damper",
                      "Murlong",
                      "Ouyen_spade",
                      "Telopea_Downs",
                      "Tempy",
                      "Waikerie",
                      "Wynarka",
                      "Yenda",
                      "Younghusband")


metedata_sites_ready <- metedata_sites %>% 
  filter(site_sub %in% list_sites_ready  )

names(metedata_sites_ready)


metedata_sites_ready <- metedata_sites_ready %>% 
  select(- site_sub )
write.csv(metedata_sites_ready, "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/metadata.csv",row.names = FALSE )
