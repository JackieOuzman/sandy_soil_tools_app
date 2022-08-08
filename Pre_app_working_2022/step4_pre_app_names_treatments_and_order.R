## create a df which has all the treatments and names of what they mean:


library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)

DB_df <- read.csv("X:/Therese_Jackie/Sandy_soils/Development_database/completeDB/sites_merged_all_messy.csv")
names(DB_df)
list_of_Descriptors <- DB_df %>% distinct( site, Descriptors, .keep_all = TRUE) %>% 
  select(site, Descriptors)

list_of_Descriptors <- list_of_Descriptors %>% 
  mutate(
    detailed_name = case_when(
      site=="Kooloonong_chickpea"& Descriptors == "Unmodified_Cl.surface"~"chicken litter applied surface",
      site=="Kooloonong_chickpea"& Descriptors == "Control"~"Control",
      site=="Kooloonong_chickpea"& Descriptors == "Rip.50_none"~"Ripping to 50cm with no amendments",
      site=="Kooloonong_chickpea"& Descriptors == "Rip.50_Cl.surface"~"Ripping to 50cm with chicken litter applied surface",
      site=="Kooloonong_lentil"& Descriptors == "Control"~"Control",
      site=="Kooloonong_lentil"& Descriptors == "Unmodified_Cl.surface"~"chicken litter applied surface",
      site=="Kooloonong_lentil"& Descriptors == "Rip.50_Cl.surface"~"Ripping to 50cm with chicken litter applied surface",
      site=="Kooloonong_lentil"& Descriptors == "Rip.50_none"~"Ripping to 50cm with no amendments",
      site=="Kooloonong_lupin"& Descriptors == "Control"~"Control",
      site=="Kooloonong_lupin"& Descriptors == "Unmodified_Cl.surface"~"chicken litter applied surface",
      site=="Kooloonong_lupin"& Descriptors == "Rip.50_Cl.surface"~"Ripping to 50cm with chicken litter applied surface",
      site=="Kooloonong_lupin"& Descriptors == "Rip.50_none"~"Ripping to 50cm with no amendments",
      site=="Tempy"& Descriptors == "Control"~"Control",
      site=="Tempy"& Descriptors == "Rip.50_none"~"Ripping to 50cm with no amendments",
      site=="Tempy"& Descriptors == "Rip.50IncRip_none"~"Ripping to 50cm and inclusion with no amendments",
      site=="Tempy"& Descriptors == "Rip.50IncRip_Cl.surface"~"Ripping to 50cm and inclusion with chicken litter applied surface",
      site=="Tempy"& Descriptors == "Rip.50_Cl.surface"~"Ripping to 50cm with chicken litter applied surface",
      site=="Wynarka"& Descriptors == "Control"~"Control",
      site=="Wynarka"& Descriptors == "Rip.60IncRip_none"~"Ripping to 60cm and inclusion with no amendments",
      site=="Wynarka"& Descriptors == "Rip.60IncRip+Spade.30_none"~"Ripping to 60cm and inclusion and spading 30cm with no amendments",
      site=="Wynarka"& Descriptors == "Rip.30_none"~"Ripping to 30cm with no amendments",
      site=="Wynarka"& Descriptors == "Rip.60_none"~"Ripping to 60cm with no amendments",
      site=="Wynarka"& Descriptors == "Rip.60Spade.30_none"~"Ripping to 30cm and spading 30cm with no amendments",
      site=="Wynarka"& Descriptors == "Spade.30_none"~"spade 30cm with no amendments",
      site=="Monia_Gap"& Descriptors == "Spade.30_Gypsum.incorp_30"~"spade 30cm with gypsum incorporated 30cm",
      site=="Monia_Gap"& Descriptors == "Spade.30_Cl.incorp_30"~"spade 30cm with chicken litter incorporated 30cm",
      site=="Monia_Gap"& Descriptors == "Rip.60Spade.30_none"~"Ripping to 30cm and spading 30cm with no amendments",
      site=="Monia_Gap"& Descriptors == "Spade.30_Cl.incorp_30.Gypsum.incorp_30"~"spade 30cm with chicken litter and gypsum incorporated 30cm",
      site=="Monia_Gap"& Descriptors == "Spade.30_none"~"spade 30cm with no amendments",
      site=="Monia_Gap"& Descriptors == "Control"~"Control",
      site=="Malinong"& Descriptors == "Control"~"Control",
      site=="Malinong"& Descriptors == "Spade.30_none"~"spade 30cm with no amendments",
      site=="Malinong"& Descriptors == "DiscInv.30_none"~"Disc inversion to 30cm with no amendments",
      site=="Malinong"& Descriptors == "Rip.30_none"~"Ripping to 30cm with no amendments",
      site=="Malinong"& Descriptors == "Rip.40_none"~"Ripping to 40cm with no amendments",
      site=="Sherwood"& Descriptors == "Control"~"Control",
      site=="Sherwood"& Descriptors == "Rip.30_none"~"Ripping to 30cm with no amendments",
      site=="Sherwood"& Descriptors == "Spade.30_none"~"spade 30cm with no amendments",
      site=="Sherwood"& Descriptors == "Rip.30_Fert.surface"~"Ripping to 30cm with fertiliser applied at surface",
      site=="Sherwood"& Descriptors == "Spade.30_Fert.incorp_30"~"spade 30cm with fertiliser incorporated 30cm",
      site=="Cummins"& Descriptors == "Control"~"Control",
      site=="Cummins"& Descriptors == "Rip.30_none"~"Ripping to 30cm with no amendments",
      site=="Cummins"& Descriptors == "Rip.30IncRip_none"~"Ripping to 30cm and inclusion with no amendments",
      site=="Cummins"& Descriptors == "Rip.30IncRip_Gypsum.incorp_30"~"Ripping to 30cm and inclusion with Gypsum incorporated 30cm",
      site=="Karkoo"& Descriptors == "Control"~"Control",
      site=="Karkoo"& Descriptors == "Rip.40_none"~"Ripping to 40cm with no amendments",
      site=="Karkoo"& Descriptors == "Rip.40IncRip_none"~"Ripping to 40cm and inclusion with no amendments",
      site=="Karkoo"& Descriptors == "Rip.40IncRip_Lc.incorp_40"~"Ripping to 40cm and inclusion with chicken litter incorporated 40cm",
      site=="Buckleboo"& Descriptors == "Rip.35_none"~"Ripping to 35cm with no amendments",
      site=="Buckleboo"& Descriptors == "Rip.45_none"~"Ripping to 45cm with no amendments",
      site=="Buckleboo"& Descriptors == "Rip.45IncRip_none"~"Ripping to 45cm and inclusion with no amendments",
      site=="Buckleboo"& Descriptors == "Rip.45IncRip_Fert_High.band_45"~"Ripping to 45cm and inclusion with fertiliser (high rate) banded 45cm",
      site=="Buckleboo"& Descriptors == "Rip.45IncRip_Fert_Low.band_45"~"Ripping to 45cm and inclusion with fertiliser (low rate) banded 45cm",
      site=="Buckleboo"& Descriptors == "Rip.45IncRip_Fert_APP.band_45"~"Ripping to 45cm and inclusion with fertiliser (app) banded 45cm",
      site=="Buckleboo"& Descriptors == "Control"~"Control",
      site=="Mt Damper"& Descriptors == "Control"~"Control",
      site=="Mt Damper"& Descriptors == "Spade.30_none"~"spade 30cm with no amendments",
      site=="Mt Damper"& Descriptors == "Rip.45IncRip_none"~"Ripping to 45cm and inclusion with no amendments",
      site=="Mt Damper"& Descriptors == "Rip.45IncRip+Spade.30_none"~"Ripping to 45cm and inclusion and spading 30cm with no amendments",
      site=="Mt Damper"& Descriptors == "Rip.45IncRip_Fert.incorp_45"~"Ripping to 45cm and inclusion with fertiliser banded 45cm",
      site=="Kybunga"& Descriptors == "Control"~"Control",
      site=="Kybunga"& Descriptors == "Rip.30_none"~"Ripping to 30cm with no amendments",
      site=="Kybunga"& Descriptors == "Rip.50_none"~"Ripping to 50cm with no amendments",
      site=="Kybunga"& Descriptors == "Spade.30_none"~"spade 30cm with no amendments",
      site=="Kybunga"& Descriptors == "Rip.50Spade.30_none"~"Ripping to 50cm and spading 30cm with no amendments",
      site=="Kybunga"& Descriptors == "Rip.50_Cl.surface"~"Ripping to 50cm with chicken litter applied surface",
      site=="Kybunga"& Descriptors == "Spade.30_Cl.incorp_30"~"spade 30cm with chicken litter incorporated 30cm",
      site=="Warnertown"& Descriptors == "DiscInv.30_none"~"Disc inversion to 30cm with no amendments",
      site=="Warnertown"& Descriptors == "Rip.50_none"~"Ripping to 50cm with no amendments",
      site=="Warnertown"& Descriptors == "Rip.50IncRip_none"~"Ripping to 50cm and inclusion with no amendments",
      site=="Warnertown"& Descriptors == "Spade.30_none"~"spade 30cm with no amendments",
      site=="Warnertown"& Descriptors == "Rip.30_none"~"Ripping to 30cm with no amendments",
      site=="Warnertown"& Descriptors == "Control"~"Control",
      site=="Telopea_Downs"& Descriptors == "Spade.30_Clay@250.incorp_30"~"spade 30cm with clay at 250t/ha",
      site=="Telopea_Downs"& Descriptors == "Spade.30_Clay@500.incorp_30_Yr07,20"~"spade 30cm with clay at 250t/ha in years 2007 and 2020",
      site=="Telopea_Downs"& Descriptors == "Control"~"Control",
      site=="Kooloonong_canola"& Descriptors == "Unmodified_Cl.surface"~"chicken litter applied surface",
      site=="Kooloonong_canola"& Descriptors == "Rip.50_none"~"Ripping to 50cm with no amendments",
      site=="Kooloonong_canola"& Descriptors == "Control"~"Control",
      site=="Kooloonong_canola"& Descriptors == "Rip.50_Cl.surface"~"Ripping to 50cm with chicken litter applied surface",
      site=="Brooker"& Descriptors == "Spade.30_Lc@8.incorp_30.K_added.surface"~"spade 30cm with lucerne at 8t/ha incorporated 30cm and potassium applied at surface",
      site=="Brooker"& Descriptors == "Spade.30_Lc@8.incorp_30"~"spade 30cm with lucerne at 8t/ha incorporated 30cm",
      site=="Brooker"& Descriptors == "Spade.30_none"~"spade 30cm with no amendments",
      site=="Brooker"& Descriptors == "Spade.30_K_added.surface"~"spade 30cm with potassium applied at surface",
      site=="Brooker"& Descriptors == "Unmodified_K_added.surface"~"potassium applied surface",
      site=="Brooker"& Descriptors == "Control"~"Control",
      site=="Brooker"& Descriptors == "Spade.30_Fert.incorp_30"~"spade 30cm with fertiliser incorporated 30cm",
      site=="Brooker"& Descriptors == "Spade.30_Fert.incorp_30.K_added.incorp_30"~"spade 30cm with fertiliser and potassium incorporated 30cm",
      site=="Brooker"& Descriptors == "Spade.30_Lc@15.incorp_30.K_added.surface"~"spade 30cm with lucerne at 15t/ha incorporated 30cm and potassium applied at surface",
      site=="Brooker"& Descriptors == "Spade.30_Lc@15.incorp_30"~"spade 30cm with lucerne at 15t/ha incorporated 30cm",
      site=="Brooker"& Descriptors == "Spade.30_Lc@4.incorp_30"~"spade 30cm with lucerne at 4t/ha incorporated 30cm",
      site=="Brooker"& Descriptors == "Spade.30_Lc@4.incorp_30.K_added.surface"~"spade 30cm with lucerne at 4t/ha incorporated 30cm and potassium applied at surface",
      site=="Brooker"& Descriptors == "Spade.30_Lc@1.incorp_30.K_added.surface"~"spade 30cm with lucerne at 1t/ha incorporated 30cm and potassium applied at surface",
      site=="Brooker"& Descriptors == "Spade.30_Lc@1.incorp_30"~"spade 30cm with lucerne at 1t/ha incorporated 30cm",
      site=="Brooker"& Descriptors == "Spade.30_Lc@10.incorp_30.K_added.surface"~"spade 30cm with lucerne at 10t/ha incorporated 30cm and potassium applied at surface",
      site=="Brooker"& Descriptors == "Spade.30_Lc@10.incorp_30"~"spade 30cm with lucerne at 10t/ha incorporated 30cm",
      site=="Brooker"& Descriptors == "Spade.30_Lc@6.incorp_30"~"spade 30cm with lucerne at 6t/ha incorporated 30cm",
      site=="Brooker"& Descriptors == "Spade.30_Lc@6.incorp_30.K_added.surface"~"spade 30cm with lucerne at 6t/ha incorporated 30cm and potassium applied at surface",
      site=="Brooker"& Descriptors == "Spade.30_Lc@2.incorp_30"~"spade 30cm with lucerne at 2t/ha incorporated 30cm",
      site=="Brooker"& Descriptors == "Spade.30_Lc@2.incorp_30.K_added.surface"~"spade 30cm with lucerne at 2t/ha incorporated 30cm and potassium applied at surface",
      site=="Brooker"& Descriptors == "Spade.30_Lc@20.incorp_30.K_added.surface"~"spade 30cm with lucerne at 20t/ha incorporated 30cm and potassium applied at surface",
      site=="Brooker"& Descriptors == "Spade.30_Lc@20.incorp_30"~"spade 30cm with lucerne at 20t/ha incorporated 30cm",
      site=="Bute_CSIRO"& Descriptors == "Rip.30_none"~"Ripping to 30cm with no amendments",
      site=="Bute_CSIRO"& Descriptors == "Rip.50_Cl@7.5.band_50"~"Ripping to 50cm with chicken litter at 7.5t/ha banded 50cm",
      site=="Bute_CSIRO"& Descriptors == "Unmodified_Cl@2.5.surface_Yr18,19,20"~"chicken litter at 2.5t/ha applied surface in years 2018, 2019, 2020",
      site=="Bute_CSIRO"& Descriptors == "Rip.50IncRip_none"~"Ripping to 50cm and inclusion with no amendments",
      site=="Bute_CSIRO"& Descriptors == "Rip.50_Cl@2.5.surface_Yr18,19,20"~"Ripping to 50cm with chicken litter at 2.5t/ha applied at surface in years 2018, 2019, 2020",
      site=="Bute_CSIRO"& Descriptors == "Rip.50_none"~"Ripping to 50cm with no amendments",
      site=="Bute_CSIRO"& Descriptors == "Rip.50IncRip_Cl@7.5.incorp_50"~"Ripping to 50cm and inclusion with chicken litter incorporated 50cm",
      site=="Bute_CSIRO"& Descriptors == "Rip.30_Cl@7.5.surface"~"Ripping to 30cm with chicken litter @ 7.5t/ha applied at surface",
      site=="Bute_CSIRO"& Descriptors == "Rip.50_Cl@7.5.surface"~"Ripping to 50cm with chicken litter at 7.5t/ha applied at surface",
      site=="Bute_CSIRO"& Descriptors == "Rip.30_Cl@7.5.band_30"~"Ripping to 30cm with chicken litter @ 7.5t/ha banded at 30cm",
      site=="Bute_CSIRO"& Descriptors == "Control"~"Control",
      site=="Bute_CSIRO"& Descriptors == "Unmodified_Cl@7.5.surface"~"chicken litter at 7.5t/ha applied surface",
      site=="Bute_Trengrove"& Descriptors == "Unmodified_Cl@20.incorp_8"~"chicken litter at 20t/ha incorporated 8cm",
      site=="Bute_Trengrove"& Descriptors == "Rip.50_Cl@20.incorp_20.Fert.surface"~"Ripping to 50cm with chicken litter at 20t/ha incorporated 20 cm and fertiliser applied at surface",
      site=="Bute_Trengrove"& Descriptors == "Unmodified_Clay.incorp_8"~"clay incorporated 8cm",
      site=="Bute_Trengrove"& Descriptors == "Control"~"Control",
      site=="Bute_Trengrove"& Descriptors == "Rip.50_Cl@5.incorp_20.Clay.incorp_20"~"Ripping to 50cm with chicken litter at 5t/ha incorporated 20 cm and clay incorporated 20cm",
      site=="Bute_Trengrove"& Descriptors == "Rip.50_Cl@5.incorp_20.Fert.surface"~"Ripping to 50cm with chicken litter at 5t/ha incorporated 20 cm and fertiliser applied at surface",
      site=="Bute_Trengrove"& Descriptors == "Unmodified_Cl@5.incorp_8"~"chicken litter at 5t/ha incorporated 8cm",
      site=="Bute_Trengrove"& Descriptors == "Unmodified_Cl@20.incorp_8.Fert.surface.Clay.incorp_8"~"chicken litter at 20t/ha and fertiliser and clay incorporated 8cm",
      site=="Bute_Trengrove"& Descriptors == "Rip.50_Fert.surface"~"Ripping to 50cm with fertiliser applied at surface",
      site=="Bute_Trengrove"& Descriptors == "Unmodified_Cl@5.incorp_8.Fert.surface"~"chicken litter at 5t/ha incorporated 8cm and fertiliser applied at surface",
      site=="Bute_Trengrove"& Descriptors == "Unmodified_Cl@5.incorp_8.Fert.surface.Clay.incorp_8"~"chicken litter at 5t/ha and fertiliser and clay incorporated 8cm",
      site=="Bute_Trengrove"& Descriptors == "Unmodified_Fert.surface"~"fertiliser applied surface",
      site=="Bute_Trengrove"& Descriptors == "Unmodified_Fert.surface.Clay.incorp_8"~"fertiliser and clay incorporated 8cm",
      site=="Bute_Trengrove"& Descriptors == "Rip.50_none"~"Ripping to 50cm with no amendments",
      site=="Bute_Trengrove"& Descriptors == "Unmodified_Cl@5.incorp_8.Clay.incorp_8"~"chicken litter at 5t/ha and clay incorporated 8cm",
      site=="Bute_Trengrove"& Descriptors == "Rip.50_Cl@20.incorp_20.Fert.surface.Clay.incorp_20"~"Ripping to 50cm with chicken litter at 20t/ha incorporated 20 cm and fertiliser applied at surface and clay incorporated 20cm",
      site=="Bute_Trengrove"& Descriptors == "Unmodified_Cl@20.incorp_8.Clay.incorp_8"~"chicken litter at 20t/ha and clay incorporated 8cm",
      site=="Bute_Trengrove"& Descriptors == "Rip.50_Fert.surface.Clay.incorp_20"~"Ripping to 50cm with fertiliser applied at surface and clay incorporated 20cm",
      site=="Bute_Trengrove"& Descriptors == "Unmodified_Cl@20.incorp_8.Fert.surface"~"chicken litter at 20t/ha and fertiliser applied at surface",
      site=="Bute_Trengrove"& Descriptors == "Rip.50_Cl@20.incorp_20.Clay.incorp_20"~"Ripping to 50cm with chicken litter at 20t/ha incorporated 20 cm and clay incorporated 20cm",
      site=="Bute_Trengrove"& Descriptors == "Rip.50_Cl@5.incorp_20"~"Ripping to 50cm with chicken litter at 5t/ha incorporated 20 cm",
      site=="Bute_Trengrove"& Descriptors == "Rip.50_Cl@5.incorp_20.Fert.surface.Clay.incorp_20"~"Ripping to 50cm with chicken litter at 5t/ha incorporated 20 cm and fertiliser applied at surface and clay incorporated 20cm",
      site=="Bute_Trengrove"& Descriptors == "Rip.50_Clay.incorp_20"~"Ripping to 50cm with clay incorporated 20cm",
      site=="Bute_Trengrove"& Descriptors == "Rip.50_Cl@20.incorp_20"~"Ripping to 50cm with chicken litter at 20t/ha incorporated 20 cm",
      site=="Carwarp_Amelioration"& Descriptors == "Spade.30_Lc.incorp_30"~"spade 30cm with lucerne incorporated 30cm",
      site=="Carwarp_Amelioration"& Descriptors == "Spade.30_none"~"spade 30cm with no amendments",
      site=="Carwarp_Amelioration"& Descriptors == "Rip.60Spade.30_Lc.incorp_30+band_60"~"Ripping to 60cm and spading 30cm with lucerne incorporated 30cm and banded 60cm",
      site=="Carwarp_Amelioration"& Descriptors == "Rip.60Spade.30_none"~"Ripping to 60cm and spading 30cm with no amendments",
      site=="Carwarp_Amelioration"& Descriptors == "Rip.30+60_none"~"Ripping to 30cm and 60cm with no amendments",
      site=="Carwarp_Amelioration"& Descriptors == "Rip.30+60_Lc.band_30+60"~"Ripping to 30cm and 60cm with lucerne banded 30cm and 60cm",
      site=="Carwarp_Amelioration"& Descriptors == "Rip.60_Lc.band_60"~"Ripping to 60cm with lucerne banded 60cm",
      site=="Carwarp_Amelioration"& Descriptors == "Rip.60_none"~"Ripping to 60cm with no amendments",
      site=="Carwarp_Amelioration"& Descriptors == "Rip.30_none"~"Ripping to 30cm with no amendments",
      site=="Carwarp_Amelioration"& Descriptors == "Rip.30_Lc.band_30"~"Ripping to 30cm with lucerne banded 30cm",
      site=="Carwarp_Amelioration"& Descriptors == "Control"~"Control",
      site=="Carwarp_Amelioration"& Descriptors == "Unmodified_Lc.surface"~"lucerne applied surface",
      site=="Lowaldie_Crest"& Descriptors == "Control"~"Control",
      site=="Lowaldie_Crest"& Descriptors == "Rip.40_none"~"Ripping to 40cm with no amendments",
      site=="Lowaldie_Crest"& Descriptors == "Rip.60_none"~"Ripping to 60cm with no amendments",
      site=="Lowaldie_Deep sand"& Descriptors == "Control"~"Control",
      site=="Lowaldie_Deep sand"& Descriptors == "Rip.40_none"~"Ripping to 40cm with no amendments",
      site=="Lowaldie_Deep sand"& Descriptors == "Rip.60_none"~"Ripping to 60cm with no amendments",
      site=="Murlong"& Descriptors == "Rip.40_none"~"Ripping to 40cm with no amendments",
      site=="Murlong"& Descriptors == "Rip.30_Lc.incorp_30"~"Ripping to 30cm with lucerne incorporated 30cm",
      site=="Murlong"& Descriptors == "Rip.30_Fert.incorp_30"~"Ripping to 30cm with fertiliser incorporated 30cm",
      site=="Murlong"& Descriptors == "Spade.30_Lc.incorp_30"~"spade 30cm with lucerne incorporated 30cm",
      site=="Murlong"& Descriptors == "Rip.40_Lc.incorp_40"~"Ripping to 40cm with lucerne incorporated 40cm",
      site=="Murlong"& Descriptors == "Control"~"Control",
      site=="Murlong"& Descriptors == "Rip.40_Fert.incorp_40"~"Ripping to 40cm with fertiliser incorporated 40cm",
      site=="Murlong"& Descriptors == "Spade.30_none"~"spade 30cm with no amendments",
      site=="Murlong"& Descriptors == "Spade.30_Fert.incorp_30"~"spade 30cm with fertiliser incorporated 30cm",
      site=="Murlong"& Descriptors == "Rip.30_none"~"Ripping to 30cm with no amendments",
      site=="Brimpton Lake"& Descriptors == "Control"~"Control",
      site=="Brimpton Lake"& Descriptors == "Spade.30_Lc.incorp_30"~"spade 30cm with lucerne incorporated 30cm",
      site=="Brimpton Lake"& Descriptors == "Spade.30_none"~"spade 30cm with no amendments",
      site=="Brimpton Lake"& Descriptors == "Spade.30_Fert.incorp_30"~"spade 30cm with fertiliser incorporated 30cm",
      site=="Brimpton Lake"& Descriptors == "Spade.30_Lc.incorp_30.Fert.incorp_30"~"spade 30cm with lucerne and fertiliser incorporated 30cm",
      site=="Brimpton Lake"& Descriptors == "Unmodified_Fert.band_30"~"fertiliser banded 30cm",
      site=="Brimpton Lake"& Descriptors == "Spade.30_Fert.incorp_30.Clay.incorp_30"~"spade 30cm with fertiliser and clay incorporated 30cm",
      site=="Brimpton Lake"& Descriptors == "Spade.30_Clay.incorp_30"~"spade 30cm with clay incorporated 30cm",
      site=="Brimpton Lake"& Descriptors == "Unmodified_Fert.band_30.Clay.incorp_10"~"fertiliser banded 30cm and clay incorporated 10cm",
      site=="Brimpton Lake"& Descriptors == "Spade.30_Lc.incorp_30.Clay.incorp_30"~"spade 30cm with lucerne and clay incorporated 30cm",
      site=="Brimpton Lake"& Descriptors == "Unmodified_Clay.incorp_10"~"clay incorporated 10cm",
      site=="Brimpton Lake"& Descriptors == "Spade.30_Lc.incorp_30.Fert.incorp_30.Clay.incorp_30"~"spade 30cm with ulcerous and fertiliser and clay incorporated 30cm",
      site=="Cadgee"& Descriptors == "Unmodified_Fert.band_30.Clay.incorp_10"~"fertiliser banded 30cm and clay incorporated 10cm",
      site=="Cadgee"& Descriptors == "Spade.30_Lc.incorp_30.Fert.incorp_30.Clay.incorp_30"~"spade 30cm with ulcerous and fertiliser and clay incorporated 30cm",
      site=="Cadgee"& Descriptors == "Spade.30_Lc.incorp_30"~"spade 30cm with lucerne incorporated 30cm",
      site=="Cadgee"& Descriptors == "Spade.30_Fert.incorp_30"~"spade 30cm with fertiliser incorporated 30cm",
      site=="Cadgee"& Descriptors == "Spade.30_none"~"spade 30cm with no amendments",
      site=="Cadgee"& Descriptors == "Control"~"Control",
      site=="Cadgee"& Descriptors == "Spade.30_Fert.incorp_30.Clay.incorp_30"~"spade 30cm with fertiliser and clay incorporated 30cm",
      site=="Cadgee"& Descriptors == "Spade.30_Clay.incorp_30"~"spade 30cm with clay incorporated 30cm",
      site=="Cadgee"& Descriptors == "Spade.30_Lc.incorp_30.Fert.incorp_30"~"spade 30cm with ulcerous and fertiliser incorporated 30cm",
      site=="Cadgee"& Descriptors == "Unmodified_Clay.incorp_10"~"clay incorporated 10cm",
      site=="Cadgee"& Descriptors == "Unmodified_Fert.band_30"~"fertiliser banded 30cm",
      site=="Cadgee"& Descriptors == "Spade.30_Lc.incorp_30.Clay.incorp_30"~"spade 30cm with lucerne and clay incorporated 30cm",
      site=="Karoonda"& Descriptors == "Spade.30_Fert.incorp_30.Clay.incorp_30"~"spade 30cm with fertiliser and clay incorporated 30cm",
      site=="Karoonda"& Descriptors == "Unmodified_Fert.band_30.Clay.incorp_10"~"fertiliser banded 30cm and clay incorporated 10cm",
      site=="Karoonda"& Descriptors == "Spade.30_Lc.incorp_30"~"spade 30cm with lucerne incorporated 30cm",
      site=="Karoonda"& Descriptors == "Spade.30_Fert.incorp_30"~"spade 30cm with fertiliser incorporated 30cm",
      site=="Karoonda"& Descriptors == "Spade.30_none"~"spade 30cm with no amendments",
      site=="Karoonda"& Descriptors == "Control"~"Control",
      site=="Karoonda"& Descriptors == "Unmodified_Clay.incorp_10"~"clay incorporated 10cm",
      site=="Karoonda"& Descriptors == "Spade.30_Lc.incorp_30.Fert.incorp_30.Clay.incorp_30"~"spade 30cm with ulcerous and fertiliser and clay incorporated 30cm",
      site=="Karoonda"& Descriptors == "Spade.30_Lc.incorp_30.Fert.incorp_30"~"spade 30cm with ulcerous and fertiliser incorporated 30cm",
      site=="Karoonda"& Descriptors == "Spade.30_Clay.incorp_30"~"spade 30cm with clay incorporated 30cm",
      site=="Karoonda"& Descriptors == "Unmodified_Fert.band_30"~"fertiliser banded 30cm",
      site=="Karoonda"& Descriptors == "Spade.30_Lc.incorp_30.Clay.incorp_30"~"spade 30cm with lucerne and clay incorporated 30cm",
      site=="Ouyen_Spade"& Descriptors == "Control"~"Control",
      site=="Ouyen_Spade"& Descriptors == "Spade.30_none"~"spade 30cm with no amendments",
      site=="Ouyen_Spade"& Descriptors == "Spade.30_Cereal.incorp_30"~"spade 30cm with cereal incorporated 30cm",
      site=="Ouyen_Spade"& Descriptors == "Spade.30_Vetch.incorp_30"~"spade 30cm with vetch incorporated 30cm",
      site=="Ouyen_Spade"& Descriptors == "Spade.30_Fert.incorp_30"~"spade 30cm with fertiliser incorporated 30cm",
      site=="Ouyen_Spade"& Descriptors == "Spade.30_Vet_Cer.incorp_30"~"spade 30cm with vetch and cereal incorporated 30cm",
      site=="Ouyen_Spade"& Descriptors == "Spade.30_Vet_Cer_In.incorp_30"~"spade 30cm with vetch and cereal and inoculant incorporated 30cm",
      site=="Ouyen_Spade"& Descriptors == "Spade.30_Com.incorp_30"~"spade 30cm with compost incorporated 30cm",
      site=="Ouyen_Spade"& Descriptors == "Spade.30_Cl.incorp_30"~"spade 30cm with chicken litter incorporated 30cm",
      site=="Waikerie"& Descriptors == "Control"~"Control",
      site=="Waikerie"& Descriptors == "Unmodified_Cl.surface"~"chicken litter applied at surface",
      site=="Waikerie"& Descriptors == "Unmodified_Fert.band_8"~"fertiliser banded 8cm",
      site=="Waikerie"& Descriptors == "Rip.30_none"~"Ripping to 30cm with no amendments",
      site=="Waikerie"& Descriptors == "Rip.30_Cl.surface"~"Ripping to 30cm with chicken litter applied at surface",
      site=="Waikerie"& Descriptors == "Rip.30_Fert.band_8"~"Ripping to 30cm with fertiliser banded at 8cm",
      site=="Waikerie"& Descriptors == "Rip.30_Cl.band_30"~"Ripping to 30cm with chicken litter banded at 30cm",
      site=="Waikerie"& Descriptors == "Rip.30_Fert.band_30"~"Ripping to 30cm with fertiliser banded at 30cm",
      site=="Waikerie"& Descriptors == "Rip.60_Cl.surface"~"Ripping to 60cm with chicken litter applied at surface",
      site=="Waikerie"& Descriptors == "Rip.60_Fert.band_8"~"Ripping to 60cm with fertiliser banded 8cm",
      site=="Waikerie"& Descriptors == "Rip.60_Cl.band_60"~"Ripping to 60cm with chicken litter banded 60cm",
      site=="Waikerie"& Descriptors == "Rip.60_Fert.band_60"~"Ripping to 60cm with fertiliser banded 60cm",
      site=="Waikerie"& Descriptors == "Rip.60_none"~"Ripping to 60cm with no amendments",
      site=="Yenda"& Descriptors == "Sweep.30_Lime.incorp_30"~"sweep 30cm with lime incorporated 30cm",
      site=="Yenda"& Descriptors == "Sweep.30_Cl@9.incorp_30"~"sweep 30cm with chicken litter at 9t/ha incorporated 30cm",
      site=="Yenda"& Descriptors == "Sweep.30_Cl@9.incorp_30_Yr17,18,19"~"sweep 30cm with chicken litter at 9t/ha incorporated 30cm applied 2017, 2018, 2019",
      site=="Yenda"& Descriptors == "Sweep.30_Cl@3.incorp_30"~"sweep 30cm with chicken litter at 3t/ha incorporated 30cm",
      site=="Yenda"& Descriptors == "Unmodified_Cl@3.incorp_8"~"chicken litter at 3t/ha incorporated 8cm",
      site=="Yenda"& Descriptors == "Rip.60_none"~"Ripping to 60cm with no amendments",
      site=="Yenda"& Descriptors == "Sweep.30_none"~"sweep 30cm with no amendments",
      site=="Yenda"& Descriptors == "Unmodified_Fert.foliar"~"fertiliser applied to foliar",
      site=="Yenda"& Descriptors == "Control"~"Control",
      site=="Yenda"& Descriptors == "Sweep.30_Cl@6.incorp_30"~"sweep 30cm with chicken litter at 6t/ha incorporated 30cm",
      site=="Yenda"& Descriptors == "Sweep.30_Cl@3.incorp_30.Lime.incorp_8"~"sweep 30cm with chicken litter at 3t/ha incorporated 30cm and lime incorporated 8cm",
      site=="Yenda"& Descriptors == "Unmodified_Fert.incorp_8"~"fertiliser incorporated 8cm",
      site=="Younghusband"& Descriptors == "Unmodified+DeepTill.18_SE14.band_8"~"deep tillage 18cm with SE14 (wetter) banded 8cm",
      site=="Younghusband"& Descriptors == "Unmodified+DeepTill.18_none"~"deep tillage 18cm with no amendments",
      site=="Younghusband"& Descriptors == "Rip.50_none"~"Ripping to 50cm with no amendments",
      site=="Younghusband"& Descriptors == "Spade.30_none"~"spade 30cm with no amendments",
      site=="Younghusband"& Descriptors == "Rip.50IncRip_none"~"Ripping to 50cm and inclusion with no amendments",
      site=="Younghusband"& Descriptors == "Unmodified_SE14.band_8"~"SE14 (wetter) banded 8cm",
      site=="Younghusband"& Descriptors == "Control"~"Control",
      site=="Younghusband"& Descriptors == "Unmodified_Bi_Agra.surface+band_8"~"Bi Agra (wetter) surface and banded 8cm",
      site=="Younghusband"& Descriptors == "Unmodified+OnRow_none"~"on row sowing with no amendments",
      site=="Ouyen_Placement"& Descriptors == "Pre_drill_20+7.5_none"~"Pre drill to depth 20cm with growers standard fertiliser to 7.5cm with no additional amendments",
      site=="Ouyen_Placement"& Descriptors == "Unmodified_none.Fert.surface"~"growers standard fertiliser to surface with no additional amendments",
      site=="Ouyen_Placement"& Descriptors == "Rip_30+30_none_annual"~"Ripping to 30cm with growers standard fertiliser to 30cm with no amendments annually",
      site=="Ouyen_Placement"& Descriptors == "Pre_drill_20+20_none"~"Pre drill to depth 20cm with growers standard fertiliser to 20cm with no additional amendments",
      site=="Ouyen_Placement"& Descriptors == "Rip_30+30_Fert.banded_30_annual"~"Ripping to 30cm with growers standard fertiliser to 30cm with fertiliser banded 30cm annually",
      site=="Ouyen_Placement"& Descriptors == "Pre_drill_20+20_none_annual"~"Pre drill to depth 20cm with growers standard fertiliser to 20cm with no additional amendments performed annually",
      site=="Ouyen_Placement"& Descriptors == "Rip_30+30_Fert.banded_30"~"Ripping to 30cm with growers standard fertiliser to 30cm with fertiliser banded 30cm",
      site=="Ouyen_Placement"& Descriptors == "Control"~"Control",
      site=="Ouyen_Placement"& Descriptors == "Rip_30+7.5_none"~"Ripping to 30cm with growers standard fertiliser to 7.5cm with no amendments ",
      site=="Ouyen_Placement"& Descriptors == "Rip_30+7.5_none_annual"~"Ripping to 30cm with growers standard fertiliser to 7.5cm with no amendments annually",
      site=="Ouyen_Placement"& Descriptors == "Pre_drill_20+20_Fert.banded_20"~"Pre drill to depth 20cm with growers standard fertiliser to 20cm with fertiliser banded at 20cm",
      site=="Ouyen_Placement"& Descriptors == "Pre_drill_20+20_Fert.banded_20_annual"~"Pre drill to depth 20cm with growers standard fertiliser to 20cm with fertiliser banded at 20cm annually ",
      site=="Ouyen_Placement"& Descriptors == "Pre_drill_20+7.5_none_annual"~"Pre drill to depth 20cm with growers standard fertiliser to 7.5cm with no additional amendments annually",
      site=="Ouyen_Placement"& Descriptors == "Rip_30+30_none"~"Ripping to 30cm and 30cm with no amendments annually",
      
      TRUE                      ~ "ahhh"
    )
  )




order <- c(
  "Control",
  "Unmodified+OnRow_none",
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
  
  "Unmodified_Clay.incorp_8",
  "Unmodified_Clay.incorp_10",
  "Unmodified_none.Fert.surface", #ouyen DD
  
  "Pre_drill_20+7.5_none", #ouyen DD
  "Pre_drill_20+7.5_none_annual",
  "Pre_drill_20+20_none",
  "Pre_drill_20+20_none_annual",
  "Pre_drill_20+20_Fert.banded_20",
  "Pre_drill_20+20_Fert.banded_20_annual",
  
  "Spade.30_none",
  "Spade.30_Lc@1.incorp_30",
  "Spade.30_Lc@2.incorp_30",
  "Spade.30_Lc@4.incorp_30",
  "Spade.30_Lc@6.incorp_30",
  "Spade.30_Lc@8.incorp_30",
  "Spade.30_Lc@10.incorp_30",
  "Spade.30_Lc@15.incorp_30",
  
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
  
  "Rip_30+7.5_none", #ouyen DD
  "Rip_30+7.5_none_annual",
  "Rip_30+30_none",
  "Rip_30+30_none_annual",
  "Rip_30+30_Fert.banded_30",
  "Rip_30+30_Fert.banded_30_annual",
  
  "Sweep.30_none",
  "Sweep.30_Cl@3.incorp_30",
  "Sweep.30_Cl@6.incorp_30",
  "Sweep.30_Cl@9.incorp_30",
  "Sweep.30_Cl@9.incorp_30_Yr17,18,19",
  "Sweep.30_Lime.incorp_30",
  "Sweep.30_Cl@3.incorp_30.Lime.incorp_8",
  
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
  
  
  "Rip.50IncRip_none",
  "Rip.50IncRip_Cl.incorp_50",
  "Rip.50IncRip_Cl.surface",
  "Rip.50IncRip_Cl@7.5.incorp_50",
  
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
  
  
  
  
  "Unmodified+DeepTill.18_none",
  "Unmodified+DeepTill.18_SE14.band_8",
  
  "DiscInv.30_none"
  
)

order_df <- as.data.frame(order)

## add a oder index to this order_df.

order_df$order_rank <- 1:nrow(order_df)


### add the two datasets togther

str(order_df)
str(list_of_Descriptors)


list_of_Descriptors <- left_join(list_of_Descriptors,order_df, by = c("Descriptors" = "order"))




write.csv(list_of_Descriptors,
          "list_of_Descriptors_with_order_rank.csv",
          row.names = FALSE)


########################################################################################################################


list_of_Descriptors_pdf <- list_of_Descriptors %>% 
  distinct(order_rank, .keep_all= TRUE) %>% 
  arrange(order_rank) %>% 
  select(-site, -order_rank)

list_of_Descriptors_pdf <- list_of_Descriptors_pdf %>% 
rename("treatment codes" = "Descriptors",
       "treatment detailed name" = "detailed_name")
names(list_of_Descriptors_pdf)



#########
library(grid)
library(gridExtra)
head(list_of_Descriptors_pdf)

myTable <- head(list_of_Descriptors_pdf[,1:2])
grid.table(myTable)
grid.draw(tableGrob(myTable))

d <- head(list_of_Descriptors_pdf[,1:2])
grid.table(d)
library(gridExtra)
library(grid)
d <- head(iris[,1:3])
grid.table(d)


#Export to pdf
pdf("www/test.pdf",width = 10) # this is where the file will sit (relative paths so it avaialble to other computers)
grid.draw(tableGrob(myTable))
dev.off()



######

myTable <- tableGrob(
  S, 
  rows = NULL, 
  theme = ttheme_default(core = list(bg_params = list(fill = "grey99")))
)
#Export to pdf
pdf('Example.pdf',width = 10)
grid.draw(myTable)
dev.off()