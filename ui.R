
#bring in the library that I will be working with
library(shiny)
 library(ggplot2)
 library(readxl)
 library(tidyverse)
 library(rhandsontable)
 library(shinyalert)
 library(shinydashboard)
 library(htmltools)
 library(leaflet)
 library(slickR) #new install on land and water linex

library(shinyWidgets)

library(vroom) #new install on land and water linex

library(stringi)
library(plotly)




# Define UI 

# shinyUI(
#   fluidPage(title="Application Title",
#             tags$head(
#               # Script for CSIRO branding tab ----
#               tags$script(type="text/javascript", src="https://www.csiro.au/themes/default/js/csirotab.min.js"),
#             )
#   )
# )
  
#########################################################################################################
###############                       navbartitle                          #################################
#########################################################################################################

  
   ui <- (navbarPage(title = '', collapsible = TRUE,
                    
                     header = tagList(    
                       useShinydashboard(),
                       tags$head(
                         # Script for CSIRO branding tab ----
                         tags$script(type="text/javascript", src="https://www.csiro.au/themes/default/js/csirotab.min.js"),
                       )
                     ) ,   ## these two lines get the value box to display without using shinydashboards        
                     # 
                     #  header = tagList(    
                     #    useShinydashboard()
                     # ) ,   ## these two lines get the value box to display without using shinydashboards        

                     
                   
#########################################################################################################
#########################################################################################################
###############                      Introduction tab                     #################################
#########################################################################################################                 
                     
                     
                     tabPanel("Introduction", 
                              fluidPage( 
                                
                                fluidRow(
                                  box(
                                    title = "Welcome to Sandy soil App - Sandbox", width = 12, solidHeader = TRUE, status = "primary",
                                    tags$h2("Contributors:"),
                                      tags$i("Therese McBeath, Melissa Fraser, Jackie Ouzman, Masood Azeem, 
                                      Rodrigo Da Silva, Jack Desbiolles, Chris Saunders, Mustafa Ucgul, 
                                      Nigel Wilhelm, Michael Moodie, Tanja Morgan, Sam Trengove, 
                                      Stuart Sherriff, Rachael Whitworth, Murray Unkovich, Rick Llewellyn, Lynne Macdonald."),
                                    
                                    tags$h2("Acknowledgements:"),
                                    tags$p("This research has been enriched by preceding research trials, 
                                    the significant contributions of growers and consultants across the Southern region, 
                                    and the support of the GRDC. CSP00203 research and validation activities are a collaboration between the CSIRO, the University of South Australia, the SA state government through Primary Industries and Regions SA, Mallee Sustainable Farming Inc., Frontier Farming Systems, Trengove Consulting, AgGrow Agronomy, AirEP, and MacKillop Farm Management Group."),
                                    
                                    tags$h2("Licence:"),
                                    
                                    tags$p("Creative Commons Attribution-ShareAlike 4.0 International Licence.
                                    This license lets others remix, tweak, and build upon your work even for commercial purposes, 
                                    as long as they credit you and license their new creations under the identical terms. 
                                    This license is often compared to “copyleft” free and open source software licenses. 
                                    All new works based on yours will carry the same license, 
                                    so any derivatives will also allow commercial use. 
                                    This is the license used by Wikipedia, 
                                    and is recommended for materials that would benefit from incorporating content from Wikipedia and similarly licensed projects."),
                                    
                                    tags$a(href="https://creativecommons.org/licenses/by-sa/4.0/", 
                                           "Creative Commons Attribution 4.0 International Licence"),
                                    
                                    tags$h2("Overview:"),
                                    tags$p("The App is an interactive web application that allows the user interrogate and visualise trial results alongside site climate and soil constraint information. 
                                    This helps grain growers evaluate the outcomes of various amelioration options in the context of their own soils and climate. 
                                    A series of fact sheets and flow charts relating to soil constraint identification and machinery selection and optimisation support the user to view the most useful results and to identify the most suitable management options for testing."),
                                    
                                    tags$h2("About the app:"),
                                    tags$p("The App draws upon a database of trials results from the Grains Research & Development Corporation (GRDC) CSP00203 project"),
                                    
                                    tags$h2("About the trial:"),
                                    tags$p("The trial program consisted of 24 amelioration trials and 3 seeder-strategy trials across Southern Region between 2016 - 2022.
                                           The trail results are collated in a database, and focus on yield resposne, and contains plant responses, such as plant establishment and biomass.
                                           Additional site metadata includes soil constraints and climate data. 
                                           The database provides statistical  analysis of the yield results, using ANOVA analysis with a P value of 0.1.
                                           The analysis undertaken on a site by year basis and site by cumulative year"),
                                    
                                    tags$h2("Acessing the database:"),
                                    tags$p("The database can be accessed via the Data access portal"),
                                    tags$a(href="https://data.csiro.au/", 
                                           "CSIRO data access portal"),
                                    
                                    
                                    tags$h2("References:"),
                                    tags$p("List..."),
                                    
                                    tags$br(), #this is a 2 break
                                  ), #box Citation and Licence
                                  
                                ), #fluid row     #1
                                
                                
                               
                                
                                
                                
                              ), #tabPanel landing page bracket
                     ),  #fluid page
                     

                     
                     

                   
#########################################################################################################
#########################################################################################################
###############                       what is my constraint page                     #################################
#########################################################################################################                 


tabPanel("What is my constraint", #tab title pageleafletOutput("map"),
         fluidPage( 
           
           ### Diagnosing Sandy Soil Constraints Water repellence ####
           
           fluidRow(
             box(
               title = "Water repellence", width = 6, solidHeader = TRUE, status = "primary",
               tags$p("Water repellence forms when waxes from decayed organic material (e.g. stubbles) coat grains of soil, "),
               tags$p("making them repel water, which inhibits water entry into the soil profile."),
               tags$p("This leads to patchy crop establishment and a staggered germination of weeds, reducing yield potential at the start of the season. "),
               tags$br(), #this is a break
               tags$br(), #this is a 2 break
               tags$img(src = "water drop.jpg",height = 185, style="display: block; margin-left: auto; margin-right: auto;"), #need to resize this image
               tags$br(), #this is a break
                
               #tags$a(href=" https://grdc.com.au/resources-and-publications/all-publications/factsheets/2022/diagnosing-sandy-soil-constraints-water-repellence-and-ph-south-west",      
               tags$a(href="GRDC DSSC Water repellence & pHFact Sheet_v2.pdf", ## this documnet need to sit inside the app directory in a folder called www
                     "click here for more info on water repllency")
             ), #box water repellency
             
             
             ###Diagnosing Sandy Soil Constraints Nutrition ####
             
             
             box(
               title = "Nutrition", width = 6, solidHeader = TRUE, status = "primary",
               tags$p("Sandy soils are often nutrient deficient or infertile because they are highly weathered soils which are low in carbon and have a poor ability to retain and cycle nutrients.") ,
               tags$p("The realisation of the crop yield potential generated by amelioration of sands relies on adequate crop nutrition to feed the new yield potential."),
               tags$p(" There are some key considerations when assessing nutrient status."),
               tags$br(), #this is a break
               tags$img(src = "N_def_crop_large.jpg", height = 185, style="display: block; margin-left: auto; margin-right: auto;"), 
               tags$br(), #this is a break
               tags$br(), #this is a 2 break
               #tags$a(href="https://grdc.com.au/resources-and-publications/all-publications/factsheets/2022/diagnosing-sandy-soil-constraints-nutrition-south-west",
               tags$a(href="GRDC DSSC Nutrition Fact Sheet_v2.pdf", ## this documnet need to sit inside the app directory in a folder called www
                      "click here for more info on nutritional constraints" ),
               
             ), #box Nutrition
             
             ###Diagnosing Sandy Soil Constraints Acid ####
             
             box(
               title = "Acid", width = 6, solidHeader = TRUE, status = "primary",
               tags$p("pH is a measure of the concentration of hydrogen (H+) and hydroxyl (OH-) ions in a soil solution and indicates that a soil is acidic (low pH), neutral or alkaline (high pH)."),
               tags$p("pH variation through the soil profile can occur. "),
               tags$p("It’s important to understand this variation as nutrient availability and crop type tolerance can be affected, resulting in potential plant deficiencies or toxicities."),
               tags$br(), #this is a break
               tags$br(), #this is a 2 break
               
               tags$img(src = "pH_chart.jpg", height = 185, style="display: block; margin-left: auto; margin-right: auto;"), #wrong size
               tags$br(), #this is a break
               #tags$a(href="https://grdc.com.au/resources-and-publications/all-publications/factsheets/2022/diagnosing-sandy-soil-constraints-water-repellence-and-ph-south-west",
               tags$a(href="GRDC DSSC Water repellence & pHFact Sheet_v2.pdf", ## this documnet need to sit inside the app directory in a folder called www
                      "click here for more info on acidity" )
             ), #box acid
             
             ### Diagnosing Sandy Soil Constraints High soil strength  ####
             
             box(
               title = "High soil strength", width = 6, solidHeader = TRUE, status = "primary",
               tags$p("High soil strength can be caused by compaction and/or hard setting and can severely limit plant root penetration through the soil, preventing access to deep reserves of soil moisture and nutrients."),
               tags$p("A hydraulic cone penetrometer is the simplest tool to use to measure soil strength."),
               tags$p("It measures the force required to insert a standard cone into the soil, reported as either kiloPascals (kPa) or megaPascals (1 MPa = 1000 kPa)."),
               tags$br(), #this is a break
               tags$img(src = "soil_strength.jpg", height = 185, style="display: block; margin-left: auto; margin-right: auto;"), 
               tags$br(), #this is a break
               tags$br(), #this is a 2 break
               #tags$a(href="https://grdc.com.au/resources-and-publications/all-publications/factsheets/2022/diagnosing-sandy-soil-constraints-high-soil-strength-south-west",
               tags$a(href="GRDC DSSC High Soil Strength Fact Sheet_v2.pdf", ## this documnet need to sit inside the app directory in a folder called www
                      "click here for more info on high soil strength" )
             ) #box acid
             
           ), #fluid row     #1
           
           
           hr(),
           fluidRow(
                h1(HTML("What are your constraints?</b>"),
                   style="text-align:center"),
                h4(HTML("Use the filter feature in the below table"),
                   style="text-align:center"),
                h4(HTML("To select sites with similar constraints"),
                   style="text-align:center"),
           ) , #fluid row     #2
           
          
         
         
           
           hr(),
           
           fluidRow(DT::dataTableOutput("constraints_table"),
             
             
           ),#fluid row bracket #6
           
           
           
           
           
           
           
         ), #tabPanel landing page bracket
),  #fluid page

#########################################################################################################
                    
#########################################################################################################
###############                       tab 1 Trial results               #################################
#########################################################################################################                 
                    

                     tabPanel("Trial results", #tab title pageleafletOutput("map"),
                              leafletOutput("map"),


                      fluidRow(
                             column(1,), #this is just forcing my plot to appear in the middle

                             column(width = 6,
                               selectInput(
                                 "site_selection",
                                 label = h2("select site:"),
                                 choices = list(
                                   "Brimpton Lake",
                                   "Brooker",
                                   "Buckleboo",
                                   "Bute_CSIRO",
                                   "Bute_Trengrove",
                                   "Cadgee",
                                   "Carwarp_Amelioration",
                                   "Cummins",
                                   "Karkoo",
                                   "Karoonda",
                                   "Kooloonong_canola",
                                   "Kooloonong_chickpea",
                                   "Kooloonong_lentil",
                                   "Kooloonong_lupin",
                                   "Kybunga",
                                   "Lowaldie_Crest",
                                   "Lowaldie_Deep sand",
                                   "Malinong",
                                   "Monia_Gap",
                                   "Mt Damper",
                                   "Murlong",
                                   "Ouyen_Placement",
                                   "Ouyen_Spade",
                                   "Sherwood",
                                   "Telopea_Downs",
                                   "Tempy",
                                   "Waikerie",
                                   "Warnertown",
                                   "Wynarka",
                                   "Yenda",
                                   "Younghusband"
                                   
                              ),
                                  selected = "Brooker"),
                             ),#bracket for select input
                             ),#fluid row bracket #1

                             fluidRow(
                               column(1,), #this is just forcing my plot to appear in the middle
                               column(8,
                                      #align = "center",
                                      offset =.5,
                                      tags$a(href="shorthand trial name_v2.pdf", ## this document need to sit inside the app directory in a folder called www
                                      
                                    h5("click here for more info on trial names" )
                                    )
                               )#clm bracket
                             ),#fluid row bracket #2

                              fluidRow(
                                column(1,), #this is just forcing my plot to appear in the middle
                                column(10,
                                h1(HTML("Cumulative Yields</b>"), 
                                   style="text-align:center"),
                                
                                plotlyOutput('trial_plot')
                                
                                )#clm bracket

                                ),#fluid row bracket #3
                              
                      
                      fluidRow(
                                h1(HTML("ANOVA Results of Cumulative Yields</b>"),
                                   style="text-align:center"),
                                column(1,), #this is just forcing my plot to appear in the middle
                                column(10,
                              DT::dataTableOutput("ANOVA"),
                                )#clm bracket
                              ),#fluid row bracket #4

                      fluidRow(
                        h1(HTML("Yield Gains By Year </b>"), 
                           style="text-align:center"),
                        column(1,), #this is just forcing my plot to appear in the middle
                        column(10,
                               DT::dataTableOutput("trial_table"),
                        )#clm bracket
                      ),#fluid row bracket #5
                      
                      
                      # ###Temp code##
                      # fluidRow(
                      #   column(width=6,   verbatimTextOutput("site_selection"))

                      #),#fluid row bracket #4 Which is just temp

                             ), #tabPanel1 bracket


#########################################################################################################
#########################################################################################################
###############                       Management Options page                     #################################
#########################################################################################################                 


tabPanel("Management Options", #tab title pageleafletOutput("map"),
         fluidPage( 
           fluidRow(
             
             box(
               title = "Matix", width = 10, solidHeader = TRUE, status = "primary",
               
               
               tags$br(), #this is a break
               
               tags$img(src = "Mel_Matrix1.png",
                        height = 300, 
                        style="display: block; margin-left: auto; margin-right: auto;"), 
               
               tags$br(), #this is a break
               
               
               
               
             ), #box for matrix
             
             #### JACKS Water repellence  #### 
             
             box(
               title = "Water repellence", width = 10, solidHeader = TRUE, status = "primary",
               tags$p("Mitigation options for water repellence involve increasing water in the seed row with options through the seeder set up and the use of soil wetters."),
               tags$p("Ameliorating water repellence requires that the repellent layer is diluted through mixing or redistribution using tools like inclusion ripping."),
               
               tags$br(), #this is a break
               
               ## PICTURE FOR SEEDER WITH WETTERES ###
               
               tags$img(src = "seeding with wetter.jpg",height = 185, style="display: block; margin-left: auto; margin-right: auto;"), #need to resize this image
               
               tags$br(), #this is a break
               
               ## URL FOR SEEDER SET UP  ###
               
               tags$a(href="https://grdc.com.au/resources-and-publications/all-publications/factsheets/2022/seeding-sandy-soils-national",
                      "Information on seeder set up "),
               tags$br(), #this is a break
               
               ## URL FOR SOIL WETTERS ###
               
               tags$a(href="https://grdc.com.au/resources-and-publications/all-publications/factsheets/2022/soil-wetter-national",
                      "Information on soil wetters"),
               tags$br(), #this is a break
               
               ## PICTURE FOR SPADER ###
               
               tags$img(src = "Farmax Spader Wynarka.jpg",height = 185, style="display: block; margin-left: auto; margin-right: auto;"), #need to resize this image
               tags$br(), #this is a break
               
               ## URL FOR SPADING  ###
               
               tags$a(href="GRDC_Soil mixing by spading factsheet-Updated 28Aug22.pdf",
               #tags$a(href="https://grdc.com.au/resources-and-publications/all-publications/factsheets/2022/soil-mixing-by-spading-national", 
                      "Information on spading"),
               tags$br(), #this is a break
               
               ## PICTURE FOR RIPPER ###
               
               tags$img(src = "ripping1.jpg",height = 185, style="display: block; margin-left: auto; margin-right: auto;"), #need to resize this image
               tags$br(), #this is a break
               
               ## URL FOR RIPPER ###
               
               tags$a(href="https://grdc.com.au/resources-and-publications/all-publications/factsheets/2022/ripping-technology-national-fact-sheet", 
                      "Information on ripping"),
               
               
               
             ), #box water repellency
             
             
             #### JACKS Acid  #### 
             
             box(
               title = "Acid", width = 10, solidHeader = TRUE, status = "primary",
               tags$p("Management options that mix or redistribute soil layers will have an effect on pH and also provide a means to incorporate amendments like lime to improve pH."), 
               tags$p("Spading and inclusion ripping are the key management options covered here."),
               
               ## PICTURE FOR Spading ###
               
               tags$br(), #this is a break
               tags$img(src = "Farmax Spader Wynarka.jpg", height = 185, style="display: block; margin-left: auto; margin-right: auto;"),       
               tags$br(), #this is a break
               
               ## URL FOR Spading ###
               
               tags$a(href="GRDC_Soil mixing by spading factsheet-Updated 28Aug22.pdf",
                      #tags$a(href="https://grdc.com.au/resources-and-publications/all-publications/factsheets/2022/soil-mixing-by-spading-national", 
                      "Information on spading"),
               tags$br(), #this is a break
               
               ## PICTURE FOR Rripping with Incl ###
               
               tags$img(src = "ripper_inc.jpg", height = 185, style="display: block; margin-left: auto; margin-right: auto;"),       
               tags$br(), #this is a break
               
               ## URL FOR Rripping with Incl ###
               
               tags$a(href="GRDC_FS2022_IncRippTech_v04.pdf",
               #tags$a(href="https://grdc.com.au/resources-and-publications/all-publications/factsheets/2022/inclusion-ripping-technology-national",
                      "Information on inclusion ripping" ),
               
               
             ), #box acid
            
             #### JACKS High soil strength  #### 
              box(
               title = "High soil strength", width = 10, solidHeader = TRUE, status = "primary",
               
               tags$p("The management option that most targets high soil strength is ripping."), 
               tags$p("Spading and inclusion ripping will also reduce soil strength to the depth of tillage."),
               
               ## PICTURE FOR RIPPING ###
               
               tags$br(), #this is a break
               tags$img(src = "ripping1.jpg", height = 185, style="display: block; margin-left: auto; margin-right: auto;"),       
               tags$br(), #this is a break
               
               ## URL FOR RIPPING ###
               
               tags$a(href="https://grdc.com.au/resources-and-publications/all-publications/factsheets/2022/ripping-technology-national-fact-sheet", 
                      "Information on ripping"),
               tags$br(), 
               
               ## PICTURE FOR SPADING ###
               
               tags$img(src = "Farmax Spader Wynarka.jpg", height = 185, style="display: block; margin-left: auto; margin-right: auto;"),       
               tags$br(), #this is a break
               
               ## URL FOR SPADING ###
               
               tags$a(href="https://grdc.com.au/resources-and-publications/all-publications/factsheets/2022/soil-mixing-by-spading-national", 
                      "Information on spading"),
               
               
               
               
               
               
               
             ) #High soil strength
             
           ), #fluid row     #1
           
           
           
                ), #tabPanel what to do now
),  #fluid page


#########################################################################################################
useShinyalert()                  
#########################################################################################################








#########################################################################################################
###############                       tab 2                           #################################
#########################################################################################################                 
#tabPanel("To be removed", #tab title page



############################################################################################################
#################                  options to filter the data       ########################################
############################################################################################################


     #fluidRow(
    #   box(
    #     title = "Site", width = 4, solidHeader = TRUE, status = "primary",
    # 
    #     uiOutput("data2"),

        # useShinyalert(),actionButton("use_page", "How to use this page"),
        # tags$a(href="Undiscounted cash flow.pdf", "More information on model"),
     # ), #box sites


    #   box(
    #     title = "Constraint", width = 8, solidHeader = TRUE, status = "primary",
    # 
    #     infoBoxOutput("non_wetting"),
    #     infoBoxOutput("acidic"),
    #     infoBoxOutput("physical"),
    #     valueBoxOutput("rainfall")
    # 
    #   ) #box Constraint
    # ),#fluid row 1
    # 
    # 
    # 
    # 
    # fluidRow(
    #   column(width=6, uiOutput("data1_scen1")),   ## uiOutput - gets the UI from the server
    #   column(width=6, uiOutput("data1_scen2"))   #remove on of this one?
    # ),#fluid row bracket 2

    ############################################################################################################
    ##############################            the tables         ###############################################
    ############################################################################################################
    ## Cost table
    # fluidRow(
    #   column(width=6,h2("Cost of modification Scenario 1")),
    # 
    #   column(width=6,h2("Cost of modification Scenario 2"))
    # ),#fluid row bracket 2
    # fluidRow(
    #   column(width=6,rHandsontableOutput("cost_sc1")),
    #   column(width=6,rHandsontableOutput("cost_sc2"))
    # ),#fluid row bracket 3
    # 
    # ## Yield table
    # fluidRow(
    #   column(width=6,h2("Yield t/ha Scenario 1")),
    #   column(width=6,h2("Yield t/ha Scenario 2"))
    # ),#fluid row bracket 4
    # fluidRow(
    #   column(width=6,rHandsontableOutput("yld_sc1")),
    #   column(width=6,rHandsontableOutput("yld_sc2"))
    # ),#fluid row bracket 5
    # 
    # ## Extra table
    # fluidRow(
    #   column(width=6,h2("Extra cost or benefits Scenario 1")),
    #   column(width=6,h2("Extra cost or benefits Scenario 2"))
    # ),#fluid row bracket 6
    # fluidRow(
    #   column(width=6,rHandsontableOutput("extra_sc1")),
    #   column(width=6,rHandsontableOutput("extra_sc2"))
    # ),#fluid row bracket 7

    ############################################################################################################
    ##############################            results         ###############################################
    ############################################################################################################

   # select number of years
    # fluidRow(
    #   column(width=6,selectInput("years", label = h3("years for analysis"),
    #                              choices = list("1 Year" = 1, "2 Year" = 2, "3 Year" = 3,
    #                                             "4 Year" = 4, "5 Year" = 5),
    #                              selected = 3))
    # ),#fluid bracket 8
    # 
    # #heading for results
    # fluidRow(
    #   column(width=12,h2("Results Scenario 1 and 2"))
    # ),#fluid row bracket 9
    # 
    # fluidRow(
    #   column(width=6,plotOutput("plot1"))
    # ),#fluid row bracket 10
    # 
    # fluidRow(
    #   column(width=6,DT::dataTableOutput("economic_tb1")) #this is just a check
    # ),#fluid row bracket 11

    ############################################################################################################
    ##############################            save data buttons         ###########################################
    ############################################################################################################
       # fluidRow(downloadButton("download",
       #                   label = "Download data table"))#fluid row bracket 12

    ############################################################################################################
    ##############################            end of UI              ###########################################
    ############################################################################################################
#####################       end of UI for the comparision        ###########################################
############################################################################################################

#) #tabPanel2 bracket

   ) #navbar bracket



   ) # ui bracket
   


