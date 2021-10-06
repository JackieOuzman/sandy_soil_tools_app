
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
library(slickR)

library(shinyWidgets)





# Define UI 

#ui <-dashboardPage(
#  dashboardHeader(title = ""),
  
#########################################################################################################
###############                       navbartitle                          #################################
#########################################################################################################
 
  
   ui <- (navbarPage(title = '', collapsible = TRUE,
                    
                     
                      header = tagList(    
                        useShinydashboard()
                     ) ,   ## these two lines get the value box to display without using shinydashboards        
#########################################################################################################
###############                       landing page                     #################################
#########################################################################################################                 
                     
                     
                     tabPanel("landing page", #tab title pageleafletOutput("map"),
                              h6("Welcome") ,      
                              
                               slickROutput("slickr",
                                            height = 100,
                                            width="95%" 
                                            )
                               
                              #I think I need to resize my images first and then try and load thme in
                              
                              #tags$img(src = "LRQHE5309.jpg", height = 50, width =50) # this is old code with one image loaded
                              
                              
                              
                     ), #tabPanel landing page bracket
                     
                     
                     #########################################################################################################
                    
#########################################################################################################
###############                       tab 1                           #################################
#########################################################################################################                 
                    
                    
                     tabPanel("map of sites", #tab title pageleafletOutput("map"),
                              leafletOutput("map"),
                             
                             # table 
                             tableOutput('tim'),
                             
                             fluidRow(column(
                               width = 6,
                               selectInput(
                                 "site_selection",
                                 label = h3("select site:"),
                                 choices = list(
                                   "Kooloonong_chickpea",
                                   "Kooloonong_lentil" ,
                                   "Kooloonong_lupin"  ,
                                   "Tempy"     ,
                                   "Wynarka"    ,
                                   "Monia_Gap"  ,
                                   "Malinong" ,
                                   "Sherwood"  ,
                                   "Cummins"  ,
                                   "Karkoo" ,
                                   "Buckleboo"   ,
                                   "Mt_Damper" ,
                                   "Kybunga"  ,
                                   "Warnertown"  ,
                                   "Telopea_Downs"
                                 ),
                                 selected = "Tempy"
                               ),
                               plotOutput("trial_plot"),
                               tableOutput("trial_table")
                             )),
                             
                             
                             
                    ), #tabPanel1 bracket
 

#########################################################################################################
###############                       tab 2                           #################################
#########################################################################################################                 
tabPanel("comparision with trial", #tab title page
             
         
  
############################################################################################################
#################                  options to filter the data       ########################################
############################################################################################################
  # dashboardSidebar(disable = TRUE),
  # dashboardBody(
    
    fluidRow(
      box(
        title = "Site", width = 4, solidHeader = TRUE, status = "primary",
        
        uiOutput("data2")
      ), #box sites
      
      
      box(
        title = "Constraint", width = 8, solidHeader = TRUE, status = "primary",
        
        infoBoxOutput("non_wetting"),
        infoBoxOutput("acidic"),
        infoBoxOutput("physical"),
        valueBoxOutput("rainfall")
        
      ) #box Constraint
    ),#fluid row 1
    
    
    
    
    fluidRow(
      column(width=6, uiOutput("data1_scen1")),   ## uiOutput - gets the UI from the server
      column(width=6, uiOutput("data1_scen2"))   #remove on of this one?
    ),#fluid row bracket 2
    
    ############################################################################################################
    ##############################            the tables         ###############################################
    ############################################################################################################
    ## Cost table
    fluidRow(
      column(width=4,h2("Cost of modification Scenario 1")),
      column(width=2,useShinyalert(),  # Set up shinyalert
             actionButton("cost", "More information on costs")), 
      column(width=4,h2("Cost of modification Scenario 2"))
    ),#fluid row bracket 2
    fluidRow(
      column(width=6,rHandsontableOutput("cost_sc1")),
      column(width=6,rHandsontableOutput("cost_sc2")) #can't output the exact same tabel
    ),#fluid row bracket 3
    
    ## Yield table
    fluidRow(
      column(width=4,h2("Yield t/ha Scenario 1")),
      column(width=2,useShinyalert(),  # Set up shinyalert
             actionButton("yield", "More information on yield")), 
      column(width=4,h2("Yield t/ha Scenario 2"))
    ),#fluid row bracket 4
    fluidRow(
      column(width=6,rHandsontableOutput("yld_sc1")),
      column(width=6,rHandsontableOutput("yld_sc2")) 
    ),#fluid row bracket 5
    
    ## Extra table
    fluidRow(
      column(width=4,h2("Extra cost or benefits Scenario 1")),
      column(width=2,useShinyalert(),  # Set up shinyalert
             actionButton("extra", "More information on yield")), 
      column(width=4,h2("Extra cost or benefits Scenario 2"))
    ),#fluid row bracket 6
    fluidRow(
      column(width=6,rHandsontableOutput("extra_sc1")),
      column(width=6,rHandsontableOutput("extra_sc2")) 
    ),#fluid row bracket 7
    
    ############################################################################################################
    ##############################            results         ###############################################
    ############################################################################################################
    
    #select number of years
    fluidRow(
      column(width=6,selectInput("years", label = h3("years for analysis"), 
                                 choices = list("1 Year" = 1, "2 Year" = 2, "3 Year" = 3,
                                                "4 Year" = 4, "5 Year" = 5), 
                                 selected = 3)) 
    ),#fluid bracket 8
    
    #heading for results
    fluidRow(
      column(width=12,h2("Results Scenario 1 and 2"))
    ),#fluid row bracket 9
    
    fluidRow(
      column(width=6,plotOutput("plot1")) 
    ),#fluid row bracket 10
    
    fluidRow(
      column(width=6,verbatimTextOutput("economic_tb1")) #this is just a check
    ),#fluid row bracket 11
    
    ############################################################################################################
    ##############################            action buttons         ###########################################
    ############################################################################################################
    
    
    fluidRow(
      column(width=6,actionButton("saveBtn", "Save")) 
    )#fluid row bracket 12
    
    ############################################################################################################
    ##############################            end of UI              ###########################################
    ############################################################################################################
#####################       end of UI for the comparision        ###########################################
############################################################################################################

) #tabPanel2 bracket

   ) #navbar bracket
   ) # ui bracket
   


