
#bring in the library that I will be working with
library(shiny)
# library(ggplot2)
# library(readxl)
# library(tidyverse)
# library(rhandsontable)
# library(shinyalert)
# library(shinydashboard)
# library(htmltools)
# library(leaflet)
# library(slickR) #new install on land and water linex

library(shinyWidgets)

library(vroom) #new install on land and water linex

library(vroom)






# Define UI 


  
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
                              fluidPage( 
                              fluidRow(
                                box(
                                  title = "Water repellence", width = 2, solidHeader = TRUE, status = "primary",
                                  
                                  tags$a(href="Water repellence.pdf", ## this documnet need to sit inside the app directory in a folder called www
                                         "click here for more info on water repllency" )
                                ), #box water repellency
                                
                                
                                box(
                                  title = "Waterlogging", width = 2, solidHeader = TRUE, status = "primary",
                                  tags$a(href="Waterlogging.pdf", ## this documnet need to sit inside the app directory in a folder called www
                                         "click here for more info on waterlogging" )
                                ), #box Waterlogging
                                box(
                                  title = "Acid", width = 2, solidHeader = TRUE, status = "primary",
                                  tags$a(href="acidity.pdf", ## this documnet need to sit inside the app directory in a folder called www
                                         "click here for more info on acidity" )
                                ), #box acid
                                box(
                                  title = "Hard setting", width = 2, solidHeader = TRUE, status = "primary",
                                  tags$a(href="hard setting.pdf", ## this documnet need to sit inside the app directory in a folder called www
                                         "click here for more info on hard setting" )
                                ) #box acid
                              
                                ), #fluid row     
                              
                              hr(),
                                
                             
                                
                               tags$img(src = "flow_chart_soil_constraints.jpg") 
                                        #height = 50, width =50
                                        #) # this is old code with one image loaded
                              
                               
                              
                              
                              
                     ), #tabPanel landing page bracket
                     ) , #fluid page
                     
                     #########################################################################################################
                    
#########################################################################################################
###############                       tab 1                           #################################
#########################################################################################################                 
                    
                     # 
                     # tabPanel("map of sites", #tab title pageleafletOutput("map"),
                     #          leafletOutput("map"),
                     # 
                     #          
                     #  fluidRow(
                     #                 
                     #         column(width = 6,
                     #           selectInput(
                     #             "site_selection",
                     #             label = h3("select site:"),
                     #             choices = list(
                     #               "Buckleboo"   ,
                     #               "Cummins"  ,
                     #               "Karkoo" ,
                     #               "Kooloonong_chickpea",
                     #               "Kooloonong_lentil" ,
                     #               "Kooloonong_lupin"  ,
                     #               "Kybunga"  ,
                     #               "Malinong" ,
                     #               "Monia_Gap"  ,
                     #               "Mt_Damper" ,
                     #               "Sherwood"  ,
                     #               "Telopea_Downs",
                     #               "Tempy"     ,
                     #               "Warnertown"  ,
                     #               "Wynarka"   ),
                     #              selected = "Tempy"),
                     #         ),#bracket for select input
                     #         ),#fluid row bracket #1
                     #         
                     #         fluidRow(
                     #           column(8, 
                     #                  #align = "center", 
                     #                  offset =.5,
                     #                  tags$a(href="shorthand trial name.pdf", ## this documnet need to sit inside the app directory in a folder called www
                     #                "click here for more info on trial names" )
                     #           )#clm bracket
                     #         ),#fluid row bracket #2
                     #            
                     #          fluidRow(
                     #           column(width=6,   DT::dataTableOutput("trial_table")),
                     #           column(width = 6,  plotOutput("trial_plot")) 
                     #           
                     #            ),#fluid row bracket #3 
                     #            
                     #            
                     #         
                     #         ), #tabPanel1 bracket
                     # 

#########################################################################################################
###############                       tab 2                           #################################
#########################################################################################################                 
tabPanel("comparision with trial", #tab title page
             
         
  
############################################################################################################
#################                  options to filter the data       ########################################
############################################################################################################
  
    
    # fluidRow(
    #   box(
    #     title = "Site", width = 4, solidHeader = TRUE, status = "primary",
    #     
    #     uiOutput("data2"),
    #     
    #     useShinyalert(),actionButton("use_page", "How to use this page"),
    #     tags$a(href="Undiscounted cash flow.pdf", "More information on model"),
    #   ), #box sites
    #   
    #   
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
    # 
    # ############################################################################################################
    # ##############################            the tables         ###############################################
    # ############################################################################################################
    # ## Cost table
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
    # 
    # ############################################################################################################
    # ##############################            results         ###############################################
    # ############################################################################################################
    # 
    # #select number of years
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
    # 
    # ############################################################################################################
    # ##############################            save data buttons         ###########################################
    # ############################################################################################################
    #   fluidRow(downloadButton("download",
    #                     label = "Download data table"))#fluid row bracket 12
    # 
    ############################################################################################################
    ##############################            end of UI              ###########################################
    ############################################################################################################
#####################       end of UI for the comparision        ###########################################
############################################################################################################

) #tabPanel2 bracket

   ) #navbar bracket
   ) # ui bracket
   


