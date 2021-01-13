#example of navbarPage

library(shiny)
library(shiny)
library(dplyr)
library(htmltools)

#######################################################################################################################################################

## bring in the data and do formatting
site_info <- read.csv(file = paste0("X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/", "site_location_plus_info.csv"))
#remove sites with no coods
site_info <- 
  filter(site_info, latitude != "NA")
site_info <- site_info %>% 
  rename(site = "ï..Site" )

names(site_info)


site_info <- site_info %>% 
  mutate( site_label = paste0("Site = ", site),
          non.wetting_label = paste0("non wetting score = ", non.wetting),
          acidic_label = paste0("acidic score = ", acidic),
          physical_label = paste0("physical score = ", physical))

site_info <- site_info %>% 
  mutate( label = paste(sep = "<br/>",site_label, non.wetting_label,acidic_label, physical_label ))


#######################################################################################################################################################

ui <- (navbarPage(#title = "App name: sandy soils",
                   title = '', collapsible=TRUE,
                   tabPanel("map of sites", #tab title page
                            
                            leafletOutput("map"),
                              
                              
                              # table 
                              tableOutput('tim')
                              
                            ), #tabPanel1 bracket
                    
                   
                   
                   tabPanel("comparison with trial data", #tab title page
                             h4("add the Ui content here"), #inside the tab
                    ) #tabPanel2 bracket
                     
                   )# navbarPage bracket
                   )# Ui bracket
                  
server <- function(input, output, session) {
 
    #display the map with popup and labels on multiple lines 
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite, #This is spot to change the base map
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addMarkers(data = site_info, popup=site_info$label, layerId = site_info$site )
    })
    
    
    
    
    
    
    
    output$tim <- renderTable({
      click <- input$map_marker_click
      if (is.null(click))
        return()
      temp <- site_info %>% filter(longitude == click$lng) %>% 
        
        select( "site",
                "latitude" ,
                "longitude",
                "Region"  ,
                "trial.type",
                "non.wetting",
                "acidic" ,
                "physical" ,
                "met_station_number" ,
                "average_annual_rainfall")
      
      print(temp)
    })
    
  } 
  
  

shinyApp(ui, server)
