library(shiny)
library(dplyr)
library(htmltools)



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


#https://rstudio.github.io/leaflet/shiny.html

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  
  # display the map
  leafletOutput("mymap"),
  
 # display a widget to click test out observed events
 selectInput("x", label = h3("Select box"), 
                          choices = list("Karoonda" = "Karoonda", "Waikerie" = "Waikerie", "	Bute" = 	"Bute"), 
                          #choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                          selected = "Karoonda"),
   br(),
   actionButton("button", "Show"),





# table 
 tableOutput("table"),

# table 
tableOutput("table2"))



server <- function(input, output, session) {

#display the map with popup and labels on multiple lines 
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite, #This is spot to change the base map
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = site_info, popup=site_info$label, layerId = site_info$site )
  })
  
  
  
  # observeEvent(input$button, {
  #   
  # })
  
  
  df <- eventReactive(input$button, {
      filter(site_info, site == input$x) %>% 
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
  })
  
  ### mess about with this one
  df2 <- eventReactive(input$button, {
      #print(input$x)
       print(input$mymap_shape_click)
    
  })
 
  
   
  output$table <- renderTable({
    df()
  }) 
  
  
  ### mess about with this one
  output$table2 <- renderTable({
    df2()
  }) 
  
}

shinyApp(ui, server)
