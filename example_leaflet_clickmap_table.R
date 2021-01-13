

      
ui = fluidPage(
  leafletOutput("map"),
  textOutput("temp"),
  tableOutput('tim')
)

#server.r

#df$location <- gsub( " " , "+" , df$location)

str(site_info)
df <- site_info
#df$location <- gsub( " " , "+" , df$latitude)

server = function(input, output, session) {
  
  
  output$map <- renderLeaflet({
    leaflet(df)%>% addTiles() %>% 
      #addMarkers(lng = longitude, lat = latitude)
      addMarkers(data = df, popup=df$label, layerId = df$site )
  })
  
  output$temp <- renderPrint({

    input$map_marker_click$lng #ooh this works!
  })
  
  # output$tim <- renderPlot({
  #   temp <- df %>% filter(longitude == input$map_marker_click$lng)
  #   # timeVariation(temp, pollutant = "value")
  #   print(ggplot(data = temp, aes(longitude, latitude)) + geom_point())
  # })
  # 
  
   output$tim <- renderTable({
     temp <- df %>% filter(longitude == input$map_marker_click$lng)
  #   # timeVariation(temp, pollutant = "value")
     print(temp)
   })
   
  
}

shinyApp(ui = ui, server = server)
