

      
ui = fluidPage(
  leafletOutput("map"),
  tableOutput('tim')
  )

server = function(input, output, session) {
  
  
  output$map <- renderLeaflet({
    leaflet(df)%>% addTiles() %>% 
      addMarkers(data = site_info, popup=site_info$label, layerId = site_info$site )
  })
  
  
   output$tim <- renderTable({
     click <- input$map_marker_click
     if (is.null(click))
       return()
     temp <- site_info %>% filter(longitude == click$lng)
     #temp <- site_info %>% filter(longitude == input$map_marker_click$lng)
      print(temp)
   })
   
  
   
   # observe({
   #   click <- input$map_marker_click
   #   if (is.null(click))
   #     return()
   #   
   #   print(click)
   #   text <-
   #     paste("Lattitude ",
   #           click$lat,
   #           "Longtitude ",
   #           click$lng)
   #   
   #   leafletProxy(mapId = "map") %>%
   #     clearPopups() %>%
   #     addPopups(dat = click, lat = ~lat, lng = ~lng, popup = text)
   #   
   #   # map$clearPopups()
   #   # map$showPopup(click$latitude, click$longtitude, text)
   # })
   # 
   
   
   
}

shinyApp(ui = ui, server = server)
