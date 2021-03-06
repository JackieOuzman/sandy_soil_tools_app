require(shiny)
require(rhandsontable)
#require(shinydashboard)
require(htmltools)
require(leaflet)
require(slickR)


######################################################################################################
#####                       bring in the data the app will use - for the map   #######################
######################################################################################################

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


######################################################################################################
#####                       bring in the data the app will use       #################################
######################################################################################################

df <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                 sheet = "DT_selection")
df_info <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                      sheet = "DT_selection")
cost_table <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                         sheet = "DT_cost")
yld_table <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                        sheet = "DT_yld")
extra_table <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                          sheet = "DT_extra_cost_benefits")

extra_table <- extra_table %>% 
  mutate(year = paste0("year ", year)) %>% 
  pivot_wider(names_from = year, values_from = value) %>% 
  relocate(c(comments,`data source`), .after = last_col()) 
####################################################################################################
######################         create some extra row for labels              ######################
####################################################################################################

df_info <- df_info %>% 
  mutate(
    non_wetting_label =  case_when(
      `non-wetting`  == "green"   ~    "No issue",
      `non-wetting`  == "orange"  ~    "Moderate issue",
      `non-wetting`  == "red"     ~    "Severe issue",
      TRUE                      ~    "other"
    ))


df_info <- df_info %>% 
  mutate(
    acidic_label =  case_when(
      acidic  == "green"   ~    "No issue",
      acidic  == "orange"  ~    "Moderate issue",
      acidic  == "red"     ~    "Severe issue",
      TRUE                      ~    "other"
    ))

df_info <- df_info %>% 
  mutate(
    physical_label =  case_when(
      physical  == "green"   ~    "No issue",
      physical  == "orange"  ~    "Moderate issue",
      physical  == "red"     ~    "Severe issue",
      TRUE                      ~    "other"
    ))



######################################################################################################
#########################                       server               #################################
######################################################################################################
server <- shinyServer(function(input, output, session) {
  
  ######################################################################################################
  ##################         function for filtering the data - drop down      ##########################
  ######################################################################################################
  
  
  ######## sc1   #######################################################################################
  
  output$data1_scen1 <- renderUI({
    selectInput("data1_scen1", "modification for scenario 1",
                choices = c(unique(df$modification)),
                selected = "Ploughing")
  })
  
  output$data1_scen2 <- renderUI({
    selectInput("data1_scen2", "modification for scenario 2",
                choices = c(unique(df$modification)),
                selected = "Ploughing")
  })
  ## input dependant on the choices in `data1`
  output$data2 <- renderUI({
    selectInput("data2", "select",
                choices = c(unique(df$site
                                   [df$modification == input$data1_scen1])),
                selected = "Cadgee")
  })
  
  ######################################################################################################
  ##################         render table sc1                                    #######################
  ######################################################################################################
  
  output$tb_chosen3 <- renderTable(subset(df,
                                          df$modification==input$data1_scen1 & df$modification==input$data2 &
                                            df$site==input$data2
  ),
  rownames=TRUE)
  
  
  ######################################################################################################
  ##################        pop up    not sure this needs to be in server        #######################
  ######################################################################################################
  
  
  shinyalert(
    title = "What the name of your farm / analysis?", 
    type = "input",
    #text = "This ia draft app",
    size = "s", 
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  
  
  ######################################################################################################
  ##################                     reactivity                              #######################
  ###################################################################################################### 
  
  
  ## df for the info boxs about the site
  reactive_df_info <- reactive({
    filter(df_info, 
           site == input$data2)    %>% 
      select("non-wetting", "acidic", "physical", "rainfall_mean_annual",
             "non_wetting_label","acidic_label",  "physical_label") %>% 
      unique()
    
  })
  
  
  ######## cost table sc1 and sc2 ########
  reactive_filter_cost_sc1 <- reactive({
    filter(cost_table, 
           modification == input$data1_scen1  &
             site == input$data2)   %>% 
      select(activity , price, comments, `data source`)
  })
  
  reactive_filter_cost_sc2 <- reactive({
    filter(cost_table, 
           modification == input$data1_scen2  &
             site == input$data2)   %>% 
      select(activity , price, comments, `data source`)
  })
  
  ########   yld table sc1 and sc2   ########
  reactive_filter_yld_sc1 <- reactive({
    filter(yld_table, 
           modification == input$data1_scen1  &
             site == input$data2)   %>% 
      select(year, crop, `yield  (un modified)`, `yield (modified)`,price, `data source` )
  })
  
  reactive_filter_yld_sc2 <- reactive({
    filter(yld_table, 
           modification == input$data1_scen2  &
             site == input$data2)   %>% 
      select(year, crop, `yield  (un modified)`, `yield (modified)`, price, `data source`)
  })
  
  ########   extra table sc1 and sc2   ########
  reactive_filter_extra_sc1 <- reactive({
    filter(extra_table, 
           modification == input$data1_scen1  &
             site == input$data2)   %>% 
      #select(activity ,year, value, `data source`)
      select(activity ,`year 1`, `year 2`,`year 3`,`year 4`,`year 5`, `data source`)
  })
  
  reactive_filter_extra_sc2 <- reactive({
    filter(extra_table, 
           modification == input$data1_scen2  &
             site == input$data2)   %>% 
      select(activity ,`year 1`, `year 2`,`year 3`,`year 4`,`year 5`, `data source`)
    #select(activity ,year, value, `data source`)
  })
  
  
  ######   cost_sc1 yld_sc1 extra_sc1
  reactive_economics <- reactive({
    
    economics_tbl_sc1 <- function_economics_tb_sc1(hot_to_r(input$cost_sc1), 
                                                   hot_to_r(input$yld_sc1), 
                                                   hot_to_r(input$extra_sc1), 
                                                   1, as.double(input$years))
    economics_tbl_sc2 <- function_economics_tb_sc1(hot_to_r(input$cost_sc2), 
                                                   hot_to_r(input$yld_sc2), 
                                                   hot_to_r(input$extra_sc2), 
                                                   2, as.double(input$years))
    
    economics_tbl_sc1_sc2 <- bind_rows(economics_tbl_sc1, economics_tbl_sc2)
    economics_tbl_sc1_sc2
  })
  
  reactive_plot2 <- reactive({
    function_graph_cashflow(reactive_economics()) 
  })
  
  
  
  #############################################################################################
  ####################                  functions        #####################################
  #############################################################################################
  
  
  #Function 1 - that bring togther the input data table and creates a df with economics
  function_economics_tb_sc1 <- function(cost_sc_x, yld_sc_x, extra_sc_x, sc_x, run_of_years ){
    
    #change extra table from wide to narrow
    extra_sc_x <- extra_sc_x %>% 
      rename("1" = "year 1",
             "2" = "year 2",
             "3" = "year 3",
             "4" = "year 4",
             "5" = "year 5")
    
    
    extra_sc_x <- extra_sc_x %>% 
      pivot_longer(cols = c(`1`, `2`, `3`, `4`, `5` ),
                   names_to = "year",
                   values_to = "value"
      )
    extra_sc_x$year <- as.double(extra_sc_x$year)
    
    ## replace all na with 0 value
    cost_sc_x[is.na(cost_sc_x)] <- 0
    yld_sc_x[is.na(yld_sc_x)] <- 0
    extra_sc_x[is.na(extra_sc_x)] <- 0
    
    #value of yield
    yld_sc_x <- yld_sc_x %>%
      mutate(yld_gain_value = (`yield (modified)` - `yield  (un modified)`) * price) %>%
      mutate(scenario = paste0("scenario ", sc_x)) %>%
      select(scenario, year, yld_gain_value )
    
    #add a clm that has type is it a cost or saving
    extra_sc_x <- extra_sc_x %>%
      mutate(type =  case_when(
        activity == "additional costs ($/ha)" ~ "cost",
        activity == "cost harvesting and handling extra grain $/t" ~ "cost",
        activity == "additional savings ($/ha)" ~ "benefit",
        TRUE ~ activity
      ))
    
    extra_benefits_cost_sc_x <- extra_sc_x %>%
      group_by(year, type ) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>%
      ungroup()
    
    extra_benefits_sc_x <-  extra_benefits_cost_sc_x %>%
      filter(type == "benefit") %>%
      mutate(scenario = paste0("scenario ", sc_x)) %>%
      select(scenario, year, value )
    
    #Add the beneifits to the yld table
    benefits_sc_x <- left_join(yld_sc_x, extra_benefits_sc_x) %>%
      mutate(total_benefit = yld_gain_value + value) %>%
      select(scenario  ,  year, total_benefit )
    
    
    
    ##### now for the costs
    extra_cost_sc_x <-  extra_benefits_cost_sc_x %>%
      filter(type == "cost") %>%
      mutate(scenario = paste0("scenario ", sc_x)) %>%
      select(scenario, year, value ) %>%
      rename(total_cost =value)
    
    
    
    ### inital costs
    intial_cost_sc_x <- cost_sc_x %>%
      summarise(total_cost = sum(price, na.rm = TRUE)) %>%
      mutate(scenario = paste0("scenario ", sc_x)) %>%
      mutate(year = 0) %>%
      select(scenario, year, total_cost )
    
    total_cost_sc_x <- rbind(intial_cost_sc_x, extra_cost_sc_x)
    economics_tbl_sc_x <- left_join(total_cost_sc_x, benefits_sc_x)
    economics_tbl_sc_x[is.na(economics_tbl_sc_x)] <- 0
    economics_tbl_sc_x <- economics_tbl_sc_x %>%
      mutate(undiscounted_cash_flow = total_benefit - total_cost)
    
    economics_tbl_sc_x <- economics_tbl_sc_x[ 1: (run_of_years+1),]
    
    return(economics_tbl_sc_x)
  }
  
  
  #### function for the graph  
  function_graph_cashflow <- function(economics_tbl_sc1_sc2 ){
    
    x_max <- max(economics_tbl_sc1_sc2$year) 
    x_min <- 1 
    y_max <- filter(economics_tbl_sc1_sc2, year != 0) %>% 
      summarise(max = max(undiscounted_cash_flow))
    y_min <- filter(economics_tbl_sc1_sc2, year != 0) %>% 
      summarise(min = min(undiscounted_cash_flow))
    
    
    Undiscounted_cash_flow <- 
      ggplot(data= economics_tbl_sc1_sc2, aes(x= year, y = undiscounted_cash_flow, colour = scenario))+
      geom_line()+
      geom_hline(yintercept=0, linetype="dashed", 
                 color = "black", size=0.5)+
      theme_bw()+
      scale_x_continuous(limits = c(x_min, x_max), breaks = seq(0, 5, by = 1))+
      scale_y_continuous(limits = c(y_min[[1]], y_max[[1]]))+
      xlab("Years after modification") + ylab("$/ha") +
      ggtitle("Undiscounted cash flow")
    
    
    return(Undiscounted_cash_flow)
  }
  
  
  
  
  
  
  #############################################################################################
  ####################                  outputs        #####################################
  ############################################################################################# 
  
  
  output$non_wetting <- renderInfoBox({
    shinydashboard::valueBox(value = tags$p("non-wetting", style = "font-size: 50%;"),
             subtitle = paste0(reactive_df_info()[1,5]),
             icon = NULL,
             color = paste0(reactive_df_info()[1,1]))
  })
  output$acidic <- renderInfoBox({
    valueBox(value = tags$p("acidic", style = "font-size: 50%;"),
             subtitle = paste0(reactive_df_info()[1,6]),
             icon = NULL,
             color = paste0(reactive_df_info()[1,2]))
  })
  output$physical <- renderInfoBox({
    valueBox(value = tags$p("physical", style = "font-size: 50%;"),
             subtitle = paste0(reactive_df_info()[1,7]),
             icon = NULL,
             color = paste0(reactive_df_info()[1,3]))
  })
  
  output$rainfall <- renderValueBox({
    valueBox(
      value = paste0(round((reactive_df_info()[1,4]),0)), 
      subtitle = "Mean Annual rainfall",
      icon = icon("cloud"), # not working??
      color = "blue"
    )
  })
  
  #http://jrowen.github.io/rhandsontable/
  
  ### cost tables
  output$cost_sc1 <- renderRHandsontable({
    rhandsontable(reactive_filter_cost_sc1(), rowHeaders = NULL )%>%
      hot_col("price", format = "0")
    #converts the R dataframe to rhandsontable object and sets format
  })
  output$cost_sc2 <- renderRHandsontable({
    rhandsontable(reactive_filter_cost_sc2(), rowHeaders = NULL) %>%
      hot_col("price", format = "0")#converts the R dataframe to rhandsontable object
  })
  
  
  ### yld tables
  output$yld_sc1 <- renderRHandsontable({
    rhandsontable(reactive_filter_yld_sc1(), rowHeaders = NULL) %>%
      hot_col("year", format = "0")%>% 
      hot_col("price", format = "0")
  })
  output$yld_sc2 <- renderRHandsontable({
    rhandsontable(reactive_filter_yld_sc2(), rowHeaders = NULL) %>%
      hot_col("year", format = "0") %>% 
      hot_col("price", format = "0")
  })
  
  
  
  ### extra tables
  output$extra_sc1 <- renderRHandsontable({
    rhandsontable(reactive_filter_extra_sc1(), rowHeaders = NULL) %>%
      hot_col("year 1", format = "1") %>% 
      hot_col("year 2", format = "1") %>%
      hot_col("year 3", format = "1") %>%
      hot_col("year 4", format = "1") %>%
      hot_col("year 5", format = "1") 
  })
  output$extra_sc2 <- renderRHandsontable({
    rhandsontable(reactive_filter_extra_sc2(), rowHeaders = NULL) %>%
      hot_col("year 1", format = "1") %>% 
      hot_col("year 2", format = "1") %>%
      hot_col("year 3", format = "1") %>%
      hot_col("year 4", format = "1") %>%
      hot_col("year 5", format = "1") 
  }) 
  
  output$plot1 <- renderPlot({
    
    ggplot(data= reactive_economics(), aes(x= year, y = undiscounted_cash_flow, colour = scenario))+
      geom_line()+
      geom_hline(yintercept=0, linetype="dashed",
                 color = "black", size=0.5)+
      theme_bw()+
      #scale_x_continuous(limits = c(x_min, x_max), breaks = seq(0, 5, by = 1))+
      #scale_y_continuous(limits = c(y_min[[1]], y_max[[1]]))+
      xlab("Years after modification") + ylab("$/ha") +
      ggtitle("Undiscounted cash flow")
  })
  
  
  
  output$economic_tb1 <- renderPrint({   #this is just a check
    reactive_economics() #this has my filtering reactive object
  })
  
  
  
  observeEvent(input$saveBtn, write.csv(hot_to_r(input$cost),
                                        file = "test.csv",
                                        row.names = FALSE))
  
  
  #this is the plot 
  output$plot2 <- renderPrint({reactive_plot2()})
  
  observeEvent(input$cost, {
    # Show a modal when the button is pressed
    shinyalert("Inital costs include", 
               HTML("This is the first line. 
      This should be the second.
       Third line
       etc..."))
  })
  
  observeEvent(input$yield, {
    # Show a modal when the button is pressed
    shinyalert("Inital costs include", 
               HTML("This is the first line. 
      This should be the second.
       Third line
       etc..."))
  })
  
  observeEvent(input$extra, {
    # Show a modal when the button is pressed
    shinyalert("Inital costs include", 
               HTML("This is the first line. 
      This should be the second.
       Third line
       etc..."))
  })
  
  output$test <- renderPrint(reactive_df_info()) 

  
  ############################################################################################################################################################
  ############################                    the map                      ###############################################################################
  ############################################################################################################################################################
  
  # #display the map with popup and labels on multiple lines 
  # 
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

  ############################################################################################################################################################
  ############################                    the landing page                      ###############################################################################
  ############################################################################################################################################################

  
   output$slickr <- renderSlickR({
     imgs <- list.files("X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/app_modification/www", pattern=".jpg", full.names = TRUE)  
     
     
     slickR(imgs)
   })
   
  
  
}) 

  


