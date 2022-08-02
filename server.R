require(shiny)
require(rhandsontable)
require(shinydashboard)
require(htmltools)
require(leaflet)
require(slickR)
library(DT)



######################################################################################################################################################## 
###################################code for GIT pushing on esoil.io machine ############################################################################################ 
######################################################################################################################################################## 


machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discover2'){
  rootDir <- '/srv/shiny-server/jax'
  dataStoreDir <- '/srv/shiny-server/jax'
  #source(paste0( rootDir, '/appUtils.R'))

}else{
  rootDir <- 'X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app'
  dataStoreDir <- 'X:/Therese_Jackie/Sandy_soils/App_development2021/sandy_soil_tools_app/'
  #source(paste0( rootDir, '/appUtils.R'))
}


######################################################################################################
#####                       bring in the data the app will use - for the map   #######################
######################################################################################################

## bring in the data and do formatting




site_info <- read.csv(file = "site_location_plus_info.csv")

#remove sites with no coods
site_info <-
  filter(site_info, latitude != "NA")





site_info <- site_info %>%
  mutate(
    non_wetting_label =  case_when(
      non_wetting  == "green"   ~    "No issue",
      non_wetting  == "orange"  ~    "Moderate issue",
      non_wetting  == "red"     ~    "Severe issue",
      TRUE                      ~    "other"
    ))


site_info <- site_info %>%
  mutate(
    acidic_label =  case_when(
      acidic  == "green"  ~    "No issue",
      acidic  == "orange"  ~    "Moderate issue",
      acidic  == "red"     ~    "Severe issue",
      TRUE                      ~    "other"
    ))

site_info <- site_info %>%
  mutate(
    physical_label =  case_when(
      physical  == "green"   ~    "No issue",
      physical  == "orange"  ~    "Moderate issue",
      physical  == "red"     ~    "Severe issue",
      TRUE                      ~    "other"
    ))

site_info <- site_info %>%
  mutate( site_label = paste0("Site = ", site),
          non_wetting_label = paste0("non wetting score = ", non_wetting_label),
          acidic_label = paste0("acidic score = ", acidic_label),
          physical_label = paste0("physical score = ", physical_label))

site_info <- site_info %>%
  mutate( label = paste(sep = "<br/>",site_label, non_wetting_label,acidic_label, physical_label ))
# this label is what is displyed in the popup on the map br mean that there is a new line between each label.
# this is a bit tricky so prior the the label on the popup is the label for each soil property (eg non_wetting_label)


# ######################################################################################################
# #####       bring in the data the app will use - for the trial results plots   #######################
# ######################################################################################################
# 
# 
trial_results <- read_csv("primary_data_all.csv")

### arrange the data so the order reflect level of intervension


order <- c(
  "Control" ,
  "Unmodified_Cl.surface" ,
  "Rip.30_none",
  "Rip.30_Fert.surface",
  "Rip.30IncRip_none" ,
  "Rip.30IncRip_gypsum.mix",

  "Rip.35_none",

  "Rip.40_none"  ,
  "Rip.40IncRip_none"    ,
  "Rip.40IncRip_Lc.mix"  ,

  "Rip.45_none" ,
  "Rip.45IncRip_none"   ,
  "Rip.45IncRip_Fert.deep",
  "Rip.45IncRip+Spade.30_none",
  "Rip.45IncRip_Fert.mix" ,

  "Rip.50_none",
  "Rip.50_Cl.surface" ,
  "Rip.50_Cl.deep",
  "Rip.50IncRip_none" ,
  "Rip.50IncRip_Cl.mix"  ,

  "Rip.60_none" ,
  "Rip.60IncRip_none" ,
  "Rip.60IncRip+Spade.30_none",
  "Rip.60Spade.30_none" ,

  "Spade.30_none"   ,
  "Spade.30_gypsum.mix" ,
  "Spade.30_Cl.mix"  ,
  "Spade.30_Cl.gypsum.mix" ,
  "Spade.30_Fert.mix" ,
  "Spade.30_clay@250"   ,
  "Spade.30_clay@500",

  "DiscInv.30_none"

)

trial_results$Descriptors <- factor(trial_results$Descriptors,
                                    levels = order)
trial_results <- trial_results %>%
  arrange(site)

######## trial data #####

file =
trial_results_table <- read_csv("yield_table_av_all.csv")
trial_results_table <- trial_results_table %>%
  arrange(site)


######################################################################################################
##### bring in the data the app will use  for the economics page     #################################
######################################################################################################


#New working dataset

df <- read.csv(file = "primary_data.csv")

df <- df %>%
  dplyr::select(grouping,
                modification,
                site,
                non_wetting,
                acidic,
                physical,
                rainfall_mean_annual,
                site_numb) %>%
   distinct(site, grouping, .keep_all = TRUE)
df <- df %>%
  filter(modification == "deep ripping")
df$grouping <- as.character(df$grouping)
df$modification <- as.character(df$modification)
df$site <- as.character(df$site)
df$non_wetting <- as.character(df$non_wetting)
df$acidic <- as.character(df$acidic)
df$physical <- as.character(df$physical)

df <- df %>%
  arrange(site)

df_info <- df


cost_table <- read.csv("cost_table.csv")

cost_table <- cost_table %>% rename(`data source` = "data.source")
cost_table$grouping <- as.character(cost_table$grouping)
cost_table$modification <- as.character(cost_table$modification)
cost_table$site <- as.character(cost_table$site)
cost_table$activity <- as.character(cost_table$activity)
cost_table$`data source` <- as.character(cost_table$`data source`)

cost_table <- cost_table %>%
  dplyr::select(- comments)

extra_table <- read.csv("extra_table.csv")

#names(extra_table)
extra_table <- extra_table %>% rename(`data source` = "data.source")

extra_table <- extra_table %>%
  mutate(year = paste0("year ", year)) %>%
  pivot_wider(names_from = year,
              values_from = value,
              values_fill = list(value = 0)) %>%
  relocate(c(comments,`data source`), .after = last_col())

extra_table$grouping <- as.character(extra_table$grouping)
extra_table$modification <- as.character(extra_table$modification)
extra_table$site <- as.character(extra_table$site)
extra_table$non_wetting <- as.character(extra_table$non_wetting)
extra_table$acidic <- as.character(extra_table$acidic)
extra_table$physical <- as.character(extra_table$physical)
extra_table$activity <- as.character(extra_table$activity)
extra_table$comments <- as.character(extra_table$comments)
extra_table$`data source` <- as.character(extra_table$`data source`)

#yld_table

yld_table <- read.csv("yield_table_av.csv")


yld_table <- yld_table %>% rename(
  "yield_unmodified" = 'yield...un.modified.',
  "yield_modified" =    'yield..modified.')

yld_table <- yld_table %>%
  dplyr::select(grouping,
                modification,
                site,
                year = yr_post_amelioration,
                crop,
                "yield_unmodified",
                "yield_modified",
                price,
                "data source" = data.source,
  )

yld_table <- ungroup(yld_table)

  yld_table <- yld_table %>%
  mutate(year = year +1)

  yld_table$grouping <- as.character(yld_table$grouping)
  yld_table$modification <- as.character(yld_table$modification)
  yld_table$site <- as.character(yld_table$site)
  yld_table$crop  <- as.character(yld_table$crop )
  yld_table$`data source` <- as.character(yld_table$`data source`)

  #code no trial data as such
yld_table <- yld_table %>%
    dplyr::mutate(`data source` = case_when(
      is.na(crop) ~ "no trial data",
      TRUE   ~ `data source`
    ))

####################################################################################################
######################         create some extra row for labels              ######################
####################################################################################################

df_info <- df_info %>%
  mutate(
    non_wetting_label =  case_when(
      `non_wetting`  == "green"   ~    "No issue",
      `non_wetting`  == "orange"  ~    "Moderate issue",
      `non_wetting`  == "red"     ~    "Severe issue",
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


df_info <- df_info %>%
  arrange(site)


######################################################################################################
#########################                       server               #################################
######################################################################################################
server <- shinyServer(function(input, output, session) {

######################################################################################################
########    function for filtering the data - and creating a plot on the map page     ################
###################################################################################################### 
  output$trial_plot <- renderPlot({

    ## text that appears on the plot
  ## what is the decile year for our trial results?
   decile_year0_temp <-filter(trial_results,
                              site == input$site_selection & yr_post_amelioration == 0 ) %>%
     select(decile) %>%
     distinct(decile)
   decile_year1_temp <-filter(trial_results,
                              site == input$site_selection & yr_post_amelioration == 1 ) %>%
     select(decile) %>%
     distinct(decile)

   decile_year0 = as.character(unique(decile_year0_temp$decile))
   decile_year1 = as.character(unique(decile_year1_temp$decile))

   ## text that appears on the plot
   ## what is the year of amerolaition for the site / trial ?

   year_amelioration <- trial_results %>%
     filter(site == input$site_selection) %>%
     dplyr::select(Amelioration_Year) %>%
     distinct(Amelioration_Year)

   year_amelioration = as.character(unique(year_amelioration$Amelioration_Year))

   ## the plot
   site_plot_descriptors <-
     trial_results %>% filter(site == input$site_selection) %>%
     dplyr::select(site, Descriptors, yr_post_amelioration, yield) %>%
     ggplot ( aes(x = Descriptors)) +
     geom_bar(
       aes(y = yield, fill = as.factor(yr_post_amelioration)),
       stat = "summary",
       fun.y = "mean",
       position = position_dodge(0.8),
       width = 0.7,
       show.legend = TRUE
     ) +

     guides(fill = guide_legend(title = "Years post amelioration")) +
     scale_fill_grey() +
     theme_bw()+
     theme(plot.title = element_text(size = 20),
           plot.subtitle = element_text(size = 18),

           axis.title.y = element_text(size = 18),
           axis.title.x = element_text(size = 18),

           axis.text.x = element_text(angle = 90, hjust=1,size = 14),
           axis.text.y = element_text(size = 14))+

     labs(title= paste0("Site: ", input$site_selection, ", Year amelioration: ", year_amelioration),
          subtitle = paste0("Deciles: year 0 = " , decile_year0, " , year 1 = ", decile_year1),
          x ="Trial", y = "Yield t/ha")


   site_plot_descriptors

  })


########     function for filtering the data - and creating a table of grouped data on map page #####################

  #table trial data
    output$trial_table <- DT::renderDataTable({


    trial_results_table_1 <-filter(trial_results_table,
                                        site == input$site_selection ) %>%
                                        dplyr::select(-price, -`data source`, -modification) %>%
      dplyr::rename(`aggregated trial` = grouping,
                    `yr post amelioration` = yr_post_amelioration) %>%
      dplyr::filter(!is.na(crop))%>%
      dplyr::mutate(`yield gain` = `yield (modified)` - `yield  (un modified)`)



    DT::datatable(  trial_results_table_1,
                    options = list(dom = 't'),#removes the search bar
                    caption = 'Table 1: Yield response t/ha.') %>%
      formatRound(c(6:8), 2)

  })


  ######################################################################################################
  ##################         function for filtering the data - drop down economics     ##########################
  ######################################################################################################


  ######## sc1   #######################################################################################

  output$data1_scen1 <- renderUI({
    selectInput("data1_scen1", "Modification for scenario 1",
                choices = c(unique(df$grouping)),
                selected = "deep ripping < 40cm no amendment")
  })

  output$data1_scen2 <- renderUI({
    selectInput("data1_scen2", "Modification for scenario 2",
                choices = c(unique(df$grouping)),
                selected = "deep ripping < 40cm no amendment")
  })
  ## input dependant on the choices in `data1`
  output$data2 <- renderUI({
    selectInput("data2", "select",
                choices = c(unique(df$site
                                   [df$grouping == input$data1_scen1])),
                selected = "Cummins")
  })

  ######################################################################################################
  ##################         render table sc1       economics                             #######################
  ######################################################################################################

  output$tb_chosen3 <- renderTable(subset(df,
                                          df$grouping==input$data1_scen1 & df$grouping==input$data2 &
                                            df$site==input$data2
  ),
  rownames=TRUE)


  ######################################################################################################
  ##################        pop up    not sure this needs to be in server        #######################
  ######################################################################################################


  shinyalert(
    title = "This app is under development",
    type = "input",
    text = "What the name of your farm / analysis?",
    #size = "s",
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
      select("non_wetting", "acidic", "physical", "rainfall_mean_annual",
             "non_wetting_label","acidic_label",  "physical_label") %>%
      unique()

  })


  ######## cost table sc1 and sc2 ########
  reactive_filter_cost_sc1 <- reactive({
    filter(cost_table,
           grouping == input$data1_scen1  &
             site == input$data2)   %>%
      select(activity , price,  `data source`)

  })

  reactive_filter_cost_sc2 <- reactive({
    filter(cost_table,
           grouping == input$data1_scen2  &
             site == input$data2)   %>%
      select(activity , price,  `data source`)

  })

  ########   yld table sc1 and sc2   ########
  reactive_filter_yld_sc1 <- reactive({
    filter(yld_table,
           grouping == input$data1_scen1  &
             site == input$data2)   %>%
      select(year, crop, yield_unmodified, yield_modified,price, `data source` )
  })

  reactive_filter_yld_sc2 <- reactive({
    filter(yld_table,
           grouping == input$data1_scen2  &
             site == input$data2)   %>%
      select(year, crop, yield_unmodified, yield_modified, price, `data source`)
  })

  ########   extra table sc1 and sc2   ########
  reactive_filter_extra_sc1 <- reactive({
    filter(extra_table,
           grouping == input$data1_scen1  &
             site == input$data2)   %>%
      #select(activity ,year, value, `data source`)
      select(activity ,`year 1`, `year 2`,`year 3`,`year 4`,`year 5`, `data source`)
  })

  reactive_filter_extra_sc2 <- reactive({
    filter(extra_table,
           grouping == input$data1_scen2  &
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
      mutate(yld_gain_value = (yield_modified - yield_unmodified) * price) %>%
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





  #############################################################################################
  ####################                  outputs        #####################################
  #############################################################################################


  output$non_wetting <- renderInfoBox({
    shinydashboard::valueBox(value = tags$p("non wetting", style = "font-size: 50%;"),
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

     x_max <- max(reactive_economics()$year)
     x_min <- 0
      y_max <- filter(reactive_economics(), year != 0) %>%
        summarise(max = max(undiscounted_cash_flow))

      y_min <- filter(reactive_economics(), year != 0) %>%
        summarise(min = min(undiscounted_cash_flow))



      ggplot(data= reactive_economics(), aes(x= year, y = undiscounted_cash_flow, colour = scenario))+
      geom_line()+
      geom_hline(yintercept=0, linetype="dashed",
                 color = "black", size=0.5)+
        theme_bw()+
        theme(plot.title = element_text(size = 20),

              axis.title.y = element_text(size = 18),
              axis.title.x = element_text(size = 18),

              axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14))+


      scale_x_continuous(limits = c(x_min,
                                     x_max), breaks = seq(0, 5, by = 1))+
      scale_y_continuous(limits = c(y_min[[1]], y_max[[1]]))+

        xlab("Years") + ylab("$/ha") +

        ggtitle("Undiscounted cash flow")
  })








  output$economic_tb1 <- DT::renderDataTable({   #this is just a check
    #dummy data
    reactive_economics()%>%
      dplyr::rename(`total cost` = "total_cost"  ,
                    `total benefit` = "total_benefit" ,
                    `undiscounted cash flow` = "undiscounted_cash_flow" )

    DT::datatable(  reactive_economics(),
                    rownames = FALSE, #removed the row ID
                    colnames = c('Scenario', 'Year', 'Total cost $/ha',
                                 'Total benefit $/ha', 'Undiscounted cash flow $/ha'), #names of clm headings in table
                    options = list(dom = 't'),#removes the search bar
                    caption = 'Table 2: Undiscounted cash flow.') %>%
      formatRound(c(4:5), 2) #note the index is not counted in clms

  })

######################################################################
############### downlaod data button on econmics page ###############

  # Reactive value for GM dataset download----## something wrong with this code its not selecting the correct df
  output$download <-  downloadHandler(
      filename = function() {
        paste0(paste0(input$shinyalert,"Site_",input$data2, "_modification_",input$data1_scen1), ".csv") # change scenario to something else
      },
      content = function(file) {
        vroom::vroom_write(reactive_economics(), file) # this is the output of the economics
      }
    )




  #this is the plot
  output$plot2 <- renderPrint({reactive_plot2()})

  observeEvent(input$use_page, {
    # Show a modal when the button is pressed
    shinyalert("How to use this page",
               HTML("Select site and modification that is most relevant.
      The tables are prefilled,
       but you can edit them with values that are locally relevant."))
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





  ############################################################################################################################################################
  ############################                    the landing page                      ###############################################################################
  ############################################################################################################################################################



  
  
  
}) 

  


