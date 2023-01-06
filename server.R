require(shiny)
require(rhandsontable)
require(shinydashboard)
require(htmltools)
require(leaflet)
require(slickR)
library(DT)
library(data.table)

library(stringi) 
library(plotly)
#install.packages("shinyalert")


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

site_info <- read.csv(file = "site_location_plus_info_v2.csv")

#remove sites with no coods
site_info <-
  filter(site_info, latitude != "NA")



names(site_info)

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
  mutate(
    nutrientl_label =  case_when(
      nutrient  == "green"   ~    "No issue",
      nutrient  == "orange"  ~    "Moderate issue",
      nutrient == "red"     ~    "Severe issue",
      TRUE                      ~    "other"
    ))

site_info <- site_info %>%
  mutate( site_label = paste0("Site = ", site),
          non_wetting_label = paste0("Water repellence = ", non_wetting_label),
          acidic_label = paste0("Acid = ", acidic_label),
          physical_label = paste0("Soil strength = ", physical_label),
          nutrientl_label = paste0("Nutrition = ", nutrientl_label)
  )

site_info <- site_info %>%
  mutate( label = paste(sep = "<br/>",site_label, non_wetting_label,acidic_label, physical_label,nutrientl_label  ))
# this label is what is displyed in the popup on the map br mean that there is a new line between each label.
# this is a bit tricky so prior the the label on the popup is the label for each soil property (eg non_wetting_label)

# ######################################################################################################
# #####       bring in the data the app will use - for the trial results plots and table  #######################
# ######################################################################################################
# 
#This trial_results is used in the table 
trial_results <- read_csv("primary_data_all_v2.csv")
#names(trial_results)

### arrange the data / Descriptor so the order reflect level of intervension
#list_of_Descriptors_with_order_rank <- read_csv("list_of_Descriptors_with_order_rank.csv")
list_of_Descriptors_with_order_rank <- read_csv("list_of_Descriptors_with_new_Desciptors_name.csv")
#str(list_of_Descriptors_with_order_rank)
list_of_Descriptors_with_order_rank <- list_of_Descriptors_with_order_rank %>% 
  dplyr::rename(Descriptors_new = 'New names of Descriptors')

order_df <- list_of_Descriptors_with_order_rank %>% 
  distinct(order_rank, .keep_all= TRUE) %>% 
  arrange(order_rank)

#order <- as.list(order_df$Descriptors) 
order <- as.list(order_df$Descriptors_new)
names(trial_results)
trial_results <- trial_results %>% 
  dplyr::rename(Descriptors_new = New.names.of.Descriptors)


trial_results$Descriptors_new <- factor(trial_results$Descriptors_new,
                                    levels = order)
trial_results <- trial_results %>%
  arrange(site)

######## trial data ##### #This trial_results is used in the plot 


trial_results_table <- read_csv("primary_data_all_v2.csv")
trial_results_table <- trial_results_table %>%
  arrange(site)

names(trial_results_table)

trial_results_table <- trial_results_table %>% 
  dplyr::rename(Descriptors_new = New.names.of.Descriptors)


######################################################################################################
##### bring in the data the app will use  for the economics page     #################################
######################################################################################################


#New working dataset 03/08/2022

df <- read.csv(file = "primary_data_all_v2.csv")
#names(df)
df <- df %>%
  dplyr::select(grouping,
                modification,
                site,
                non_wetting,
                acidic,
                physical,
                nutrient,
                rainfall_mean_annual,
                site_numb) %>%
  distinct(site, grouping, .keep_all = TRUE)
distinct(df,modification )


df <- df %>%
  filter(modification == "Rip")
df$grouping <- as.character(df$grouping)
df$modification <- as.character(df$modification)
df$site <- as.character(df$site)
df$non_wetting <- as.character(df$non_wetting)
df$acidic <- as.character(df$acidic)
df$physical <- as.character(df$physical)
df$nutrient <- as.character(df$nutrient)

df <- df %>%
  arrange(site)

df_info <- df


cost_table <- read.csv("cost_table_v2.csv")

cost_table <- cost_table %>% rename(`data source` = "data.source")
cost_table$grouping <- as.character(cost_table$grouping)
cost_table$modification <- as.character(cost_table$modification)
cost_table$site <- as.character(cost_table$site)
cost_table$activity <- as.character(cost_table$activity)
cost_table$`data source` <- as.character(cost_table$`data source`)

cost_table <- cost_table %>%
  dplyr::select(- comments)

extra_table <- read.csv("extra_table_v2.csv")

#names(extra_table)
extra_table <- extra_table %>% rename(`data source` = "data.source")

extra_table <- extra_table %>%
  mutate(year = paste0("year ", year)) %>%
  pivot_wider(names_from = year,
              values_from = value,
              values_fill = list(value = 0)) %>%
  relocate(c(comments,`data source`), .after = last_col())
#names(extra_table)
extra_table$grouping <- as.character(extra_table$grouping)
extra_table$modification <- as.character(extra_table$modification)
extra_table$site <- as.character(extra_table$site)
extra_table$non_wetting <- as.character(extra_table$non_wetting)
extra_table$acidic <- as.character(extra_table$acidic)
extra_table$physical <- as.character(extra_table$physical)
extra_table$nutrient <- as.character(extra_table$nutrient)
extra_table$activity <- as.character(extra_table$activity)
extra_table$comments <- as.character(extra_table$comments)
extra_table$`data source` <- as.character(extra_table$`data source`)

#yld_table

yld_table <- read.csv("yield_table_av_v2.csv")
#names(yld_table)
yld_table <- yld_table %>% 
  dplyr::rename(Descriptors_new = New.names.of.Descriptors)


yld_table <- yld_table %>% rename(
  "yield_unmodified" = 'yield...un.modified.',
  "yield_modified" =    'yield..modified.')

yld_table <- yld_table %>%
  dplyr::select(
    site,
    Descriptors_new,
    grouping,
    modification,
    crop,
    year,
    yr_post_amelioration,
    "yield_unmodified",
    "yield_modified",
    price,
    "data source" = data.source,
    decile,
    rainfall_mean_annual
  )




yld_table <- ungroup(yld_table)

yld_table <- yld_table %>%
  mutate("yr post amelioration" = yr_post_amelioration +1)

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
####################                   anova data      ##############################################
######################################################################################################

ANOVA_Cum_Yld <- read.csv(file = "ANOVA_Cum_Yld_df_v2.csv")

#names(ANOVA_Cum_Yld)
ANOVA_Cum_Yld <- ANOVA_Cum_Yld %>% 
  dplyr::rename(Descriptors_new = New.names.of.Descriptors)

######################################################################################################
#########################                       server               #################################
######################################################################################################
server <- shinyServer(function(input, output, session) {

  
  
######################################################################################################
########    function for filtering the data - and creating a table on the map page     ################
###################################################################################################### 
 
  output$ANOVA <- DT::renderDataTable({
    ### name of site selected
    a <- c(reactive_site_selection())
   
    ANOVA_Cum_Yld_site <- ANOVA_Cum_Yld %>%  filter(site == a) 
    order_df <- order_df %>% 
      dplyr::select(Descriptors_new, order_rank, "detailed_name")
    
    ANOVA_Cum_Yld_site <- left_join(ANOVA_Cum_Yld_site,order_df, by = c("Descriptors_new" =  "Descriptors_new") ) 
  
    ANOVA_Cum_Yld_site <- ANOVA_Cum_Yld_site %>%
      arrange(order_rank.x) %>% #arrange the data table using the ranking of treatments
      dplyr::select("Descriptors_new", "detailed_name.x", 
                    "Yield", 
                    #"Standard.error", 
                    #"count",
                    "Significance", "groups", )
      
      
    colnames(ANOVA_Cum_Yld_site) <- c("Treatment", "Detailed Treatment Name", "Yield", "Significance of ANOVA", "Groups")
    DT::datatable(ANOVA_Cum_Yld_site ,
                  rownames = FALSE,  
                  options = list(columnDefs =
                                   list(list(className = 'dt-center',
                                             targets = "_all")))) %>%
      #formatRound(c(4), 0) %>%
      #formatRound(c(3), 2) %>%#this round clm number  to 2 decimal places
      formatRound(c(3), 2)  
    
})
  
######################################################################################################
########    function for filtering the data - and creating a plot on the map page     ################
###################################################################################################### 
  
  output$trial_plot <- renderPlotly({
    
    # set the site name call on the reactive function defined in te reactive secetion
    site_in_app <- c(reactive_site_selection())
    
    ### name of site selected
    a <-  site_in_app #name of the site
    
    
    
   
    
    ##############################################################################################################################################
    ################                 TRIAL DATA                    ################
    ###############################################################################################################################################
    
    #filter the TRIAL data on site 
    
    
    site_year_yld_summary_site <- trial_results_table %>%  filter(site == a) 
    
    
    ### brooker is a problem site I want to filter out these ones:
    
    site_year_yld_summary_site <- site_year_yld_summary_site %>%
      filter(Descriptors  != "Spade.30_Lc@1.incorp_30") %>%
      filter(Descriptors  != "Spade.30_Lc@1.incorp_30.K_added.surface") %>%
      filter(Descriptors  != "Spade.30_Lc@2.incorp_30") %>%
      filter(Descriptors  != "Spade.30_Lc@2.incorp_30.K_added.surface") %>%
      filter(Descriptors  != "Spade.30_Lc@6.incorp_30") %>%
      filter(Descriptors  != "Spade.30_Lc@6.incorp_30.K_added.surface") %>%
      filter(Descriptors  != "Spade.30_Lc@10.incorp_30") %>%
      filter(Descriptors  != "Spade.30_Lc@10.incorp_30.K_added.surface") %>%
      filter(Descriptors  != "Spade.30_Lc@20.incorp_30") %>% 
      filter(Descriptors  != "Spade.30_Lc@20.incorp_30.K_added.surface")
    
    ### Younghusband is a problem site I want to filter out these ones there is not the same level of reps for treatments:
    
    site_year_yld_summary_site <- site_year_yld_summary_site %>%
      
      filter(Descriptors  != "Unmodified+DeepTill.18_SE14.band_8") %>%
      filter(Descriptors  != "Unmodified+DeepTill.18_none") %>%
      filter(Descriptors  != "Unmodified+DeepTill.18_none") %>%
      filter(Descriptors  != "Unmodified+OnRow_none") 
    
    
    site_year_yld_summary_site <- site_year_yld_summary_site[!( site_year_yld_summary_site$site == "Younghusband" & 
                                                                  ( site_year_yld_summary_site$Descriptors == "Control" )),] 
    
    site_year_yld_summary <- site_year_yld_summary_site 
    
    ## this is creating mean value for each year and treatment
    site_year_yld_summary <- site_year_yld_summary_site %>% 
      dplyr::group_by(Descriptors_new, year) %>%
      dplyr::summarise(mean=mean(yield, na.rm = TRUE), 
                       sd=sd(yield, na.rm = TRUE),
                       count = n(),
                       std_error = sd/(sqrt(count))
      ) %>%
      arrange(desc(mean))
    
    site_year_yld_summary$Descriptors_new <- factor(site_year_yld_summary$Descriptors_new,
                                                levels = order)
    
    # the order of years 
    order_yrs <- c(
      "2021",
      "2020",
      "2019",
      "2018",
      "2017",
      "2016",
      "2015",
      "2014"
    )
    
    site_year_yld_summary$year <- factor(site_year_yld_summary$year,
                                         levels = order_yrs)
    
    
    ### Plot
    
    #ensure year is defined a a factor for a nice plot
    site_year_yld_summary$year <- as.factor(site_year_yld_summary$year)
    sum_cum_yld <- site_year_yld_summary %>% 
      group_by(Descriptors_new) %>% 
      dplyr::summarise(sum_yld =sum(mean, na.rm = TRUE) )
    
    #sum_cum_yld
    max_sum_cum <- max(sum_cum_yld$sum_yld, na.rm = TRUE)
    max_sum_cum <- max_sum_cum +0.5
    max_sum_cum <- ceiling(max_sum_cum)
    
    site_year_yld_summary <- site_year_yld_summary %>% 
      rename(Treatment = Descriptors_new,
             Yield = mean)
    site_year_yld_summary$Yield <- round(site_year_yld_summary$Yield, digits = 2)
    
    CumPlot <- site_year_yld_summary %>% 
      ggplot( aes(x = Treatment, y = Yield, 
                  fill = year, 
                  colour = year,
                  text = paste0(Treatment, '\n', " Yield ",Yield)
                  )) + 
      geom_bar(stat = "identity",  alpha = 0.5)  +
      labs(x="", 
           y="t/ha", 
           title = paste0(a))+
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_y_continuous(breaks=seq(0,max_sum_cum,by=2.0), limits = c(0, max_sum_cum))+
      scale_x_discrete(labels = function(Treatment) str_wrap(stri_replace_all_fixed(Treatment, c(".","_"), c(".\U200B", "_\U200B"),vectorize_all=FALSE), width = 5)) +
      
      theme(
        #axis.text.x=element_text(angle=50,hjust=1, size = 12),
        axis.text.y=element_text(size = 10),
        axis.title.y.left = element_text(size = 16),
        plot.title = element_text(size = 20)) 
    
    
    #ggplotly(CumPlot)
    ggplotly(CumPlot, tooltip = "text")
    
    
    #https://plotly-r.com/controlling-tooltips.html

  })

 
    

########     function for filtering the data - and creating a table of yield gains data on map page #####################

  # #table trial data
 
  output$trial_table <- DT::renderDataTable({
    
    
    trial_results <-filter(yld_table,
                                   site == c(reactive_site_selection()) ) %>%
                                   
      
      dplyr::select(
        
        site,
        Descriptors_new,
        #grouping,
        #modification,
        year,
        crop,
        yr_post_amelioration,
        "yield_unmodified",
        "yield_modified",
        decile,
        rainfall_mean_annual
                    )%>%
      dplyr::rename(Treatment = Descriptors_new,
      ) %>%
      dplyr::filter(!is.na(crop))%>%
      dplyr::mutate(`yield gain` = yield_modified - yield_unmodified)  
     
      trial_results <- left_join(trial_results,order_df, by = c("Treatment" =  "Descriptors_new") ) 
      
      trial_results <- trial_results %>% 
        arrange(order_rank) %>% #arrange the data table using the ranking of treatments
        dplyr::select(Treatment,
                      year,
                      crop,
                      `yield gain`,
                      decile,
                      rainfall_mean_annual)#,
      #order_rank)  
      
      
      

    colnames(trial_results) <- c("Treatment", "Year", "Crop","Yield Gains/ha",  "Decile", "Mean Annual Rainfall")
      
    
    DT::datatable(trial_results ,
                  rownames = FALSE,  
                  #caption = 'Table 1: Yield response t/ha.') %>%
      options = list(columnDefs = 
                       list(list(className = 'dt-center', 
                                 targets = "_all")))) %>%
      formatRound(c(4), 2) %>% #this round clm number 4  to 2 decimal places
      formatRound(c(6), 0) #this round clm  6 to 0 decimal places
    
   
       
  })

  ################################################################################################################################
  ############# Constriants tabel on the first tab ##############################################################################
  ########     function for creating a table of conatraints at the sites                                    #####################
  
  
  constraints_table <- site_info %>%
    mutate(
      Repellence =  case_when(
        non_wetting  == "green"   ~    "No issue",
        non_wetting  == "orange"  ~    "Moderate issue",
        non_wetting  == "red"     ~    "Severe issue",
        TRUE                      ~    "other"
      ))
  
  
  constraints_table <- constraints_table %>%
    mutate(
      Acid =  case_when(
        acidic  == "green"  ~    "No issue",
        acidic  == "orange"  ~    "Moderate issue",
        acidic  == "red"     ~    "Severe issue",
        TRUE                      ~    "other"
      ))
  
  constraints_table <- constraints_table %>%
    mutate(
      Strength =  case_when(
        physical  == "green"   ~    "No issue",
        physical  == "orange"  ~    "Moderate issue",
        physical  == "red"     ~    "Severe issue",
        TRUE                      ~    "other"
      ))
  
  constraints_table <- constraints_table %>%
    mutate(
      Nutrition =  case_when(
        nutrient  == "green"   ~    "No issue",
        nutrient  == "orange"  ~    "Moderate issue",
        nutrient == "red"     ~    "Severe issue",
        TRUE                      ~    "other"
      )) 
  
  
  
  
  output$constraints_table <- DT::renderDataTable({
  
    
    
    constraints_table <- constraints_table %>%
      dplyr::select(site,
                    Repellence,
                    Nutrition,
                    Acid,
                    Strength
                    )
    
    
    
    DT::datatable(constraints_table ,
                  rownames = FALSE,
                  #caption = 'Constaints table', 
                  filter = 'top',
                  options = list(pageLength = 40, dom = 't') #'t' displays only the table and removes the search 
                  
                
                  )
    
  })
  
 
  # output$select_constraints <- renderPrint({
  # 
  #  
  #   choice_of_sites <- constraints_table %>% select(site, 
  #                                           Repellence,
  #                                           Nutrition,
  #                                           Acid,
  #                                           Strength
  #                                           ) %>%
  #     mutate(merge_constriants = paste0(Repellence, "_", Nutrition,  "_", Acid, "_", Strength))
  #     
  #   best_match_table <- left_join(reactive_select_constraints_df(), choice_of_sites) 
  #     
  #     best_match_table <- best_match_table %>%  dplyr::mutate(Note = "If blank no sites match") %>% select(site)
  #   
  #     
  #     # DT::datatable(best_match_table[,c(6,7)], 
  #     #               options = list(dom = 't'),
  #     #               rownames = FALSE,)
  #     
  #     best_match_table
  #     
  #   })

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
    #type = "input",
    text =  
      tagList(
        h3("Acknowledgements"),
        h6("This research has been enriched by preceding research trials, the significant contributions of growers and consultants across the Southern region, 
           # and the support of the GRDC. CSP00203 research and validation activities are a collaboration between the CSIRO, the University of South Australia, the SA state government through Primary Industries and Regions SA, Mallee Sustainable Farming Inc., Frontier Farming Systems, Trengove Consulting, AgGrow Agronomy, AirEP, and MacKillop Farm Management Group."),
        tags$br(),
        tags$img(src = "logos_large.jpg",
                 #height = 185, 
                 width = 250), 
        h3("Disclaimer"),
        h6("This content has been prepared in good faith by CSIRO based on the information available at the date of publication, without any independent verification. 
           Neither CSIRO, its editors nor any contributor to this website content represent, warrant or guarantee that the contents of this website are accurate or complete; nor does CSIRO, its editors nor any contributor to this website content accept any responsibility or liability for any errors or omissions in its contents, however they may arise. 
           Readers who act on any information provided on this website do so at their own risk. 
           CSIRO and contributors to this website may identify products by proprietary or trade names to help readers identify particular types of products. 
           CSIRO do not endorse or recommend the products of any manufacturer referred to. 
          Other products may perform as well as or better than those specifically referred to.")
      ),
    
                    
    #size = "s",
    # closeOnEsc = TRUE,
    # closeOnClickOutside = FALSE,
     html = TRUE,
    # showConfirmButton = TRUE,
    # showCancelButton = FALSE,
    # confirmButtonText = "OK",
    # confirmButtonCol = "#AEDEF4",
    # timer = 0,
    # imageUrl = "",
    # animation = TRUE
  )


  ######################################################################################################
  ##################                     reactivity                              #######################
  ######################################################################################################

  #constraints selection which is on first page
  
  # reactive_select_constraints_df <- reactive({
  #   
  #   Repellence <- c(0)
  #   Nutrition <-   c(0)
  #   Acid <-  c(0)
  #   Strength <-  c(0)
  #   # Join the variables to create a data frame
  #   my_contraints_table <- data.frame(Repellence,Nutrition, Acid, Strength ) 
  #   
  #   ## fill in the tabel with my selected data
  #   
  #   my_contraints_table_df <- my_contraints_table %>% 
  #     mutate(Repellence =  as.character(input$select_constraints_water),
  #            Nutrition =   as.character(input$select_constraints_nutrition),
  #            Acid =        as.character(input$select_constraints_acid),
  #            Strength =    as.character(input$High_soil_strength)
  #            
  #            )#mutate bracket
  #   
  #   my_contraints_table_df <- my_contraints_table_df %>% 
  #     mutate(
  #     merge_constriants = paste0(Repellence, "_", Nutrition,  "_", Acid, "_", Strength))
  #   
  # }  )
  
 
  
  
 #site selection which is used in the plot of yields
  
  reactive_site_selection <- reactive({
     input$site_selection 
  })

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

  


