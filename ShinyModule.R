library("move2")
library("sf")
library("bslib")
library("dplyr")
library("DT")
library("kableExtra")
library("knitr")
library("leaflet")
library("mapview") 
library("plotly")
library("rmarkdown")
library("shiny")
library("shinycssloaders")
library("shinyFiles")
library("shinyjs")
library("shinyWidgets")
library("tidyverse")
library("viridis")

  # helper function 1
  get_alertTable <- function(data){
      # store alert fields
      alerts <- c("mortality","cluster","nsd","voltage","gps_accuracy","gps_transmission","gps_resurrection")
      # subset ids and alert fields
      temp_table <- cbind(id = mt_track_id(data),as.data.frame(data)[,alerts])
      # get unique triggers over each individual
      temp_alerts <- temp_table |> group_by(id) |> summarize(mortality = sum(mortality),cluster = sum(cluster),
                                                  nsd = sum(nsd), voltage = sum(voltage), gps_accuracy = sum(gps_accuracy), 
                                                  gps_transmission = sum(gps_transmission), gps_resurrection = sum(gps_resurrection))
      temp_alerts[,alerts] <- ifelse(temp_alerts[,alerts] > 1, 1, 0)
      temp_alerts <- tidyr::gather(temp_alerts, key = "notification_type", "count",mortality, cluster, nsd, voltage, gps_accuracy, 
                                   gps_transmission, gps_resurrection) |> 
      as.data.frame()
      colnames(temp_alerts)[1] <- mt_track_id_column(data)
      return(temp_alerts)
    }

shinyModuleUserInterface <- function(id, label) {
  # all IDs of UI functions need to be wrapped in ns()
  ns <- NS(id)
  tagList(
      fluidPage(theme = bs_theme(bootswatch = "spacelab", version = 5),
      useShinyjs(),
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: visible; content: 'An error occurred. Please contact the admin.'; }"
      ),
      fluidRow(
        tags$div(tags$h1("Collar Health Shiny App"),style="text-align: center")),
      hr(),
      # Begin sidebarLayout
      sidebarLayout(
        sidebarPanel(width = 3,
            fluidRow(
              column(6,
              h6("Track information")),
              column(6,
              uiOutput(ns("ui_data_alert_switch")))
            ),
            tags$style(type = "text/css", "#info_table{margin-bottom: 5%;}"),  
            DTOutput(ns("info_table")),
            uiOutput(ns("ui_select_individual")),
            uiOutput(ns("ui_notification_type")),
            uiOutput(ns("ui_basemap_type")),
            uiOutput(ns("ui_data_filter")),
            conditionalPanel(
              condition = "input.filter_toggle == 'Date'",
                uiOutput(ns("ui_data_range"))
            ),
            conditionalPanel(
              condition = "input.filter_toggle == 'Number of locations'",
              uiOutput(ns("ui_nlocations"))
            ),
            h6("Data tools"),
            fluidRow(
              column(6,
                     # create ui for actionButton to remove individuals
                     tags$style(type = "text/css", "#delete{margin-top: 5%; margin-bottom: 5%}"),   
                     shiny::actionButton(inputId = ns("delete"), label = "Remove individual", class = "btn-warning")),
              column(6,
                     tags$style(type = "text/css", "#movebank_link{margin-top: 5%; margin-bottom: 5%}"),
                     shiny::actionButton(inputId = ns("movebank_link"), label = "Open movebank", class = "btn-info"))
      
            ),
            # data download features
            fluidRow(
              column(6,
                selectInput(ns("download_select"), label = "Select data", choices = c("All table" = "All","Individual table" = "Individual","Report"),
                            selected = "All")),
              column(6,
                tags$style(type = "text/css", "#downloadData{margin-top: 15%}"),     
                downloadButton(ns("downloadData"), "Download CSV"))),
            uiOutput(ns("ui_data_field_switch")),
            conditionalPanel(
              condition = "input.data_toggle == '1'",
              uiOutput(ns("ui_data_field_filter"))
            )
        ),
        mainPanel(width = 9,
          tabsetPanel(
            # tabPanel 1 - Leaflet map
            tabPanel(title = "Map",
              tags$style(type = "text/css", "#leafletMap {height: 80vh !important;}"),
              leafletOutput(ns("leafletMap"))),
            # tabPanel 2 - All data table   
            tabPanel(title = "All data", 
              DTOutput(ns("all_table"))),
            # tabPanel 3 - Individual data table
            tabPanel(title = "Individual data", 
              DTOutput(ns("ind_table"))),
            # tabPanel 4 - Plot for notification types
            tabPanel(title = "Event plot",
            plotlyOutput(ns("notification_plot"), width = '80%', height = '600px'))
          )
        )
      )
    )  
  )  
}

# The parameter "data" is reserved for the data object passed on from the previous app
shinyModule <- function(input, output, session, data) {
  # all IDs of UI functions need to be wrapped in ns()
  ns <- session$ns
  #current <- reactiveVal(data) # note this is not needed because data is already saved in reactiveValues
   
  # create dataset and alert table as reactiveValues
  rv <- reactiveValues(data = data, table = get_alertTable(data))
  
  # observe button click
  observe({
    req(rv,input$individual_select)
    # Filter data
    rv$data <- rv$data |> filter(.data[[mt_track_id_column(rv$data)]] != input$individual_select)
    rv$table <- get_alertTable(rv$data)
  }) %>% bindEvent(input$delete)
  
  observe({
    req(rv)
    if(input$alert_toggle){
      temp_alert_table <- get_alertTable(rv$data) |> group_by(.data[[mt_track_id_column(rv$data)]]) |> mutate(sumCounts = sum(count)) |> ungroup()
      temp_alert_table <- temp_alert_table |> as.data.frame() |> filter(sumCounts > 0)
      rv$table <- temp_alert_table |> dplyr::select(-sumCounts)
    }else
      if(isFALSE(input$alert_toggle)){
        rv$table <- get_alertTable(rv$data)
      }  
  }) %>% bindEvent(input$alert_toggle)
  
  # create swith to filter individuals by only those with event alerts
  output$ui_data_alert_switch <- renderUI({
    input_switch(id = ns("alert_toggle"), label = "Filter by alerts", value = FALSE)
  })
  
  # build data table for DT with ID, tag_local_identifier, and number of notifications
  output$info_table <- renderDT({
    req(rv)
    # summarize alert table into counts of events over individuals
    summary_table <- rv$table |> group_by(.data[[mt_track_id_column(rv$data)]]) |> summarize(nAlerts = sum(count))
    # get ids and tag ids   into tibble
    tag_info <- mt_track_data(rv$data) |> dplyr::select(mt_track_id_column(rv$data),"tag_local_identifier")
    # merge tag_local_identifier in with summary_table
    summary_table_merged <- merge(summary_table, tag_info, by = mt_track_id_column(rv$data))
    # reorganize table
    summary_table_merged <- summary_table_merged[,c(1,3,2)]
    # update column names
    colnames(summary_table_merged)[1:2] <- c("ind_id","device_id")
    #    # create data table
    DT::datatable(as.data.frame(summary_table_merged), rownames = FALSE, selection = "single",
                  options = list(scrollY = "150px", 
                                 scrollX = TRUE,  
                                 paging = FALSE,
                                 lengthChange = FALSE,
                                 searching = FALSE,
                                 info = FALSE,
                                 pageLength = 5,
                                 columnDefs = list(
                                   list(className = 'dt-left', targets = "_all"))))
  }) 
  
  # select individual
  output$ui_select_individual <- renderUI({
    req(rv$table)
    selectInput(ns("individual_select"), label = "Select individual", choices = unique(rv$table[,mt_track_id_column(rv$data)]), 
                selected = rv$table[1,mt_track_id_column(rv$data)])
  })
  
  # Observe clicks/selections in the info table
  observeEvent(input$info_table_rows_selected, {
    req(rv)
    # Get the row index
    selected_row <- input$info_table_rows_selected
    # Get the value from the data frame (e.g., Species column)
    new_value <- unique(rv$table[,mt_track_id_column(rv$data)])[selected_row]
    # Update the selectInput
    updateSelectInput(session, ns("individual_select"), selected = new_value)
  })
  
  # Create the proxy object for info table
  proxy_info_table <- dataTableProxy(ns("info_table"))
  
  # Update the selected row when the dropdown changes
  observeEvent(input$individual_select, {
    req(rv)
    row_to_select <- which(unique(rv$table[,mt_track_id_column(rv$data)]) == input$individual_select)
    selectRows(proxy_info_table, row_to_select)
  })
  
  # update selectInput for notification type depending on individual that is selected
  observeEvent(input$individual_select, {
    req(rv)
    update_choices = rv$table |> filter(rv$table[,mt_track_id_column(rv$data)] == input$individual_select &
                                               count > 0) |> dplyr::select(notification_type) |> unique()
    # now update the selectInput
    updateSelectInput(session,
                      inputId = ns("notification_type"),
                      choices = update_choices$notification_type,
                      selected = update_choices$notification_type[1]
    )
  }) 

  # radioButton for filter type
  output$ui_data_filter <- renderUI({
    radioButtons(inputId = ns("filter_toggle"), label = "Select filter",
                 choices = c("Date","Number of locations"), selected = "Date", inline = TRUE)
  })
  
  # filter based on data range
    output$ui_data_range  <- renderUI({
      min_date = suppressWarnings(min(as.Date(filter_track_data(rv$data, .track_id = input$individual_select) |> mt_time())))
      max_date = suppressWarnings(max(as.Date(filter_track_data(rv$data, .track_id = input$individual_select) |> mt_time())))
      req(as.character(min_date) != "Inf" && as.character(max_date) != "Inf")
      dateRangeInput(inputId = ns("date_range"), label = "Select date range", 
                     start = min_date, end = max_date,
                     min = min_date, max = max_date)
    })

  # filter based on number of locations
  output$ui_nlocations <- renderUI({
    max_rows <- nrow(filter_track_data(rv$data, .track_id = input$individual_select))
    req(max_rows >= 1)
    sliderInput(inputId = ns("number_locations"), label = "Select number of locations", 
                min = 1, max = max_rows, value = max_rows, step = 1)
  }) 
  
  # data filter for overall individual
  data_individual <- reactive({
    req(rv$data,input$date_range,input$number_locations,input$individual_select)
    filtered_data <- filter_track_data(rv$data, .track_id = input$individual_select)
    if(input$filter_toggle == "Date"){
      filtered_data2 <- filtered_data |> filter(between(as.Date(mt_time(filtered_data)), 
                                                        as.Date(input$date_range[1]),as.Date(input$date_range[2])))
    }else
      if(input$filter_toggle == "Number of locations"){
        filtered_data2 <- filtered_data |> slice_tail(n=input$number_locations)
      }
    return(filtered_data2)
  }) 
  
  # data filter for notification and individual
  data_individual_notification <- reactive({
    req(data_individual(),input$notification_type)
    filtered_data_notification <- data_individual() |> filter(.data[[input$notification_type]] == 1)
    return(filtered_data_notification)
  }) 
  
  # select individual
  output$ui_select_individual <- renderUI({
    req(rv)
    selectInput(ns("individual_select"), label = "Select individual", choices = unique(rv$table[,mt_track_id_column(rv$data)]), 
                selected = rv$table[1,mt_track_id_column(rv$data)])
  })
  
  # select notification type
  output$ui_notification_type  <- renderUI({
    req(rv$table)
    selectInput(ns("notification_type"), label = "Select event", 
                choices = unique(rv$table$notification_type),
                selected = unique(rv$table$notification_type)[1])
  })
  
  # make checkBoxGroup for data fields to include in table
  output$ui_data_field_filter <- renderUI({
    req(data_individual(),input$notification_type,rv$data)
    available_colnames <- colnames(data_individual())[1:(ncol(data_individual())-1)]
    alerts <- c("mortality","cluster","nsd","voltage","gps_accuracy","gps_transmission","gps_resurrection")
    # remove current input$notification from alerts vector
    alerts <- alerts[-which(alerts == input$notification_type)]
    available_colnames <- available_colnames[-which(available_colnames %in% alerts)]
    # add tag_local_identifier to available_colnames
    available_colnames <- c(available_colnames,"device_id")
    # include mortality_status in default selected fields if it is present in the data
    if("mortality_status" %in% available_colnames){
      # field indices to reorganize
      field_indices <- which(available_colnames %in% c(mt_track_id_column(rv$data),"device_id",
                                                       mt_time_column(rv$data),input$notification_type,
                                                       "mortality_status"))
      # reorganize data fields
      available_colnames <- c(mt_track_id_column(rv$data),"device_id","Latitude","Longitude",mt_time_column(rv$data),
                              input$notification_type,"mortality_status","n_locs",available_colnames[-field_indices])
      # set number of default fields to show
      n_fields <- 7
    }else
    if(isFALSE("mortality_status" %in% available_colnames)){
      # field indices to reorganize
      field_indices <- which(available_colnames %in% c(mt_track_id_column(rv$data),"device_id",
                                                       mt_time_column(rv$data),input$notification_type))
      # reorganize data fields
      available_colnames <- c(mt_track_id_column(rv$data),"device_id","Latitude","Longitude",mt_time_column(rv$data),
                              input$notification_type,"n_locs",available_colnames[-field_indices])
      # set number of default fields to show
      n_fields <- 6
    }  
    # remove notification type if there are none present
    if(sum(as.data.frame(data_individual())[,input$notification_type])==0){
      available_colnames <- available_colnames[-which(available_colnames == input$notification_type)]
    }
    # remove alias and value field
    if(any(available_colnames %in% c("mortality_alias","mortality_value"))){
      available_colnames <- available_colnames[-which(available_colnames %in% c("mortality_alias","mortality_value"))]
    }
    if(any(available_colnames %in% c("voltage_alias","voltage_value"))){
      available_colnames <- available_colnames[-which(available_colnames %in% c("voltage_alias","voltage_value"))]
    }
    if(any(available_colnames %in% c("gps_accuracy_alias","gps_accuracy_value"))){
      available_colnames <- available_colnames[-which(available_colnames %in% c("gps_accuracy_alias","gps_accuracy_value"))]
    }
    # now populate in checkboxGroupInput
    checkboxGroupInput(inputId = ns("data_fields"), label = "Select data fields",
                       choices = available_colnames,
                       selected = available_colnames[1:n_fields])
  })
  
  output$ui_data_field_switch <- renderUI({
    input_switch(id = ns("data_toggle"), label = "Show data fields", value = TRUE)
  })
  
  # make reactive data for all_table output and downloading features
  all_table_data <- reactive({
    req(rv)
    # summarize alert table into counts of events over individuals
    all_table <- rv$table |> group_by(.data[[mt_track_id_column(rv$data)]]) |> summarize(nAlerts = sum(count)) |> ungroup() |>
      as.data.frame()
    # filter alert data by ids in all_table
    alert_data <- filter_track_data(rv$data, .track_id = unique(all_table[,mt_track_id_column(rv$data)]))
    # get tag_id field
    tag_id_info <- mt_track_data(alert_data)[,c(mt_track_id_column(alert_data),"tag_local_identifier")]
    # change tag_id to device_id
    colnames(tag_id_info)[2] <- "device_id"
    # merge in tag into into all_table
    all_table <- merge(all_table, tag_id_info, by = mt_track_id_column(alert_data))
    # add number of locations per individual
    n_locs <- as.data.frame(alert_data) |> count(.data[[mt_track_id_column(alert_data)]]) 
    # merge in n_locs into all_table
    all_table <- merge(all_table, n_locs, by = mt_track_id_column(alert_data))
    # rename n to n_locs
    colnames(all_table)[which(colnames(all_table) == "n")] <- "n_locs"
    # ge tmost recent coordinates adn timestamp
    recent_locations <- alert_data %>%
      group_by(.data[[mt_track_id_column(alert_data)]]) %>%
      slice_max(order_by = .data[[mt_time_column(alert_data)]], n = 1, with_ties = FALSE) %>%
      ungroup()
    all_table$Latitude <- (recent_locations |> st_coordinates())[,2]
    all_table$Longitude <- (recent_locations |> st_coordinates())[,1]
    all_table$time_field <- mt_time(recent_locations)
    # rename time field
    colnames(all_table)[which(colnames(all_table) == "time_field")] <- mt_time_column(alert_data)
    # create 0/1 indicator for each event type
    sum_table <- rv$table |> group_by(.data[[mt_track_id_column(alert_data)]],notification_type) |> summarize(nAlerts = sum(count), .groups = "drop_last")
    # use spread to put table in wide form
    wide_table <- spread(sum_table, notification_type, nAlerts)
    # now merge in sum_table with all_table
    all_table <- merge(all_table, wide_table, by = mt_track_id_column(alert_data))
    # now rearrange columns
    all_table <- all_table[,-2]
    # format timestamp
    all_table[,mt_time_column(alert_data)] <- as.character(paste(all_table[,mt_time_column(alert_data)],"UTC"))
    return(all_table)
  })
  
  # make datatable for all data (one row for each individual)
  output$all_table <- renderDT({
    # return data table
    DT::datatable(all_table_data(), extensions = c("FixedHeader"),rownames = FALSE, selection = "single", 
                  options = list(scrollY = "600px", 
                                 scrollX = TRUE, 
                                 paging = FALSE,
                                 lengthChange = FALSE,
                                 info = FALSE,
                                 fixedHeader=TRUE))  %>%
      formatRound(columns = c('Latitude', 'Longitude'), digits = 6)
  })
  
  # Observe clicks/selections in the all data table
  observeEvent(input$all_table_rows_selected, {
    req(rv)
    # Get the row index
    selected_row <- input$all_table_rows_selected
    # Get the value from the data frame (e.g., Species column)
    new_value <- unique(rv$table[,mt_track_id_column(rv$data)])[selected_row]
    # Update the selectInput
    updateSelectInput(session, ns("individual_select"), selected = new_value)
  })
  
  # Create the proxy object for all data table
  proxy_all_table <- dataTableProxy(ns("all_table"))
  
  # Update the selected row when the dropdown changes
  observeEvent(input$individual_select, {
    req(rv)
    row_to_select <- which(unique(rv$table[,mt_track_id_column(rv$data)]) == input$individual_select)
    selectRows(proxy_all_table, row_to_select)
  })
  

  # make reactive output for ind table and downloading features
  ind_table_data <- reactive({
    req(nrow(data_individual()) > 0,input$data_fields,rv$data,input$individual_select)
    # store lat/longs from move2 object
    Latitude <- st_coordinates(data_individual())[,2]
    Longitude <- st_coordinates(data_individual())[,1]
    # store move2 data as data frame
    ind_data <- data_individual() |> as.data.frame()
    # get tag_local_identifier info
    tag_data <- mt_track_data(data_individual())[,c(mt_track_id_column(data_individual()),"tag_local_identifier")]
    # change tag_local_identifier to device_id
    colnames(tag_data)[2] <- "device_id"
    # merge in tag into into all_table
    ind_data <- merge(ind_data, tag_data, by = mt_track_id_column(data_individual()))
    # get number of locations
    ind_data$n_locs <- nrow(filter_track_data(rv$data, .track_id = input$individual_select))
    # now append Lat/Longs
    ind_data <- cbind(Latitude,Longitude,ind_data)
    # reverse the data table to show most recent locations first?
    ind_data <- ind_data[rev(1:nrow(ind_data)),]
    # format timestamp as character
    ind_data[,mt_time_column(data_individual())] <- as.character(paste(ind_data[,mt_time_column(data_individual())],"UTC"))
    # filter by input data fields
    return(ind_data[,input$data_fields])
  })
  
  # make datatable for individual data
    output$ind_table <- renderDT({
      req(nrow(ind_table_data())>0)
      # render data table
      DT::datatable(ind_table_data(), 
                    extensions = c("FixedHeader"),
                    colnames = input$data_fields,
                    rownames = FALSE, 
                    selection = "single", 
                    options = list(scrollY = "600px", 
                                   scrollX = TRUE, 
                                   paging = FALSE,
                                   lengthChange = FALSE,
                                   info = FALSE,
                                   fixedHeader=TRUE)) %>%
        formatRound(columns = c('Latitude', 'Longitude'), digits = 6)
    }, server = TRUE) 
  
  # select basemap type
  output$ui_basemap_type  <- renderUI({
    selectInput(ns("basemap_type"), label = "Select basemap", choices = c("World Imagery","World Topo Map","World Street Map",
                                                                      "NatGeo World Map","OpenStreet Map","OpenStreet Topo Map"),selected = "World Imagery")
  })
  
  # determine basemap type
  basemap <- reactive({
    if(input$basemap_type == "World Imagery"){
      return(providers$Esri.WorldImagery)
    }else
      if(input$basemap_type == "World Topo Map"){
        return(providers$Esri.WorldTopoMap)
      }else
        if(input$basemap_type == "World Street Map"){
          return(providers$Esri.WorldStreetMap) 
        }else  
          if(input$basemap_type == "NatGeo World Map"){
            return(providers$Esri.NatGeoWorldMap)
          }else
            if(input$basemap_type == "OpenStreet Map"){
              return(providers$OpenStreetMap)
            }else
              if(input$basemap_type == "OpenStreet Topo Map"){
                return(providers$OpenTopoMap)
              }  
  })
  
  # determine color of base locations
  linesColor <- reactive({
    if(input$basemap_type == "World Imagery"){
      return("yellow")
    }else
      if(input$basemap_type == "World Topo Map"){
        return("black")
      }else
        if(input$basemap_type == "World Street Map"){
          return("black")
        }else  
          if(input$basemap_type == "NatGeo World Map"){
            return("black")
          }else
            if(input$basemap_type == "OpenStreet Map"){
              return("black")
            }else
              if(input$basemap_type == "OpenStreet Topo Map"){
                return("black")
              }
  })
  
# create leaflet map for plotting with choice of basemap
leaf_map <- reactive({
  req(input$basemap_type,linesColor())
  if(nrow(data_individual_notification()) > 0){
    map1 <- leaflet() %>% 
            # add scale bar
            addScaleBar(position = "bottomleft", 
                        options = scaleBarOptions(maxWidth = 200, metric = TRUE, imperial= FALSE)) %>%   
            # add user-selected basemap
            addProviderTiles(basemap()) %>% 
            # add track lines for individual
            addPolylines(data = mt_track_lines(data_individual()),
                         weight = 2,
                         color = linesColor(),
                         opacity = 0.8) %>%
            # add all data points
            addCircles(data = data_individual(),
                       opacity = 0.3,
                       label = ~timestamp,
                       fillOpacity = 0.8,
                       radius = 10, 
                       color = "blue",
                       fillColor = "blue") %>%
            # add locations associated with selected notification
            addCircles(data = data_individual_notification(),
                       opacity = 0.8,
                       label = ~timestamp,
                       fillOpacity = 0.8, 
                       radius = 10, 
                       color = "#FF991C", 
                       fillColor = "#FF991C")  %>%
            # add locations to denote start and end of track
            addCircleMarkers(data = data_individual() |> slice(c(1, n())),
                             label = c("Start","End"),
                             fillOpacity = 1,
                             radius = 10,
                             color = c("green","red"),
                             fillColor = c("green","red"))
  }else
    if(nrow(data_individual_notification()) == 0){ 
      map1 <- leaflet() %>% 
              # add scale bar
              addScaleBar(position = "bottomleft", 
                          options = scaleBarOptions(maxWidth = 200, metric = TRUE, imperial = FALSE)) %>%   
              # add user-selected basemap
              addProviderTiles(basemap()) %>% 
              # add track lines for individual
              addPolylines(data = mt_track_lines(data_individual()),
                           weight = 2,
                           color = linesColor(),
                           opacity = 0.8) %>%
              # add all data points
              addCircles(data = data_individual(),
                         opacity = 0.3,
                         label = ~timestamp,
                         fillOpacity = 0.8,
                         radius = 10, 
                         color = "blue",
                         fillColor = "blue") %>%
              # add locations to denote start and end of track
              addCircleMarkers(data = data_individual() |> slice(c(1, n())),
                               label = c("Start","End"),
                               fillOpacity = 1,
                               radius = 10,
                               color = c("green","red"),
                               fillColor = c("green","red"))
    }
    return(map1)
})

  # now render leaflet map
  output$leafletMap <- renderLeaflet({
    leaf_map()
  })

# store user-adjusted leaflet map (zoom, and coordinates)
user_map <- reactive({
  # call the Leaflet map
  leaf_map() %>%
  # store the view based on UI
  setView(lng = input$leafletMap_center$lng,
          lat = input$leafletMap_center$lat,
          zoom = input$leafletMap_zoom)
})    
  
# update url link as individual ID changes
observeEvent(input$movebank_link, {
  req(data_individual())
  # store study id from track data
  movebank_studyID <- mt_track_data(data_individual()) |> dplyr::select(study_id)
  # store individual id from track data
  movebank_individualID <- mt_track_data(data_individual()) |> dplyr::select(individual_id)
  # get deployment id fromt rack data
  movebank_deploymentID <- mt_track_data(data_individual()) |> dplyr::select(deployment_id)
  # store movebank link
  collar_link <- paste0("window.open('","https://www.movebank.org/cms/webapp?gwt_fragment=page=studies,path=study",movebank_studyID$study_id,"+individual",movebank_individualID$individual_id,"+deployment",movebank_deploymentID$deployment_id,"')")
  # now open web page when button is clicked
  shinyjs::runjs(collar_link)
})

# plot for alert based notification event type
output$notification_plot <- renderPlotly({
  req(data_individual(),input$notification_type)
  # make a null plot if there are no events to plot
if(unique(data_individual()$nAlerts) == 0){
  ggplot() + theme_void()
}else
  # store plot colors
  plot_color <- magma(100)[c(30,70)]
  if(input$notification_type == "mortality" & unique(data_individual()$nAlerts) > 0){
  # make time series plot that adjusts based on notification type
  plot_data <- data_individual() |> as.data.frame() |> dplyr::select(mt_time_column(data_individual()),input$notification_type) 
  # rename first column to timestamp
  colnames(plot_data)[1] = "timestamp"
  # create mortality status as a factor based on event indicator variable
  plot_data$mortality_status <- character(nrow(plot_data))
  plot_data$mortality_status[which(plot_data$mortality == 1)] <- "Mortality"
  plot_data$mortality_status[which(plot_data$mortality == 0)] <- "Nothing detected"
  # now convert to a factor
  plot_data$mortality_status <- as.factor(plot_data$mortality_status)
  # reset levels
  plot_data$mortality_status <- relevel(plot_data$mortality_status, ref = "Nothing detected")
  gg1 <- ggplot(plot_data, aes(x = timestamp, y = mortality, group = mortality_status,
                           color = mortality_status,
                           text = paste("</br>Date:",timestamp,
                                        "</br>Status:",mortality_status))) + 
  geom_point() + scale_y_continuous(breaks = c(0,1)) + scale_color_manual(values = plot_color) +
  xlab("Date") + ylab("Mortality indicator") + theme_bw() +
  theme(axis.text.x = element_text(size = 16, angle = 45, hjust = 1, vjust = 1),
      axis.text.y = element_text(size = 16),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 18)) +
  guides(color=guide_legend(title="Mortality status")) 
  ggplotly(gg1, tooltip = c("text")) %>%
  layout(legend = list(orientation = "h", x = 0.5, y = 1.05, xanchor = "center", yanchor = "bottom"))
}else
if(input$notification_type == "cluster"& unique(data_individual()$nAlerts) > 0){
  # make time series plot that adjusts based on notification type
  plot_data <- data_individual() |> as.data.frame() |> dplyr::select(mt_time_column(data_individual()),input$notification_type) 
  # rename first column to timestamp
  colnames(plot_data)[1] = "timestamp"
  # create cluster status as a factor on event indicator variable
  plot_data$cluster_status <- character(nrow(plot_data))
  plot_data$cluster_status[which(plot_data$cluster == 1)] <- "In cluster"
  plot_data$cluster_status[which(plot_data$cluster == 0)] <- "Not in cluster"
  # now convert to a factor
  plot_data$cluster_status <- as.factor(plot_data$cluster_status)
  # reset levels
  plot_data$cluster_status <- relevel(plot_data$cluster_status, ref = "Not in cluster")
  gg2 <- ggplot(plot_data, aes(x = timestamp, y = cluster, group = cluster_status,
                             color = cluster_status,
                             text = paste("</br>Date:",timestamp,
                                          "</br>Status:",cluster_status))) + 
  geom_point() + scale_y_continuous(breaks = c(0,1)) + scale_color_manual(values = plot_color) +
  xlab("Date") + ylab("Cluster indicator") + theme_bw() +
  theme(axis.text.x = element_text(size = 16, angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18)) +
  guides(color=guide_legend(title="Cluster status")) 
  ggplotly(gg2, tooltip = c("text")) %>%
  layout(legend = list(orientation = "h", x = 0.5, y = 1.05, xanchor = "center", yanchor = "bottom"))
}else
if(input$notification_type == "nsd"& unique(data_individual()$nAlerts) > 0){
    # make time series plot that adjusts based on notification type
    plot_data <- data_individual() |> as.data.frame() |> dplyr::select(mt_time_column(data_individual()),input$notification_type) 
    # rename first column to timestamp
    colnames(plot_data)[1] = "timestamp"
    # create nsd status as a factor based on event indicator variable
    plot_data$nsd_status <- character(nrow(plot_data))
    plot_data$nsd_status[which(plot_data$nsd == 1)] <- "Above max NSD"
    plot_data$nsd_status[which(plot_data$nsd == 0)] <- "Not above max NSD"
    # now convert to a factor
    plot_data$nsd_status <- as.factor(plot_data$nsd_status)
    # reset levels
    plot_data$nsd_status <- relevel(plot_data$nsd_status, ref = "Not above max NSD")
    gg3 <- ggplot(plot_data, aes(x = timestamp, y = nsd, group = nsd_status,
                                 color = nsd_status,
                                 text = paste("</br>Date:",timestamp,
                                              "</br>Status:", nsd_status))) + 
    geom_point() + scale_y_continuous(breaks = c(0,1)) + scale_color_manual(values = plot_color) +
    xlab("Date") + ylab("NSD indicator") + theme_bw() +
    theme(axis.text.x = element_text(size = 16, angle = 45, hjust = 1, vjust = 1),
          axis.text.y = element_text(size = 16),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 18)) +
    guides(color=guide_legend(title="NSD status")) 
  ggplotly(gg3, tooltip = c("text")) %>%
    layout(legend = list(orientation = "h", x = 0.5, y = 1.05, xanchor = "center", yanchor = "bottom"))
}else
if(input$notification_type == "voltage" & unique(data_individual()$nAlerts) > 0){
    # make time series plot that adjusts based on notification type
    plot_data <- data_individual() |> as.data.frame() |> dplyr::select(mt_time_column(data_individual()),
                                input$notification_type,data_individual()$voltage_alias[1]) 
    # rename first column to timestamp
    colnames(plot_data)[1] = "timestamp"
    # create voltage status as a factor based on FID
    plot_data$voltage_status <- character(nrow(plot_data))
    plot_data$voltage_status[which(plot_data$voltage == 1)] <- "At or below threshold"
    plot_data$voltage_status[which(plot_data$voltage == 0)] <- "Above threshold"
    # now convert to a factor
    plot_data$voltage_status <- as.factor(plot_data$voltage_status)
    # reset levels
    plot_data$voltage_status <- relevel(plot_data$voltage_status, ref = "Above threshold")
    # remove NA values that are present
    if(any(is.na(plot_data[,data_individual()$voltage_alias[1]]))){
    plot_data <- plot_data |> filter(is.na(.data[[data_individual()$voltage_alias[1]]]) == FALSE)
    }
    # store quantile if voltage value == ""
    if(data_individual()$voltage_value[1] == ""){
      voltage_value <- quantile(as.numeric(plot_data[,data_individual()$voltage_alias[1]]), probs = 0.25, na.rm = TRUE)
    }else
    if(data_individual()$voltage_value[1] < 1){
      voltage_value <- quantile(as.numeric(plot_data[,data_individual()$voltage_alias[1]]), probs = data_individual()$voltage_value[1], na.rm = TRUE)
    }else
    if(data_individual()$voltage_value[1] >= 1){   
      voltage_value = data_individual()$voltage_value[1]
    }
    gg4 <- ggplot(plot_data, aes(x = timestamp, y = as.numeric(tag_voltage), group = voltage_status,
                                 color = voltage_status, 
                                 text = paste("</br>Date:",timestamp,
                                              "</br>Status:", voltage_status,
                                              "</br>Voltage:", tag_voltage))) + 
    geom_hline(yintercept = voltage_value, color = "gray60", linewidth = 1, linetype = "dotted") +
    geom_point() + scale_color_manual(values = plot_color) +
    xlab("Date") + ylab("Voltage (mV)") + theme_bw() +
    theme(axis.text.x = element_text(size = 16, angle = 45, hjust = 1, vjust = 1),
          axis.text.y = element_text(size = 16),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 18)) +
    guides(color=guide_legend(title="Voltage status")) 
  ggplotly(gg4, tooltip = c("text")) %>%
    layout(legend = list(orientation = "h", x = 0.5, y = 1.05, xanchor = "center", yanchor = "bottom"))
}else
  if(input$notification_type == "gps_accuracy" & unique(data_individual()$nAlerts) > 0){
    # make time series plot that adjusts based on notification type
    plot_data <- data_individual() |> as.data.frame() |> dplyr::select(mt_time_column(data_individual()),input$notification_type) 
    # rename first column to timestamp
    colnames(plot_data)[1] = "timestamp"
    # create gps_accuracy as a factor on event indicator variable
    plot_data$gps_accuracy_status <- character(nrow(plot_data))
    plot_data$gps_accuracy_status[which(plot_data$gps_accuracy == 1)] <- "2D GPS Fix or failed"
    plot_data$gps_accuracy_status[which(plot_data$gps_accuracy == 0)] <- "3D GPS Fix"
    # now convert to a factor
    plot_data$gps_accuracy_status <- as.factor(plot_data$gps_accuracy_status)
    # reset levels
    plot_data$gps_accuracy_status <- relevel(plot_data$gps_accuracy_status, ref = "2D GPS Fix or failed")
    gg5 <- ggplot(plot_data, aes(x = timestamp, y = gps_accuracy, group = gps_accuracy_status,
                                 color = gps_accuracy_status,
                                 text = paste("</br>Date:",timestamp,
                                              "</br>Status:",gps_accuracy_status))) + 
      geom_hline(yintercept = data_individual()$gps_accuracy_prop[1], color = "gray60", linewidth = 1, linetype = "dotted") +
      geom_point() + scale_y_continuous(breaks = c(0,1)) + scale_color_manual(values = plot_color) +
      xlab("Date") + ylab("GPS accuracy indicator") + theme_bw() +
      theme(axis.text.x = element_text(size = 16, angle = 45, hjust = 1, vjust = 1),
            axis.text.y = element_text(size = 16),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 18)) +
      guides(color=guide_legend(title="GPS accuracy status")) 
    ggplotly(gg5, tooltip = c("text")) %>%
      layout(legend = list(orientation = "h", x = 0.5, y = 1.05, xanchor = "center", yanchor = "bottom"))
  }else
  if(input$notification_type == "gps_transmission" & unique(data_individual()$nAlerts) > 0){
      # make time series plot that adjusts based on notification type
      plot_data <- data_individual() |> as.data.frame() |> dplyr::select(mt_time_column(data_individual()),input$notification_type) 
      # rename first column to timestamp
      colnames(plot_data)[1] = "timestamp"
      # create gps_transmission as a factor on event indicator variable
      plot_data$gps_transmission_status <- character(nrow(plot_data))
      plot_data$gps_transmission_status[which(plot_data$gps_transmission == 1)] <- "Transmission gap"
      plot_data$gps_transmission_status[which(plot_data$gps_transmission == 0)] <- "Normal transmission"
      # now convert to a factor
      plot_data$gps_transmission_status <- as.factor(plot_data$gps_transmission_status)
      # reset levels
      plot_data$gps_transmission_status <- relevel(plot_data$gps_transmission_status, ref = "Normal transmission")
      gg6 <- ggplot(plot_data, aes(x = timestamp, y = gps_transmission, group = gps_transmission_status,
                                   color = gps_transmission_status,
                                   text = paste("</br>Date:",timestamp,
                                                "</br>Status:",gps_transmission_status))) + 
      geom_point() + scale_y_continuous(breaks = c(0,1)) + scale_color_manual(values = plot_color) +
      xlab("Date") + ylab("GPS transmission indicator") + theme_bw() +
      theme(axis.text.x = element_text(size = 16, angle = 45, hjust = 1, vjust = 1),
            axis.text.y = element_text(size = 16),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 18)) +
      guides(color=guide_legend(title="GPS transmission status")) 
    ggplotly(gg6, tooltip = c("text")) %>%
      layout(legend = list(orientation = "h", x = 0.5, y = 1.05, xanchor = "center", yanchor = "bottom"))
  }else
    if(input$notification_type == "gps_resurrection" & unique(data_individual()$nAlerts) > 0){
      # make time series plot that adjusts based on notification type
      plot_data <- data_individual() |> as.data.frame() |> dplyr::select(mt_time_column(data_individual()),input$notification_type) 
      # rename first column to timestamp
      colnames(plot_data)[1] = "timestamp"
      # create gps_transmission as a factor on event indicator variable
      plot_data$gps_resurrection_status <- character(nrow(plot_data))
      plot_data$gps_resurrection_status[which(plot_data$gps_resurrection == 1)] <- "Resurrected"
      plot_data$gps_resurrection_status[which(plot_data$gps_resurrection == 0)] <- "Normal transmission"
      # now convert to a factor
      plot_data$gps_resurrection_status <- as.factor(plot_data$gps_resurrection_status)
      # reset levels
      plot_data$gps_resurrection_status <- relevel(plot_data$gps_resurrection_status, ref = "Normal transmission")
      gg7 <- ggplot(plot_data, aes(x = timestamp, y = gps_resurrection, group = gps_resurrection_status,
                                   color = gps_resurrection_status,
                                   text = paste("</br>Date:",timestamp,
                                                "</br>Status:",gps_resurrection_status))) + 
        geom_point() + scale_y_continuous(breaks = c(0,1)) + scale_color_manual(values = plot_color) +
        xlab("Date") + ylab("GPS resurrection indicator") + theme_bw() +
        theme(axis.text.x = element_text(size = 16, angle = 45, hjust = 1, vjust = 1),
              axis.text.y = element_text(size = 16),
              axis.title.x = element_text(size = 20),
              axis.title.y = element_text(size = 20),
              legend.text = element_text(size = 14),
              legend.title = element_text(size = 18)) +
        guides(color=guide_legend(title="GPS resurrection status")) 
      ggplotly(gg7, tooltip = c("text")) %>%
        layout(legend = list(orientation = "h", x = 0.5, y = 1.05, xanchor = "center", yanchor = "bottom"))  
    }
    # end of plots   
  })
  
  # download data functions
  output$downloadData <- downloadHandler(
    filename = function() {
      if(input$download_select == "All"){
      paste("data-all_", Sys.Date(), ".csv", sep = "")
      }else
      if(input$download_select == "Individual"){
      paste("data-individual_", Sys.Date(), ".csv", sep = "")
      }else
      if(input$download_select == "Report"){
      paste("collar-health-report_",input$individual_select,"_", Sys.Date(), ".html", sep = "") 
      }  
    },
    content = function(file){
      if(input$download_select == "All"){
        write.csv(all_table_data(), file, row.names = FALSE)
      }else
      if(input$download_select == "Individual"){
        write.csv(ind_table_data(), file, row.names = FALSE)  
      }else
      if(input$download_select == "Report"){
        # save leaflet map as image to temp file
        temp_dir <- tempdir()
        mapshot2(user_map(), 
                file = paste0(temp_dir,"/leaflet.png"))
        out <- rmarkdown::render(input = getAuxiliaryFilePath("auxiliary-file-a"), output_format = "html_document")
        file.rename(out, file)
      }
    }
  )

  # end of server
  
  # data must be returned. Either the unmodified input data, or the modified data by the app
  return(reactive({rv$data}))
}
