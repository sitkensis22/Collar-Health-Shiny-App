library("move2")
library("sf")
library("bslib")     
library("dplyr")
library("DT")
library("htmltools")
library("kableExtra")
library("knitr")
library("leaflet")
library("plotly")
library("rmarkdown")
library("shiny")
library("shinycssloaders")
library("shinyFiles")
library("shinyjs")
library("shinyWidgets")
library("tidyverse")
library("viridis")
library("xml2")
library("zip")

  # helper function 1
  get_alertTable <- function(data){
      # store alert fields
      alerts <- c("mortality","resurrection","cluster","nsd","voltage","gps_accuracy","gps_transmission","gps_resurrection","tag_release")
      # subset ids and alert fields
      temp_table <- cbind(id = mt_track_id(data),as.data.frame(data)[,alerts])
      # get unique triggers over each individual
      temp_alerts <- temp_table |> group_by(id) |> summarize(mortality = sum(mortality), resurrection = sum(resurrection), cluster = sum(cluster),
                                                  nsd = sum(nsd), voltage = sum(voltage), gps_accuracy = sum(gps_accuracy), 
                                                  gps_transmission = sum(gps_transmission), gps_resurrection = sum(gps_resurrection),
                                                  tag_release = sum(tag_release))
      temp_alerts[,alerts] <- ifelse(temp_alerts[,alerts] > 0, 1, 0)
      temp_alerts <- tidyr::gather(temp_alerts, key = "notification_type", "count",mortality, resurrection, cluster, nsd, voltage,  
                                   gps_accuracy, gps_transmission, gps_resurrection, tag_release) |> as.data.frame()
      colnames(temp_alerts)[1] <- mt_track_id_column(data)
      # check to see if deployment_end_type exists, if not, add it and make all tags active
      if(isFALSE("deployment_end_type" %in% colnames(mt_track_data(data)))){
         data <- data |> mutate_track_data(deployment_end_type = rep("active",nrow(mt_track_data(data))))
      }
      # get ids,status
      tag_status <- mt_track_data(data) |> dplyr::select(mt_track_id_column(data),"deployment_end_type")
      # set deployment_end_type to "status"
      colnames(tag_status)[2] = c("status")
      # add level to summary_table_merged$status
      tag_status$status <- factor(tag_status$status, levels = c(levels(tag_status$status), "active"))
      # set NA to status to active
      tag_status$status[which(is.na(tag_status$status))]  <- "active"
      # merge in deployment_end_type
      temp_alerts <- merge(temp_alerts, tag_status, by = mt_track_id_column(data))
      # get most recent timestamp for each individual
      last_locations <- data |> 
        group_by(.data[[mt_track_id_column(data)]]) |> 
        slice_max(order_by = .data[[mt_time_column(data)]], n = 1) |> as.data.frame() |>
        select(mt_track_id_column(data),mt_time_column(data))
      # merge in deployment_end_type
      temp_alerts <- merge(temp_alerts, last_locations, by = mt_track_id_column(data))
      # return temp_alerts
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
            h6("Track information"),
            fluidRow(
              column(6,
                 uiOutput(ns("ui_data_alert_switch"))),
              column(6,
                 uiOutput(ns("ui_data_status_switch")))     
            ),
          div(style = "margin-bottom: 5%;",
              DTOutput(ns("info_table")), 
          ),
            #uiOutput(ns("ui_map_all")),
            input_switch(id = ns("map_all"), label = "Map all individuals", value = FALSE), 
            uiOutput(ns("ui_select_individual")),
            uiOutput(ns("ui_notification_type")),
            uiOutput(ns("ui_basemap_type")),
            # dynamic control for data filter controls based on input$map_all
            #uiOutput(ns("dynamic_filter_toggle")),
            radioButtons(inputId = ns("filter_toggle"), label = "Select filter",
                       choices = c("Date","Number of locations"), selected = "Date", inline = TRUE),
            uiOutput(ns("dynamic_data_filter")),
              h6("Data tools"),
              fluidRow(
                column(6,
                       # create ui for actionButton to remove individuals
                       div(style = "margin-bottom: 5%;",   
                       shiny::actionButton(inputId = ns("delete"), label = "Remove individual", class = "btn-warning"))),
                column(6,
                       div(style = "margin-bottom: 5%;",
                       shiny::actionButton(inputId = ns("movebank_link"), label = "Open movebank", class = "btn-info")))
        
              ),
              # data download features
              fluidRow(
                column(6,
                  selectInput(ns("download_select"), label = "Select output", choices = c("All table" = "All","Individual table" = "Individual","Report" = "Report","KML" = "KML","Shapefile" = "Shapefile"),
                              selected = "All")),
                column(6,
                  div(style = "margin-top: 18%;",     
                  downloadButton(ns("downloadData"), "Download")))),
                  uiOutput(ns("ui_data_field_switch")),
                  uiOutput(ns("dynamic_data_fields"))
              ),
        mainPanel(width = 9,
          tabsetPanel(
            # tabPanel 1 - Leaflet map
            tabPanel(title = "Map",
                withSpinner(leafletOutput(ns("leafletMap"),width = "100%", height = "80vh"))),
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
    
    # create dataset and alert table as reactiveValues
    rv <- reactiveValues(data = data, table = get_alertTable(data))
    
    # observe button click
    observe({
      req(rv,input$individual_select)
      # Filter data
      rv$data <- rv$data |> filter(.data[[mt_track_id_column(rv$data)]] != input$individual_select)
      # now account for alert_toggle and status toggle
      if(isTRUE(input$alert_toggle) & isTRUE(input$status_toggle)){
        temp_alert_table <- get_alertTable(rv$data) |> group_by(.data[[mt_track_id_column(rv$data)]]) |> mutate(sumCounts = sum(count)) |> ungroup()
        temp_alert_table <- temp_alert_table |> as.data.frame() |> filter(sumCounts > 0)
        rv$table <- temp_alert_table |> dplyr::select(-sumCounts)
        rv$table <- rv$table |> slice(which(status == "active"))
      }else  
      if(isTRUE(input$alert_toggle) & isFALSE(input$status_toggle)){
        temp_alert_table <- get_alertTable(rv$data) |> group_by(.data[[mt_track_id_column(rv$data)]]) |> mutate(sumCounts = sum(count)) |> ungroup()
        temp_alert_table <- temp_alert_table |> as.data.frame() |> filter(sumCounts > 0)
        rv$table <- temp_alert_table |> dplyr::select(-sumCounts)
      }else
      if(isFALSE(input$alert_toggle) & isTRUE(input$status_toggle)){
        rv$table <- get_alertTable(rv$data)
        rv$table <- rv$table |> slice(which(status == "active"))
      }else
      if(isFALSE(input$alert_toggle) & isFALSE(input$status_toggle)){
        rv$table <- get_alertTable(rv$data)
      }
    }) %>% bindEvent(input$delete)
    
    observe({
      req(rv)
      # now account for alert_toggle and status toggle
      if(isTRUE(input$alert_toggle) & isTRUE(input$status_toggle)){
        temp_alert_table <- get_alertTable(rv$data) |> group_by(.data[[mt_track_id_column(rv$data)]]) |> mutate(sumCounts = sum(count)) |> ungroup()
        temp_alert_table <- temp_alert_table |> as.data.frame() |> filter(sumCounts > 0)
        rv$table <- temp_alert_table |> dplyr::select(-sumCounts)
        rv$table <- rv$table |> slice(which(status == "active"))
      }else  
      if(isTRUE(input$alert_toggle) & isFALSE(input$status_toggle)){
      temp_alert_table <- get_alertTable(rv$data) |> group_by(.data[[mt_track_id_column(rv$data)]]) |> mutate(sumCounts = sum(count)) |> ungroup()
      temp_alert_table <- temp_alert_table |> as.data.frame() |> filter(sumCounts > 0)
      rv$table <- temp_alert_table |> dplyr::select(-sumCounts)
      }else
      if(isFALSE(input$alert_toggle) & isTRUE(input$status_toggle)){
        rv$table <- get_alertTable(rv$data)
        rv$table <- rv$table |> slice(which(status == "active"))
      }else
      if(isFALSE(input$alert_toggle) & isFALSE(input$status_toggle)){
        rv$table <- get_alertTable(rv$data)
      }
    }) %>% bindEvent(input$alert_toggle,input$status_toggle)
    
    # create switch to filter individuals by only those with event alerts
    output$ui_data_alert_switch <- renderUI({
      input_switch(id = ns("alert_toggle"), label = "Filter by alerts", value = FALSE)
    })
    
    # create switch to filter individuals by only those with event alerts
    output$ui_data_status_switch <- renderUI({
      input_switch(id = ns("status_toggle"), label = "Filter by status", value = FALSE)
    })
  
    # build data table for DT with ID, tag_local_identifier, and number of notifications
    output$info_table <- DT::renderDT({
      req(rv)
      # summarize alert table into counts of events over individuals
      summary_table <- rv$table |> group_by(.data[[mt_track_id_column(rv$data)]]) |> summarize(nAlerts = sum(count))
      # now add in status
      summary_table <- merge(summary_table, unique(rv$table[,c(mt_track_id_column(rv$data),"status",mt_time_column(rv$data))]), by = mt_track_id_column(rv$data))
      # get ids, tag_ids and stauts into tibble
      tag_info <- mt_track_data(rv$data) |> dplyr::select(mt_track_id_column(rv$data),"tag_local_identifier")
      # merge tag_local_identifier in with summary_table
      summary_table_merged <- merge(summary_table, tag_info, by = mt_track_id_column(rv$data))
      # reorganize table
      summary_table_merged <- summary_table_merged[,c(1,5,3,2,4)]
      # update column names
      colnames(summary_table_merged)[c(1:2,5)] <- c("ind_id","device_id","recent_date_UTC")
      # set date as a character field
      summary_table_merged$recent_date_UTC <- as.character(summary_table_merged$recent_date_UTC)
      # create data table
      DT::datatable(as.data.frame(summary_table_merged), 
                    rownames = FALSE, 
                    selection = "single",
                    options = list(scrollY = "150px", 
                                   scrollX = TRUE,  
                                   paging = FALSE,
                                   lengthChange = FALSE,
                                   searching = FALSE,
                                   info = FALSE,
                                   pageLength = 5,
                                   columnDefs = list(
                                     list(className = 'dt-left', targets = "_all")))) |>
        formatStyle(columns = colnames(summary_table_merged), fontSize = '15px')
    }) 
    
    # select individual
    output$ui_select_individual <- renderUI({
      req(rv)
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
      updateSelectInput(session, inputId = "individual_select", selected = new_value)
    })
    
    # Create the proxy object for info table
    proxy_info_table <- DT::dataTableProxy("info_table")
    
    # Update the selected row when the dropdown changes
    observeEvent(input$individual_select, {
      req(rv)
      row_to_select <- which(unique(rv$table[,mt_track_id_column(rv$data)]) == input$individual_select)
      DT::selectRows(proxy_info_table, row_to_select)
    })
    
    # update selectInput for notification type depending on individual that is selected
    observeEvent(input$individual_select, {
      req(rv)
      update_choices = rv$table |> filter(rv$table[,mt_track_id_column(rv$data)] == input$individual_select &
                                                 count > 0) |> dplyr::select(notification_type) |> unique()
      # now update the selectInput
      updateSelectInput(session,
                        inputId = "notification_type",
                        choices = sort(update_choices$notification_type),
                        selected = sort(update_choices$notification_type)[1]
      )
    }) 
    
    # render dynmaic output for data filters
    output$dynamic_data_filter <- renderUI({
      req(rv$data)
      
      # individual mode calculations
      if (!is.null(input$individual_select)) {
        current_state <- saved_states$states_list[[input$individual_select]]
        
        track_data <- filter_track_data(rv$data, .track_id = input$individual_select)
        track_time <- mt_time(track_data)
        
        abs_min_date <- suppressWarnings(min(as.Date(track_time), na.rm = TRUE))
        abs_max_date <- suppressWarnings(max(as.Date(track_time), na.rm = TRUE))
        abs_max_rows <- nrow(track_data)
        
        saved_start <- if(!is.null(current_state$date_range)) current_state$date_range[1] else abs_min_date
        saved_end   <- if(!is.null(current_state$date_range)) current_state$date_range[2] else abs_max_date
        saved_value <- if(!is.null(current_state$number_locations)) current_state$number_locations else abs_max_rows
      }
      
      # global (map_all) mode calculations
      global_state <- saved_states$global_all_state
      
      abs_min_date_all <- suppressWarnings(min(as.Date(rv$data |> mt_time()), na.rm = TRUE))
      abs_max_date_all <- suppressWarnings(max(as.Date(rv$data |> mt_time()), na.rm = TRUE))
      abs_max_rows_all <- max(as.vector(table(mt_track_id(rv$data))), na.rm = TRUE)
      
      saved_start_all <- if(!is.null(global_state$date_range_all)) global_state$date_range_all[1] else abs_min_date_all
      saved_end_all   <- if(!is.null(global_state$date_range_all)) global_state$date_range_all[2] else abs_max_date_all
      saved_value_all <- if(!is.null(global_state$number_locations_all)) global_state$number_locations_all else abs_max_rows_all
      
      
      # conditional UI rendering
      if(input$map_all == FALSE && input$filter_toggle == "Date"){
        req(input$individual_select, as.character(abs_min_date) != "Inf" && as.character(abs_max_date) != "Inf")
        
        # Explicitly format dates to strings to suppress warnings
        dateRangeInput(inputId = ns("date_range"), label = "Select date range", 
                       start = format(as.Date(saved_start), "%Y-%m-%d"), 
                       end   = format(as.Date(saved_end), "%Y-%m-%d"),
                       min   = abs_min_date, 
                       max   = abs_max_date)
        
      } else if(input$map_all == FALSE && input$filter_toggle == "Number of locations"){
        req(input$individual_select, abs_max_rows >= 1)
        
        sliderInput(inputId = ns("number_locations"), label = "Select number of locations", 
                    min = 1, max = abs_max_rows, value = saved_value, step = 1)
        
      } else if(isTRUE(input$map_all) && input$filter_toggle == "Date"){
        req(as.character(abs_min_date_all) != "Inf" && as.character(abs_max_date_all) != "Inf")
        
        # split start and end parameter controls properly and formatted as standard character strings
        dateRangeInput(inputId = ns("date_range_all"), label = "Select date range", 
                       start = format(as.Date(saved_start_all), "%Y-%m-%d"), 
                       end   = format(as.Date(saved_end_all), "%Y-%m-%d"),
                       min   = abs_min_date_all, 
                       max   = abs_max_date_all) 
        
      } else if(isTRUE(input$map_all) && input$filter_toggle == "Number of locations"){
        req(abs_max_rows_all >= 1)
        
        sliderInput(inputId = ns("number_locations_all"), label = "Select number of locations per individual", 
                    min = 1, max = abs_max_rows_all, value = saved_value_all, step = 1)
      }
    })
    
    outputOptions(output, "dynamic_data_filter", suspendWhenHidden = FALSE)
    
    # initialize the historical state storage (added global_all_state)
    saved_states <- reactiveValues(
      states_list = list(),
      global_all_state = list(date_range_all = NULL, number_locations_all = NULL)
    )
    
    # Populate saved_states with default values when data loads
    observe({
      req(rv$table, rv$data)
      unique_ids <- unique(rv$table[, mt_track_id_column(rv$data)])
      
      isolate({
        # individual initializations
        for(i in seq_along(unique_ids)){
          if (is.null(saved_states$states_list[[unique_ids[i]]])) {
            track_data <- filter_track_data(rv$data, .track_id = unique_ids[i])
            track_time <- mt_time(track_data)
            
            saved_states$states_list[[unique_ids[i]]] <- list(
              date_range = c(suppressWarnings(min(as.Date(track_time), na.rm = TRUE)), 
                             suppressWarnings(max(as.Date(track_time), na.rm = TRUE))),
              number_locations = nrow(track_data)
            )
          }
        }
        
        # global initializations (map all mode)
        if (is.null(saved_states$global_all_state$date_range_all)) {
          saved_states$global_all_state <- list(
            date_range_all = c(suppressWarnings(min(as.Date(rv$data |> mt_time()), na.rm = TRUE)), 
                               suppressWarnings(max(as.Date(rv$data |> mt_time()), na.rm = TRUE))),
            number_locations_all = max(as.vector(table(mt_track_id(rv$data))), na.rm = TRUE)
          )
        }
      })
    })
    
    #sSave state when any visible input is manipulated
    observeEvent(list(input$date_range, input$number_locations, 
                      input$date_range_all, input$number_locations_all), {
        
        # map all mode
        if (isTRUE(input$map_all)) {
          current_global <- saved_states$global_all_state
          new_date_all <- current_global$date_range_all
          new_num_all  <- current_global$number_locations_all
          
          if (input$filter_toggle == "Date" && !is.null(input$date_range_all)) {
            new_date_all <- input$date_range_all
          }
          if (input$filter_toggle == "Number of locations" && !is.null(input$number_locations_all)) {
            new_num_all <- input$number_locations_all
          }
          
          saved_states$global_all_state <- list(
            date_range_all = new_date_all,
            number_locations_all = new_num_all
          )
          
          # individual select mode
        } else {
          req(input$individual_select)
          current_state <- saved_states$states_list[[input$individual_select]]
          new_date_range <- current_state$date_range
          new_number_locations <- current_state$number_locations
          
          if (input$filter_toggle == "Date" && !is.null(input$date_range)) {
            new_date_range <- input$date_range
          }
          if (input$filter_toggle == "Number of locations" && !is.null(input$number_locations)) {
            new_number_locations <- input$number_locations
          }
          
          saved_states$states_list[[input$individual_select]] <- list(
            date_range = new_date_range,
            number_locations = new_number_locations
          )
        }
      }, ignoreInit = TRUE)
    
    # data filter for overall individual
    data_individual <- reactive({
      req(rv$data,input$individual_select)
      filtered_data <- filter_track_data(rv$data, .track_id = input$individual_select)
      req(nrow(filtered_data) > 0)
      if(input$filter_toggle == "Date"){
        req(length(input$date_range)>1)
        filtered_data2 <- filtered_data |> filter(between(as.Date(mt_time(filtered_data)), 
                                                          as.Date(input$date_range[1]),as.Date(input$date_range[2])))
      }else
      if(input$filter_toggle == "Number of locations"){
        req(isFALSE(is.null(input$number_locations)))
        filtered_data2 <- filtered_data |> slice_tail(n=input$number_locations)
      }
      # add unique id to data for clicking on Leaflet map
      filtered_data2$unique_id <- 1:nrow(filtered_data2)
      # add label for Leaflet map
      # store labels
      filtered_data2$label <- paste("<div style='font-size: 14px;'>",
                                    "<b>Ind_id:</b>",mt_track_id(filtered_data2),
                                    "</br><b>Device_id:</b>",mt_track_data(filtered_data2)$tag_local_identifier,
                                    "</br><b>Timestamp_UTC:</b>",as.character(mt_time(filtered_data2)),
                                    "</br><b>Lon:</b>",st_coordinates(filtered_data2)[,1],
                                    "</br><b>Lat:</b>",st_coordinates(filtered_data2)[,2],
                                    "</br><b>Status:</b>",ifelse(filtered_data2$mortality==1,"dead","alive"),
                                    "</div>") %>% lapply(htmltools::HTML)
      # add lat/lon for leaflet popups
      filtered_data2$lng <- st_coordinates(filtered_data2)[,1]
      filtered_data2$lat <- st_coordinates(filtered_data2)[,2]
      # return data
      return(filtered_data2)
    }) 
    
    # data filter for all individuals
    data_all <- reactive({
      req(rv$data)
      if(input$filter_toggle == "Date"){
        req(length(input$date_range_all)>1)
        filtered_data_all <- rv$data |> filter(between(as.Date(mt_time(rv$data)), 
                                                          as.Date(input$date_range_all[1]),as.Date(input$date_range_all[2])))
      }else
      if(input$filter_toggle == "Number of locations"){
        req(isFALSE(is.null(input$number_locations_all)))
        # need to keep same number of locations per individual as they are reduced
        filtered_data_all <- rv$data |>
          group_by(.data[[mt_track_id_column(rv$data)]]) |>
          slice_tail(n = input$number_locations_all)
      }
      # add unique id to data for clicking on Leaflet map
      filtered_data_all$unique_id <- 1:nrow(filtered_data_all)
      # Format Device.ID 
      n_times <- as.vector(table(as.character(mt_track_id(filtered_data_all))))
      Device.ID <- rep(as.character(mt_track_data(filtered_data_all)$tag_local_identifier), times = n_times)
      # store labels
      filtered_data_all$label <- paste("<div style='font-size: 14px;'>",
                                    "<b>Ind_id:</b>",mt_track_id(filtered_data_all),
                                    "</br><b>Device_id:</b>",Device.ID,
                                    "</br><b>Timestamp_UTC:</b>",as.character(mt_time(filtered_data_all)),
                                    "</br><b>Lon:</b>",st_coordinates(filtered_data_all)[,1],
                                    "</br><b>Lat:</b>",st_coordinates(filtered_data_all)[,2],
                                    "</br><b>Status:</b>",ifelse(filtered_data_all$mortality==1,"dead","alive"),
                                    "</div>") %>% lapply(htmltools::HTML)
      # add lat/lon for leaflet popups
      filtered_data_all$lng <- st_coordinates(filtered_data_all)[,1]
      filtered_data_all$lat <- st_coordinates(filtered_data_all)[,2]
      # return data
      return(filtered_data_all)
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
      selectInput(ns("notification_type"), label = "Select alert", 
                  choices = sort(unique(rv$table$notification_type)),
                  selected = sort(unique(rv$table$notification_type))[1])
    })
  
    # make field names a reactive value
    field_columns <- reactive({
      req(data_individual(),input$notification_type,rv$data)
      available_colnames <- colnames(rv$data)[1:(ncol(rv$data)-1)]
      alerts <- c("mortality","resurrection","cluster","nsd","voltage","gps_accuracy","gps_transmission","gps_resurrection","tag_release")
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
                                                         "mortality_status","distMoved"))
        # reorganize data fields
        available_colnames <- c(mt_track_id_column(rv$data),"device_id","Latitude","Longitude",mt_time_column(rv$data),
                                input$notification_type,"mortality_status","distMoved","n_locs",available_colnames[-field_indices])
        # set fields to show
        selected_colnames <- c(mt_track_id_column(rv$data),"device_id","Latitude","Longitude",mt_time_column(rv$data),
                               input$notification_type,"mortality_status","distMoved")
      }else
      if(isFALSE("mortality_status" %in% available_colnames)){
        # field indices to reorganize
        field_indices <- which(available_colnames %in% c(mt_track_id_column(rv$data),"device_id",
                                                         mt_time_column(rv$data),input$notification_type,"distMoved"))
        # reorganize data fields
        available_colnames <- c(mt_track_id_column(rv$data),"device_id","Latitude","Longitude",mt_time_column(rv$data),
                               input$notification_type,"distMoved","n_locs",available_colnames[-field_indices])
        # set fields to show
        selected_colnames <- c(mt_track_id_column(rv$data),"device_id","Latitude","Longitude",mt_time_column(rv$data),
                               input$notification_type,"distMoved")
      }  
      # remove notification type if there are none present
      if(sum(as.data.frame(data_individual())[,input$notification_type])==0){
        available_colnames <- available_colnames[-which(available_colnames == input$notification_type)]
        selected_colnames <- selected_colnames[-which(selected_colnames == input$notification_type)]
      }
      # remove alias and value field
      if(any(available_colnames %in% c("mortality_alias","mortality_value"))){
        available_colnames <- available_colnames[-which(available_colnames %in% c("mortality_alias","mortality_value"))]
      }
      if(any(available_colnames %in% c("voltage_alias","voltage_value"))){
        available_colnames <- available_colnames[-which(available_colnames %in% c("voltage_alias","voltage_value"))]
      }
      if(any(available_colnames %in% c("gps_accuracy_alias","gps_accuracy_value","gps_accuracy_prop"))){
        available_colnames <- available_colnames[-which(available_colnames %in% c("gps_accuracy_alias","gps_accuracy_value","gps_accuracy_prop"))]
      }
      return(list(selected_colnames = selected_colnames, available_colnames = available_colnames))
    }) 
        
  # data toggle switch to show data fields or not
  output$ui_data_field_switch <- renderUI({
    input_switch(id = ns("data_toggle"), label = "Show data fields", value = TRUE)
  })
  
  # create a reactiveValues object to store selections for data_fields
  field_vals <- reactiveValues(checked = NULL)
  
  # save the checkbox state whenever the user changes it
  observeEvent(input$data_fields, {
    if(isFALSE(input$notification_type %in% input$data_fields)){
      field_vals$checked <- c(input$data_fields,input$notification_type)
    }else
      if(input$notification_type %in% input$data_fields){
        field_vals$checked <- input$data_fields  
      }
  })
  
  output$dynamic_data_fields <- renderUI({
    req(field_columns()$available_colnames,field_columns()$selected_colnames,input$data_toggle)
    # 3. Render the UI
    if(isTRUE(input$data_toggle)){
    checkboxGroupInput(
      inputId = ns("data_fields"), 
      label = "Select data fields",
      choices = field_columns()$available_colnames, 
      selected = field_columns()$selected_colnames
    )
    }else
    NULL
  })
  
  # update values checked when data_fields input
  observe({
    req(input$data_fields)
   # check if reactive field_vals are in the available choices
   valid_selected <- intersect(field_vals$checked, field_columns()$available_colnames)
    # Update the data_fields element
    updateCheckboxGroupInput(
      session, 
      inputId = "data_fields", 
      choices = field_columns()$available_colnames,
      selected = valid_selected
    )
    # Update the stored reactive state with the valid selection
    field_vals$checked <- valid_selected
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
  output$all_table <- DT::renderDT({
    # return data table
    DT::datatable(all_table_data(), 
                  rownames = FALSE, 
                  selection = "single", 
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
    updateSelectInput(session, inputId = "individual_select", selected = new_value)
  })
  
  # Create the proxy object for all data table
  proxy_all_table <- DT::dataTableProxy("all_table")
  
  # Update the selected row when the dropdown changes
  observeEvent(input$individual_select, {
    req(rv)
    row_to_select <- which(unique(rv$table[,mt_track_id_column(rv$data)]) == input$individual_select)
    DT::selectRows(proxy_all_table, row_to_select)
  })

  # make reactive output for ind table and downloading features
  ind_table_data <- reactive({
    req(data_individual(),rv$data,input$individual_select,nrow(data_individual())>0,input$data_fields)
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
    output$ind_table <- DT::renderDT({
      req(all(colnames(ind_table_data()) %in% input$data_fields),nrow(ind_table_data())>0)
      # render data table
      DT::datatable(ind_table_data(), 
                    rownames = FALSE, 
                    selection = "single", 
                    options = list(scrollY = "600px", 
                                   scrollX = TRUE, 
                                   paging = FALSE,
                                   lengthChange = FALSE,
                                   info = FALSE,
                                   fixedHeader=TRUE)) %>%
        formatRound(columns = c('Latitude', 'Longitude'), digits = 6)
    }, server = TRUE) # tried server = FALSE but input$notification_type wasn't updating
  
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
    req(input$basemap_type)
    if(input$basemap_type == "World Imagery"){
      return("yellow")
    }else
    if(input$basemap_type != "World Imagery"){
      return("black")
    }
  })
  
# create leaflet map for plotting with choice of basemap
leaf_map <- reactive({
    req(nrow(data_individual()) > 0,input$notification_type)
    # map data that has alerts
    if(nrow(data_individual() |> filter(.data[[input$notification_type]] == 1)) > 0){
      # now plot leatlet map
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
              # add locations to denote start and end of track
              addCircleMarkers(data = data_individual() |> slice(c(1, n())),
                               layerId = ~unique_id,
                               label = ~label,
                               opacity = 1,
                               fillOpacity = 1,
                               color = c("green","red"),
                               fillColor = c("green","red")) %>%
              # add all data points
              addCircles(data = data_individual(),
                         layerId = ~unique_id,
                         opacity = 0.3,
                         fillOpacity = 0.8,
                         label = ~label,
                         radius = 10, 
                         color = "blue",
                         fillColor = "blue") %>%
              # add alert data points
              addCircles(data = data_individual() |> filter(.data[[input$notification_type]] == 1),
                         layerId = ~unique_id,
                         opacity = 0.7,
                         fillOpacity = 0.85,
                         label = ~label,
                         radius = 10, 
                         color = "#FF991C",
                         fillColor = "#FF991C") %>%
              # add locations to denote start and end of track
              addCircles(data = data_individual() |> slice(c(1, n())),
                               label = ~label,
                               layerId = ~unique_id,
                               opacity = 0,
                               fillOpacity = 0,
                               radius = 10,
                               color = c("green","red"),
                               fillColor = c("green","red"))
    }else
      if(nrow(data_individual() |> filter(.data[[input$notification_type]] == 1)) == 0){ 
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
                # add locations to denote start and end of track
                addCircleMarkers(data = data_individual() |> slice(c(1, n())),
                           layerId = ~unique_id,
                           label = ~label,
                           opacity = 1,
                           fillOpacity = 1,
                           color = c("green","red"),
                           fillColor = c("green","red")) %>%
                # add all data points
                addCircles(data = data_individual(),
                           layerId = ~unique_id,
                           opacity = 0.3,
                           label = ~label,
                           fillOpacity = 0.8,
                           radius = 10, 
                           color = "blue",
                           fillColor = "blue") %>%
                # add locations to denote start and end of track
                addCircles(data = data_individual() |> slice(c(1, n())),
                                 layerId = ~unique_id,
                                 label = ~label,
                                 opacity = 0,
                                 fillOpacity = 0,
                                 radius = 10,
                                 color = c("green","red"),
                                 fillColor = c("green","red"))
    }
    return(map1)
})

# Reactive value to store the clicked circle ID
selected_circle <- reactiveVal(NULL)

# handle clicks: pin or unpin a label when a circle is clicked
observeEvent(input$leafletMap_shape_click, {
  clicked_id <- input$leafletMap_shape_click$id
  
  # if the user clicks the same circle again, un-pin it
  if(!is.null(selected_circle()) && selected_circle() == clicked_id){
    selected_circle(NULL)
    leafletProxy("leafletMap") %>% clearPopups()
  }else 
  if(isFALSE(!is.null(selected_circle()) && selected_circle() == clicked_id)){
    # Pin the newly clicked circle
    selected_circle(clicked_id)
    clicked_point <- subset(data_individual(), unique_id == clicked_id)
    
    leafletProxy("leafletMap") %>%
      clearPopups() %>%
      addPopups(
        lat = clicked_point$lat, 
        lng = clicked_point$lng, 
        popup = clicked_point$label,
        options = popupOptions(closeOnClick = FALSE, 
                               autoClose = FALSE,
                               closeButton = TRUE))
  }
})

# 2. Handle Hovers safely (Guards against NA/missing values)
observeEvent(input$leafletMap_shape_mouseover, {
  hover_id <- input$leafletMap_shape_mouseover$id
  
  # Safety Check 1: If mouse is not over a valid shape ID, exit early
  if(is.null(hover_id) || is.na(hover_id)) return() 
  
  current_selected <- selected_circle()
  
  # Safety Check 2: If a circle is currently pinned, evaluate safely using isTRUE()
  if(!is.null(current_selected)) {
    # isTRUE handles NA values safely if current_selected or hover_id are corrupted
    if(isTRUE(current_selected != hover_id)){
      selected_circle(NULL) # Reset state
      leafletProxy("leafletMap") %>% clearPopups()
    }
  }
})
      
# create leaflet map for plotting with choice of basemap
leaf_map_all <- reactive({
        req(nrow(data_all()) > 0)
         temp_data <- data_all()
         pal <- colorFactor(
          palette = "viridis", # explicitly generate a number of colors
          domain = mt_track_id(temp_data),
          na.color = "transparent")
         map2 <- leaflet() %>% 
              # add scale bar
              addScaleBar(position = "bottomleft", 
                          options = scaleBarOptions(maxWidth = 200, metric = TRUE, imperial= FALSE)) %>%   
              # add user-selected basemap
              addProviderTiles(basemap()) %>% 
              # add track lines for all individuals
              addPolylines(data = mt_track_lines(temp_data),
                           weight = 2,
                           color = linesColor(),
                           opacity = 0.8) %>%
              # add all data points
               addCircles(data = temp_data,
                         opacity = 0.3,
                         layerId = ~unique_id,
                         label = ~label,
                         fillOpacity = 0.8,
                         radius = 10, 
                         color = ~pal(mt_track_id(temp_data)),
                         fillColor = ~pal(mt_track_id(temp_data)),
                         labelOptions = labelOptions(
                           style = list(
                             "font-size" = "14px"))) %>%
              # add legend
                addLegend(
                  position = "topright", # Position the legend
                  pal = pal,                # The color palette function
                  values = mt_track_id(temp_data), # The values to use for the legend
                  title = "ind_id",      # Legend title
                  opacity = 0.8               # Opacity of the legend colors
                )
    return(map2)
  })    

# render leaflet map depending on map_all input
output$leafletMap <- renderLeaflet({
   if(isFALSE(input$map_all)){
     leaf_map()
   }else
   if(isTRUE(input$map_all)){
      leaf_map_all()
   }
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
  # store movebank link
  collar_link <- paste0("window.open('","https://www.movebank.org/cms/webapp?gwt_fragment=page=studies,path=study",movebank_studyID$study_id,"+deployment","')")
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
  # reset levels depending on if any indicators are zero
  if(any(plot_data$mortality == 0)){
    plot_data$mortality_status <- relevel(plot_data$mortality_status, ref = "Nothing detected")
  }
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
if(input$notification_type == "resurrection" & unique(data_individual()$nAlerts) > 0){
  # make time series plot that adjusts based on notification type
  plot_data <- data_individual() |> as.data.frame() |> dplyr::select(mt_time_column(data_individual()),input$notification_type) 
  # rename first column to timestamp
  colnames(plot_data)[1] = "timestamp"
  # create resurrection status as a factor based on event indicator variable
  plot_data$resurrection_status <- character(nrow(plot_data))
  plot_data$resurrection_status[which(plot_data$resurrection == 1)] <- "Resurrection"
  plot_data$resurrection_status[which(plot_data$resurrection == 0)] <- "No Ressurection"
  # now convert to a factor
  plot_data$resurrection_status <- as.factor(plot_data$resurrection_status)
  # reset levels depending on if any indicators are zero
  if(any(plot_data$resurrection == 0)){
    plot_data$resurrection_status <- relevel(plot_data$resurrection_status, ref = "No Ressurection")
  }
  gg2 <- ggplot(plot_data, aes(x = timestamp, y = resurrection, group = resurrection_status,
                               color = resurrection_status,
                               text = paste("</br>Date:",timestamp,
                                            "</br>Status:",resurrection_status))) + 
    geom_point() + scale_y_continuous(breaks = c(0,1)) + scale_color_manual(values = plot_color) +
    xlab("Date") + ylab("Resurrection indicator") + theme_bw() +
    theme(axis.text.x = element_text(size = 16, angle = 45, hjust = 1, vjust = 1),
          axis.text.y = element_text(size = 16),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 18)) +
    guides(color=guide_legend(title="Resurrection status")) 
  ggplotly(gg2, tooltip = c("text")) %>%
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
  # reset levels depending on if any indicators are zero
  if(any(plot_data$cluster == 0)){
    plot_data$cluster_status <- relevel(plot_data$cluster_status, ref = "Not in cluster")
  }
  gg3 <- ggplot(plot_data, aes(x = timestamp, y = cluster, group = cluster_status,
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
  ggplotly(gg3, tooltip = c("text")) %>%
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
    # reset levels depending on if any indicators are zero
    if(any(plot_data$nsd == 0)){
      plot_data$nsd_status <- relevel(plot_data$nsd_status, ref = "Not above max NSD")
    }
    gg4 <- ggplot(plot_data, aes(x = timestamp, y = nsd, group = nsd_status,
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
  ggplotly(gg4, tooltip = c("text")) %>%
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
    # reset levels depending on if any indicators are zero
    if(any(plot_data$voltage == 0)){
      plot_data$voltage_status <- relevel(plot_data$voltage_status, ref = "Above threshold")
    }
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
    gg5 <- ggplot(plot_data, aes(x = timestamp, y = as.numeric(tag_voltage), group = voltage_status,
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
  ggplotly(gg5, tooltip = c("text")) %>%
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
    # reset levels depending on if any indicators are zero
    if(any(plot_data$gps_accuracy == 0)){
      plot_data$gps_accuracy_status <- relevel(plot_data$gps_accuracy_status, ref = "2D GPS Fix or failed")
    }
    gg6 <- ggplot(plot_data, aes(x = timestamp, y = gps_accuracy, group = gps_accuracy_status,
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
    ggplotly(gg6, tooltip = c("text")) %>%
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
      # reset levels depending on if any indicators are zero
      if(any(plot_data$gps_transmission == 0)){
        plot_data$gps_transmission_status <- relevel(plot_data$gps_transmission_status, ref = "Normal transmission")
      }
      gg7 <- ggplot(plot_data, aes(x = timestamp, y = gps_transmission, group = gps_transmission_status,
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
    ggplotly(gg7, tooltip = c("text")) %>%
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
      # reset levels depending on if any indicators are zero
      if(any(plot_data$gps_resurrection == 0)){
        plot_data$gps_resurrection_status <- relevel(plot_data$gps_resurrection_status, ref = "Normal transmission")
      }
      gg8 <- ggplot(plot_data, aes(x = timestamp, y = gps_resurrection, group = gps_resurrection_status,
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
      ggplotly(gg8, tooltip = c("text")) %>%
        layout(legend = list(orientation = "h", x = 0.5, y = 1.05, xanchor = "center", yanchor = "bottom"))  
    }else
    if(input$notification_type == "tag_release" & unique(data_individual()$nAlerts) > 0){
      # make time series plot that adjusts based on notification type
      plot_data <- data_individual() |> as.data.frame() |> dplyr::select(mt_time_column(data_individual()),input$notification_type) 
      # rename first column to timestamp
      colnames(plot_data)[1] = "timestamp"
      # create tag_release as a factor on event indicator variable
      plot_data$tag_release_status <- character(nrow(plot_data))
      plot_data$tag_release_status[which(plot_data$tag_release == 1)] <- "Release detected"
      plot_data$tag_release_status[which(plot_data$tag_release == 0)] <- "No release"
      # now convert to a factor
      plot_data$tag_release_status <- as.factor(plot_data$tag_release_status)
      # reset levels depending on if any indicators are zero
      if(any(plot_data$tag_release == 0)){
        plot_data$tag_release_status <- relevel(plot_data$tag_release_status, ref = "No release")
      }
      gg9 <- ggplot(plot_data, aes(x = timestamp, y = tag_release, group = tag_release_status,
                                   color = tag_release_status,
                                   text = paste("</br>Date:",timestamp,
                                                "</br>Status:",tag_release_status))) + 
        geom_point() + scale_y_continuous(breaks = c(0,1)) + scale_color_manual(values = plot_color) +
        xlab("Date") + ylab("Tag release indicator") + theme_bw() +
        theme(axis.text.x = element_text(size = 16, angle = 45, hjust = 1, vjust = 1),
              axis.text.y = element_text(size = 16),
              axis.title.x = element_text(size = 20),
              axis.title.y = element_text(size = 20),
              legend.text = element_text(size = 14),
              legend.title = element_text(size = 18)) +
        guides(color=guide_legend(title="Tag release status")) 
      ggplotly(gg9, tooltip = c("text")) %>%
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
        paste("data-individual_",input$individual_select,"_", Sys.Date(), ".csv", sep = "")
      }else
      if(input$download_select == "Report"){
        paste("collar-health-report_",input$individual_select,"_", Sys.Date(), ".html", sep = "") 
      }else
      if(input$download_select == "Shapefile"){
        if(isFALSE(input$map_all)){
          paste("data-individual_",input$individual_select,"_",Sys.Date(),".zip", sep = "")
        }else
        if(isTRUE(input$map_all)){
          paste("data-all_", Sys.Date(), ".zip", sep = "")
        }
      }else
      if(input$download_select == "KML"){
        if(isFALSE(input$map_all)){
          paste("data-individual_",input$individual_select,"_",Sys.Date(),".kml", sep = "")
        }else
        if(isTRUE(input$map_all)){
          paste("data-all_", Sys.Date(), ".kml", sep = "")
        }  
      }
    },
    content = function(file){
      if(input$download_select == "All"){
        # store all data table as temporary
        temp_all_data <- all_table_data()
        # change field name of timestamp to timestamp_UTC
        colnames(temp_all_data)[which(colnames(temp_all_data) == "timestamp")] <- "timestamp_UTC"
        # now strip UTC from the timestamp field
        temp_all_data$timestamp_UTC <- substr(temp_all_data$timestamp_UTC,1,19)
        # write to csv file
        write.csv(temp_all_data, file, row.names = FALSE)
      }else
      if(input$download_select == "Individual"){
        # store all data table as temporary
        temp_ind_data <- ind_table_data()
        # change field name of timestamp to timestamp_UTC
        colnames(temp_ind_data)[which(colnames(temp_ind_data) == "timestamp")] <- "timestamp_UTC"
        # now strip UTC from the timestamp field
        temp_ind_data$timestamp_UTC <- substr(temp_ind_data$timestamp_UTC,1,19)
        # write to csv file
        write.csv(temp_ind_data, file, row.names = FALSE)  
      }else
      if(input$download_select == "Shapefile"){
        # create a temporary directory 
        dir.create(targetDirFiles_shp <- tempdir())
        # Convert the move2 object into a standard sf data frame
      if(isFALSE(input$map_all)){
        sf_obj <- data_individual()
        my_sf <- mt_as_event_attribute(sf_obj, names(mt_track_data(sf_obj)))
        class(sf_obj) <- class(sf_obj) %>% setdiff("move2")
        # 2. Identify and convert list columns into character strings
        # (Excludes the 'geometry' column automatically)
        sf_obj <- sf_obj %>%
          mutate(across(
            where(~ is.list(.x) && !inherits(.x, "sfc")), 
            ~ map_chr(.x, ~ paste(as.character(.x), collapse = ", "))
          ))
        # write to shapefile
        suppressWarnings(st_write(
          obj = sf_obj, 
          dsn = targetDirFiles_shp, 
          layer = paste0("data-individual_",input$individual_select,"_",Sys.Date()), 
          driver = "ESRI Shapefile", 
          delete_layer = TRUE,
          quiet = TRUE
        ))
        # save zipped files
        zip::zip(zipfile = file, files = list.files(targetDirFiles_shp, pattern = "data-individual", full.names = TRUE),mode = "cherry-pick")
      }else
      if(isTRUE(input$map_all)){
          sf_obj <- data_all()
          my_sf <- mt_as_event_attribute(sf_obj, names(mt_track_data(sf_obj)))
          class(sf_obj) <- class(sf_obj) %>% setdiff("move2")
          # 2. Identify and convert list columns into character strings
          # (Excludes the 'geometry' column automatically)
          sf_obj <- sf_obj %>%
            mutate(across(
              where(~ is.list(.x) && !inherits(.x, "sfc")), 
              ~ map_chr(.x, ~ paste(as.character(.x), collapse = ", "))
            ))
          # write to shapefile
          suppressWarnings(st_write(
            obj = sf_obj, 
            dsn = targetDirFiles_shp, 
            layer = paste0("data-all_","_",Sys.Date()), 
            driver = "ESRI Shapefile", 
            delete_layer = TRUE,
            quiet = TRUE
          ))
          # save zipped files
          zip::zip(zipfile = file, files = list.files(targetDirFiles_shp, pattern = "data-all", full.names = TRUE),mode = "cherry-pick")
        }
    }else
    if(input$download_select == "KML"){
      if(isFALSE(input$map_all)){
      # helper function to sanitize text fields for Google Maps
      clean_xml_text <- function(text) {
        if (is.null(text) || is.na(text)) return("")
        text <- gsub("&", "&amp;", text)
        text <- gsub("<", "&lt;", text)
        text <- gsub(">", "&gt;", text)
        return(text)
      }
      kml_body <- ""
        # Subset move2 object for this specific animal
        ind_data <- data_individual()
        description = as.character(mt_track_data(ind_data )$name[1])
        # Extract spatial coordinates matrix and native timestamps
        coords_mat <- st_coordinates(ind_data)
        timestamps <- mt_time(ind_data)
        device.id <- rep(as.character(mt_track_data(ind_data)$tag_local_identifier), nrow(ind_data))
        clean_ind_name <- clean_xml_text(as.character(unique(mt_track_id(ind_data))))
        # Start a folder container for the individual
        kml_body <- paste0(kml_body, '    <Folder>\n', '      <name>', clean_ind_name, '</name>\n')
        # Loop over every GPS point row for this animal
        for (i in 1:nrow(ind_data)){
          # Extract metadata fields for the current row
          current_time  <- clean_xml_text(format(timestamps[i], "%Y-%m-%d %H:%M:%S UTC"))
          Lon <- coords_mat[i, "X"]
          Lat <- coords_mat[i, "Y"]
          # Extract the custom extended attributes
          Device.id = clean_xml_text(device.id[i])
          Status = clean_xml_text(ifelse(ind_data$mortality[i]==1,"dead","alive"))
          # Construct an individual Placemark point layer
          kml_body <- paste0(
            kml_body,
            '      <Placemark>\n',
            '        <name>', clean_ind_name, ' (Point ', i, ')</name>\n',
            '        <description>', description, '</description>\n',
            
            # Inject Extended Data Schema Table
            '        <ExtendedData>\n',
            '          <Data name="Device.id">\n',
            '            <value>', Device.id, '</value>\n',
            '          </Data>\n',
            '          <Data name="Timestamp">\n',
            '            <value>', current_time, '</value>\n',
            '          </Data>\n',
            '          <Data name="Lat">\n',
            '            <value>', Lat, '</value>\n',
            '          </Data>\n',
            '          <Data name="Lon">\n',
            '            <value>', Lon, '</value>\n',
            '          </Data>\n',
            '          <Data name="Status">\n',
            '            <value>', Status, '</value>\n',
            '          </Data>\n',
            '        </ExtendedData>\n',
            
            # Spatial Geometry Node
            '        <Point>\n',
            '          <coordinates>', Lon, ',', Lat, ',0</coordinates>\n',
            '        </Point>\n',
            '      </Placemark>\n'
          )
        }
        # Close individual folder block
        kml_body <- paste0(kml_body, '    </Folder>\n')
        # Assemble and save clean file
        full_kml_string <- paste0(
        '<?xml version="1.0" encoding="UTF-8"?>\n',
        '<kml xmlns="http://opengis.net">\n',
        '  <Document>\n',
        '    <name>Move2 Extended Points Export</name>\n',
        kml_body,
        '  </Document>\n',
        '</kml>'
        )
        # Render and enforce XML compliance 
        final_doc <- read_xml(full_kml_string)
        write_xml(final_doc, file)
      }else
      if(isTRUE(input$map_all)){
        # helper function to sanitize text fields for Google Maps
        clean_xml_text <- function(text) {
          if (is.null(text) || is.na(text)) return("")
          text <- gsub("&", "&amp;", text)
          text <- gsub("<", "&lt;", text)
          text <- gsub(">", "&gt;", text)
          return(text)
        }
        # extract individuals tracks
        individual_ids <- unique(mt_track_id(data_all()))
        kml_body <- ""
        # Loop over individuals and points
        for(k in 1:length(individual_ids)){
          # Subset move2 object for this specific animal
          ind_data <- data_all()[mt_track_id(data_all()) == individual_ids[k], ]
          description = as.character(mt_track_data(ind_data)$name[1])
          # Extract spatial coordinates matrix and native timestamps
          coords_mat <- st_coordinates(ind_data)
          timestamps <- mt_time(ind_data)
          device.id <- rep(as.character(mt_track_data(ind_data)$tag_local_identifier), nrow(ind_data))
          clean_ind_name <- clean_xml_text(as.character(individual_ids[k]))
          # Start a folder container for the individual
          kml_body <- paste0(kml_body, '    <Folder>\n', '      <name>', clean_ind_name, '</name>\n')
          # Loop over every GPS point row for this animal
          for (i in 1:nrow(ind_data)) {
            # Extract metadata fields for the current row
            current_time  <- clean_xml_text(format(timestamps[i], "%Y-%m-%d %H:%M:%S UTC"))
            Lon <- coords_mat[i, "X"]
            Lat <- coords_mat[i, "Y"]
            # Extract the custom extended attributes
            Device.id = clean_xml_text(device.id[i])
            Status = clean_xml_text(ifelse(ind_data$mortality[i]==1,"dead","alive"))
            # Construct an individual Placemark point layer
            kml_body <- paste0(
              kml_body,
              '      <Placemark>\n',
              '        <name>', clean_ind_name, ' (Point ', i, ')</name>\n',
              '        <description>', description, '</description>\n',
              
              # Inject Extended Data Schema Table
              '        <ExtendedData>\n',
              '          <Data name="Device.id">\n',
              '            <value>', Device.id, '</value>\n',
              '          </Data>\n',
              '          <Data name="Timestamp">\n',
              '            <value>', current_time, '</value>\n',
              '          </Data>\n',
              '          <Data name="Lat">\n',
              '            <value>', Lat, '</value>\n',
              '          </Data>\n',
              '          <Data name="Lon">\n',
              '            <value>', Lon, '</value>\n',
              '          </Data>\n',
              '          <Data name="Status">\n',
              '            <value>', Status, '</value>\n',
              '          </Data>\n',
              '        </ExtendedData>\n',
              
              # Spatial Geometry Node
              '        <Point>\n',
              '          <coordinates>', Lon, ',', Lat, ',0</coordinates>\n',
              '        </Point>\n',
              '      </Placemark>\n'
            )
          }
          
          # Close individual folder block
          kml_body <- paste0(kml_body, '    </Folder>\n')
        }
        
        # --- 4. ASSEMBLE ARCHITECTURE & SAVE CLEAN FILE ---
        full_kml_string <- paste0(
          '<?xml version="1.0" encoding="UTF-8"?>\n',
          '<kml xmlns="http://opengis.net">\n',
          '  <Document>\n',
          '    <name>Move2 Extended Points Export</name>\n',
          kml_body,
          '  </Document>\n',
          '</kml>'
        )
        # Render and enforce XML compliance 
        final_doc <- read_xml(full_kml_string)
        write_xml(final_doc, file)
      }
    }else  
    if(input$download_select == "Report"){
      # Parameters to pass to the Rmd
      params <- list(data = rv$data, 
                     table = rv$table, 
                     widget_data = user_map(), 
                     individual_select = input$individual_select,
                     individual_table = ind_table_data())
      # Knit the document to the specified output file location
      rmarkdown::render(
        input = getAuxiliaryFilePath("auxiliary-file-a"),
        output_format = "html_document",
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  }
  )
  # end of server
  
  # data must be returned. Either the unmodified input data, or the modified data by the app
  return(reactive({rv$data}))
}
