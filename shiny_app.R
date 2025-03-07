#setwd("D:/Dokumente/Studium/9 FS/Analysis of spatial temporal data/Projekt")

library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(leaflet)
library(shiny)
library(shinyWidgets)
library(plotly)
library(httr)
library(tidyr)
library(prophet)
library(shinycssloaders)
library(stats)
library(forecast)

# define loader
options(page.spinner.type = 1)
options(page.spinner.caption = "Calculating...")
options(page.spinner.color = "black")

# Checks wether a url is reachable
url_exists <- function(url) {
  response <- HEAD(url)
  return(status_code(response) == 200)
}

# Fetches the CSV files from Open Data Portal Münster
getCSV <- function(start_timestamp, end_timestamp, station) {
  showPageSpinner()
  year_a <- substr(start_timestamp, 1, 4)
  month_a <- substr(start_timestamp, 6, 7)
  year_b <- substr(end_timestamp, 1, 4)
  month_b <- substr(end_timestamp, 6, 7)
  
  url <- paste0("https://raw.githubusercontent.com/od-ms/radverkehr-zaehlstellen/main/", station, "/", year_a, "-", month_a, ".csv")
  print(url, show_col_types = FALSE)
  # Check wether a file is existing
  if(url_exists(url)){
    data <- read_csv(url)
  }else{
    data <- NULL
  }

  # Check wether the start and end date are in the same month
  # If no, multiple files need to be loaded
  if(year_a != year_b | month_a != month_b) {
    
    i <- as.numeric(month_a)
    j <- as.numeric(year_a)
    while(TRUE){
      # Breaking condition
      if(j == year_b && i == as.numeric(month_b)){
        break
      }
      if(i == 12){
        i <- 1
        j <- j + 1
      }else{
        i <- i + 1
      }
      
      url <- paste0("https://raw.githubusercontent.com/od-ms/radverkehr-zaehlstellen/main/", station, "/", j, "-", sprintf("%02d", i), ".csv")
      print(url)
      if(url_exists(url)){
        data_2 <- read_csv(url, show_col_types = FALSE)
        #  Join the data when its not empty
        if(!is.null(data) && !is.null(data_2)){
          data <- rbind(data, data_2)
        }else{
          data <- data_2
        }
      }else{
        data_2 <- NULL
      }
    }
  }
  
  if(is.null(data)){
    hidePageSpinner()
    return(NULL)
  }
  
  data_filtered <- data %>%
    filter(Datetime >= as.POSIXct(start_timestamp) & Datetime <= as.POSIXct(end_timestamp))
  
  hidePageSpinner()
  return(data_filtered)
}

make_forecast <- function(daily_data, periods) {
  showPageSpinner()
  
  # Prepare data for prophet
  df <- data.frame(ds = as.Date(daily_data$date), y = daily_data$daily_mean)
  
  # Fit prophet model
  model <- prophet(df, yearly.seasonality = TRUE, daily.seasonality = TRUE)
  
  # Make forecast
  future <- make_future_dataframe(model, periods = periods)
  forecast <- predict(model, future)
  
  hidePageSpinner()
  
  return(list(
    model = model,
    forecast = forecast
  ))
}

locations <- st_read("fahrradzaehl-standorte_0.geojson")

# Shiny App
ui <- fluidPage(
  titlePanel("Bicycle counting stations in Münster"),
  tags$div(
    style = "position: absolute; top: 10px; right: 10px; z-index: 1000;",
    actionButton("open_info", "Info")
  ),
  sidebarLayout(
    sidebarPanel(
      width = 5,
      leafletOutput("map", height = 400),
      tags$br(),
      dropdownButton(
        circle = F,
        label = "Select stations",
        status = "default", 
        width = 450,
        tags$br(),
        tags$label("Choose:"),
        fluidRow(
          column(
            width = 6,
            checkboxGroupInput(
              inputId = "station",
              label = NULL,
              choices = unique(locations$name)[1:11],
            ),
          ),
          column(
            width = 6,
            checkboxGroupInput(
              inputId = "station",
              label = NULL,
              choices = unique(locations$name)[12:23],
            ),
          ),
            
          )
      ),
      tags$br(),
      dateRangeInput(
        "date_range", 
        "Choose timespan:", 
        start = "2024-12-01", 
        end = "2024-12-31",
        min = "2020-01-01", 
        max = "2024-12-31"
      ),
      actionButton("update", "Load data"),
      tags$br(),
      tags$br()
    ),
    
    mainPanel(
      width = 7,
      tabsetPanel(
        tabPanel("Line graph",
          plotlyOutput("line_plot"),
          uiOutput("extreme_text")
        ),
        tabPanel("Forecast",
                 plotOutput("forecast"),
                 verbatimTextOutput("forecast_text"),
                 sliderInput("forecast_days", "Forecast days:", 
                             min = 7, max = 365, value = 7, step = 1)),
        tabPanel("Autocorrelation",
                 plotOutput("acf_plot"),
                 verbatimTextOutput("acf_text"))
        
      )
    )
  )
)

server <- function(input, output, session) {
  output$forecast <- renderPlot(NULL)
  output$acf_plot <- renderPlot(NULL)
  output$forecast_text <- renderText(NULL)
  output$acf_text <- renderText(NULL)
  
  # Info-Screen
  show_info_screen <- function() {
    showModal(modalDialog(
      title = "Bycicle counting stations in Münster",
      p("With this app the user can analize the activity of the bycicle counting stations in Münster."),
      p("On the left one or more stations can be selected via a dropdown. The selected stations appear red in the map. Another way of selecting stations is by clicking on the markers in the map."),
      p("Also a timespan can be chosen. The data will be loaded by clicking the 'Load data' button."),
      p("The activity of the selected stations will be shown in a line plot."),
      p("Additionally the user can make a forecast for the next days. The number of days can be chosen by the user. This works when only one station is selected."),
      p("The temporal autocorrelation of the daily mean values can be shown in the third tab. This also works when only one station is selected."),
      p("Datasources: "),
      p(tags$a(href = "https://github.com/od-ms/radverkehr-zaehlstellen", 
                               "Github Open Data Portal Münster ", target = "_blank")),
      p(tags$a(href = "https://opendata.stadt-muenster.de/dataset/verkehrsz%C3%A4hlung-fahrradverkehr-standorte-der-radverkehr-z%C3%A4hlstellen/resource/cc2b5427-e70e", 
                               "Open Data Portal Münster ", target = "_blank")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  }
  show_info_screen()
  
  # Open Info when clicked
  observeEvent(input$open_info, {
    show_info_screen()
  })
  
  output$map <- renderLeaflet({
    leaflet(data = locations) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = 5,
        color = "blue",
        stroke = TRUE,
        fillOpacity = 0.8,
        label = ~name,
        layerId = ~name,
        labelOptions = labelOptions(
          #noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          style = list(
            "color" = "black",
            "font-size" = "12px",
            "font-weight" = "bold",
            "background" = "white",
            "border" = "1px solid black",
            "padding" = "2px"
          )
        )
      ) %>%
      setView(
        lng = mean(st_coordinates(locations)[, 1]),
        lat = mean(st_coordinates(locations)[, 2]),
        zoom = 13
      )
    })
  
  filtered_data <- eventReactive(input$update, {
    req(input$station)
    
    start_time <- paste(input$date_range[1], "00:00:00")
    end_time <- paste(input$date_range[2], "23:59:59")
    
    if (as.Date(input$date_range[2]) < as.Date(input$date_range[1])) {
      showNotification("Error: The startingdate must lie before the enddate!", type = "error")
      return(NULL)
    }
    
    stations <- locations %>% 
      filter(name %in% input$station) %>% 
      select(name, id)
    
    # Load data for each station
    data_list <- lapply(1:nrow(stations), function(i) {
      id <- stations$id[i]
      station_name <- stations$name[i]
      
      data <- getCSV(start_time, end_time, id)
      if (!is.null(data)) {
        colnames(data) <- c("datetime", "whole", "direction1", "direction2", "status_whole", "status_direction1", "status_direction2")
        data$station <- station_name
      }else{
        data <- NULL
      }
      return(data)
    })
    
    if (all(sapply(data_list, is.null))) {
      showNotification("Error: No data was found for the given timestamp.!", type = "error")
      return(NULL)
    }
    
    all_data <- bind_rows(data_list)
    
    data <- all_data
    
    # Check if multiple stations are selected
    if (length(unique(data$station)) != 1) {
      output$forecast_text <- renderText({
        "The forecast only works, when only one station is selected."
      })
      output$acf_text <- renderText({
        "The autocorrelation only works, when only one station is selected."
      })
      return(data)
    }
    
    # Calculate daily means
    daily_data <- data %>%
      mutate(date = as.Date(datetime)) %>%
      group_by(station, date) %>%
      summarise(daily_mean = mean(whole, na.rm = TRUE)) %>%
      ungroup()
    
    forecast_data <- make_forecast(daily_data, input$forecast_days)
    
    # Plot forecast
    output$forecast <- renderPlot({
      forecast_plot <- plot(forecast_data$model, forecast_data$forecast) +
        ggtitle(paste("Forecast of bicycle traffic for the next", input$forecast_days, "days"))
      print(forecast_plot)
    })
    
    output$acf_plot <- renderPlot({
      ggplot2_acf <- ggAcf(daily_data$daily_mean) + 
        ggtitle("Autocorrelation of Daily Mean Bike Counts") +
        xlab("Lag (days)") + 
        ylab("Autocorrelation") + 
        theme_minimal() + 
        theme(plot.title = element_text(hjust = 0.5))
      
      print(ggplot2_acf)
    })
    
    return(data)
  })
  
  observeEvent(input$update, {
    req(filtered_data())
    data <- filtered_data()
    # Calculate daily means
    daily_data <- data %>%
      mutate(date = as.Date(datetime)) %>%
      group_by(station, date) %>%
      summarise(daily_mean = mean(whole, na.rm = TRUE)) %>%
      ungroup()
    output$forecast <- renderPlot({
      forecast_data <- make_forecast(daily_data, input$forecast_days)
      plot(forecast_data$model, forecast_data$forecast) +
        ggtitle(paste("Forecast of bicycle traffic for the next", input$forecast_days, "days"))
    })
    
    output$acf_plot <- renderPlot({
      ggAcf(daily_data$daily_mean) +
        ggtitle("Autocorrelation of Daily Mean Bike Counts") +
        xlab("Lag (days)") +
        ylab("Autocorrelation") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
    })
  })
  
  
  extreme_values <- reactive({
    req(filtered_data())
    
    # Calculate minumum and maximum values for each station
    extreme_values <- filtered_data() %>%
      group_by(station) %>%
      summarise(
        max_time = datetime[which.max(whole)],
        max_value = max(whole, na.rm = TRUE),
        min_time = datetime[which.min(whole)],
        min_value = min(whole, na.rm = TRUE),
        mean_value = mean(whole, na.rm = TRUE)
      ) 
    
    extreme_values_long <- extreme_values %>%
      pivot_longer(cols = c(max_value, min_value), names_to = "type", values_to = "value") %>%
      mutate(datetime = as.POSIXct(ifelse(type == "max_value", max_time, min_time), origin = "1970-01-01", tz = "UTC"))
    
    extreme_values_long$mean_value <- extreme_values$mean_value[match(extreme_values_long$station, extreme_values$station)]
    
    return(extreme_values_long)
  })
  
  # Linegraph
  output$line_plot <- renderPlotly({
    data <- filtered_data()
    ev <- extreme_values()
    req(data)
    
    # Add a column where the weekday is stored
    data$Date <- as.Date(data$datetime)
    data$weekday <- weekdays(data$Date)
    
    # Filter weekends
    weekend_data <- data %>% 
      filter(weekday %in% c("Samstag", "Sonntag")) %>% 
      group_by(Date) %>% 
      summarise(start = min(datetime), end = max(datetime)) 
  
    p <- ggplot(data, aes(x = datetime, y = whole, color = station)) +
      geom_vline(data = subset(data, weekday == "Sonntag"), 
                 aes(xintercept = as.numeric(datetime)), 
                 color = "lightgrey", linetype = "solid", linewidth = 0.5) +
      geom_vline(data = subset(data, weekday == "Samstag"), 
                 aes(xintercept = as.numeric(datetime)), 
                 color = "lightgrey", linetype = "solid", linewidth = 0.5) +
      geom_line() +
      geom_point(data = subset(ev, type == "max_value"), 
                 aes(x = datetime, y = value, color = station), 
                 shape = 17, size = 3, show.legend = FALSE) +
      
      geom_point(data = subset(ev, type == "min_value"), 
                 aes(x = datetime, y = value, color = station), 
                 shape = 25, size = 3, show.legend = FALSE) +
      labs(
        title = "Activity for the chosen station(s)<br><sup>Weekends are highlighted grey. Minima and Maxima are also highlighted as triangles.</sup>",
        x = "Date",
        y = "Count"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y"), height = 400) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })
  
  output$extreme_text <- renderUI({
    req(extreme_values())
    ev <- extreme_values() %>% distinct(station, type, .keep_all = TRUE) %>%
      select(station, type, value, datetime, mean_value) %>%
      pivot_wider(names_from = type, values_from = c(value, datetime))
    print(ev)
    
    HTML(paste0(
      "<b>Extremwerte für die ausgewählten Stationen:</b><br>",
      paste0(
        "<b>", ev$station, ":</b> ",
        "<br>Maximum: ", ev$value_max_value,
        " at ", format(ev$datetime_max_value, "%d.%m.%Y %H:%M"),
        "<br>Minimum: ", ev$value_min_value,
        " at ", format(ev$datetime_min_value, "%d.%m.%Y %H:%M"),
        "<br>Mean value: ", round(ev$mean_value, 2),
        "<br><br>"
      ) %>% paste(collapse = "")
    ))
  })
  
  observeEvent(input$forecast_days, {
    # Den Schiebereglerwert abrufen
    forecast_horizon <- input$forecast_days
    data <- filtered_data()
    
    # Calculate daily means
    daily_data <- data %>%
      mutate(date = as.Date(datetime)) %>%
      group_by(station, date) %>%
      summarise(daily_mean = mean(whole, na.rm = TRUE)) %>%
      ungroup()
    
    req(data)
    
    # Überprüfen, ob nur eine Station ausgewählt ist
    if (length(unique(data$station)) != 1) {
      output$forecast_text <- renderText({
        "The forecast only works, when only one station is selected."
      })
      return()
    }
    
    # Vorhersage basierend auf den neuen Schiebereglerwert
    forecast_data <- make_forecast(daily_data, input$forecast_days)
    
    # Vorhersage plotten
    output$forecast <- renderPlot({
      forecast_plot <- plot(forecast_data$model, forecast_data$forecast) +
        ggtitle(paste("Forecast of bicycle traffic for the next", input$forecast_days, "days"))
      print(forecast_plot)
    })
  })
  
  # Observe map
  observeEvent(input$map_marker_click, {
    req(input$map_marker_click$id)
    
    current_selection <- character(0)
      
    clicked_station <- input$map_marker_click$id
    current_selection <- input$station
    
    # Delete or add station to selection
    if (clicked_station %in% current_selection) {
      updated_selection <- setdiff(current_selection, clicked_station)
    } else {
      updated_selection <- unique(c(current_selection, clicked_station))
    }
    
    updateCheckboxGroupInput(session, "station", selected = updated_selection)
  })
  
  # Update map when a station is selected
  observeEvent({list(input$station, input$update)}, {
    if (is.null(input$station) || length(input$station) == 0) {
      leafletProxy("map", data = locations) %>%
        clearMarkers() %>%
        addCircleMarkers(
          radius = 5,
          color = "blue",
          stroke = TRUE,
          fillOpacity = 0.5,
          label = ~name,
          layerId = ~name,
          labelOptions = labelOptions(
            #noHide = TRUE,
            direction = "top",
            textOnly = TRUE,
            style = list(
              "color" = "black",
              "font-size" = "12px",
              "font-weight" = "bold",
              "background" = "white",
              "border" = "1px solid black",
              "padding" = "2px"
            )
          )
        )
      return()
    }
    
    # Filter selected stations
    selected_stations <- locations %>% filter(name %in% input$station)
    
    # update map
    leafletProxy("map", data = locations) %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = locations,
        radius = 5,
        color = "blue",
        stroke = TRUE,
        fillOpacity = 0.5,
        label = ~name,
        layerId = ~name,
        labelOptions = labelOptions(
          #noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          style = list(
            "color" = "black",
            "font-size" = "12px",
            "font-weight" = "bold",
            "background" = "white",
            "border" = "1px solid black",
            "padding" = "2px"
          )
        )
      ) %>%
      addCircleMarkers(
        data = selected_stations,
        radius = 8,
        color = "red",
        stroke = TRUE,
        fillOpacity = 1,
        label = ~name,
        layerId = ~name,
        labelOptions = labelOptions(
          #noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          style = list(
            "color" = "black",
            "font-size" = "12px",
            "font-weight" = "bold",
            "background" = "white",
            "border" = "1px solid black",
            "padding" = "2px"
          )
        )
      )
  })
  
  outputOptions(output, "forecast", suspendWhenHidden = FALSE)
  outputOptions(output, "acf_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "forecast_text", suspendWhenHidden = FALSE)
  outputOptions(output, "acf_text", suspendWhenHidden = FALSE)
  
}

shinyApp(ui, server)
