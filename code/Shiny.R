library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)
source("~/R-stuff/bikeshare-2/code/bike_pred.R")
source("~/R-stuff/bikeshare-2/code/shiny_map_functs.R")
source("~/R-stuff/bikeshare-2/code/shiny_pred_functs.R")


# Shiny app starts here

ui <- dashboardPage(
  dashboardHeader(title = "Biketown"),
  dashboardSidebar(sidebarMenu(
    menuItem("Explore", tabName = "explore"),
    menuItem("Predict", tabName = "predict"),
    menuItem("Models", tabName = "models", icon = icon("bar-chart-o")),
    menuItem("Estimate", tabName = "preds", icon = icon("th")),
    menuItem("Residual Plot", tabName = "resid", icon = icon("dashboard")))),
  dashboardBody(tabItems(tabItem(tabName = "explore", 
                                 fluidRow(column(width = 9,
                                                 leafletOutput("mymap", height = "700px")),
                                          column(width = 3, box(width = "100px", actionButton("reset", "Reset"),
                                                                checkboxInput("out", "Top Outgoing Stations", value = TRUE),
                                                                checkboxInput("incoming", "Top Incoming Stations", value = FALSE),
                                                                sliderInput("slide", "Number of top stations to display", 1, 40, 10))))),
                         tabItem(tabName = "predict",
                                 fluidRow(column(width = 9, leafletOutput("predmap", height = "700px")),
                                          column(width = 3, box(width = "100px", 
                                                                sliderInput("predtime", "Hours ahead to predict", 1, 16, 1),
                                                                sliderInput("mintemp", "Minimum Temperature (F)", 20, 100, 50),
                                                                sliderInput("maxtemp", "Maximum Temperature (F)", 20, 100, 50),
                                                                sliderInput("rain", "Rainfall (1/100 inches)", 0, 300, 0),
                                                                sliderInput("hour", "Hour of day (24H time)", 0, 23, 12),
                                                                dateInput("date", "Date", format = "yyyy-mm-dd", startview = "month", weekstart = 7,
                                                                          language = "en", width = NULL),
                                                                actionButton("predict1", "Predict"),
                                                                actionButton("current", "Add Current Distribution")
                                          )
                                          )
                                 )
                         ),
                         tabItem(tabName = "preds",
                                 
                                 # Where the options go
                                 box(title = "Variable Selection",
                                     selectInput(inputId = "predictor_variable", label = "Select Variable to Predict",
                                                 choices = c("Average Duration" = "avg_leng", "Average Distance" = "avg_dist", "Number of Trips" = "num_trips")),
                                     checkboxGroupInput(inputId = "variable_opts", label = "Select Variable for Model",
                                                        choices = select_model_options, selected = "Rainfall")#,
                                     #sliderInput(inputID = "hours", label = "Control for Time of Day?",
                                     #min = min_year, max = max_year, value = c(0, 23))
                                 ),
                                 # Where the prediciton and table will go
                                 box(title = "Output",
                                     #plotOutput("prediction"),
                                     formattableOutput("prediction_table")
                                 )
                         ),
                         tabItem(tabName = "resid",
                                 box(title = "Residual Plot",
                                     plotOutput("resid_plot"))
                                 
                         ),
                         tabItem(tabName = "models",
                                 box(title = "Variable Selection",
                                     # Prediction Selection
                                     selectInput(inputId = "predicted_variable", label = "Select Variable to Predict",
                                                 choices = c("Average Duration" = "avg_leng", "Average Distance" = "avg_dist", "Number of Trips" = "num_trips")),
                                     # Day of week selection
                                     selectInput(inputId = "day", label = "Select Day of the Week",
                                                 choices = list("Sunday","Monday", "Tuesday", "Wednesday","Thursday", "Friday", "Saturday"),
                                                 selected = "Wednesday"),
                                     # Enter amount of rainfall
                                     numericInput(inputId = "rainfall", label = "Select Level of Rainfall (.01 inch)",
                                                  0, min = 0, max = 100),
                                     # Enter temp
                                     numericInput(inputId = "temp", label = "Select Temperature (Degrees Fahrenheit)",
                                                  60, min = 0, max = 150),
                                     # Season Selection
                                     selectInput(inputId = "Season", label = "Select Season:",
                                                 choices = list("Spring", "Summer", "Autumn", "Winter"),
                                                 selected = "Autumn")
                                 ),
                                 # Where the output goes
                                 box(formattableOutput("prediction")
                                 )
                         )
  )
  )
)




server <- function(input, output, session) {
  output$mymap <- renderLeaflet({map_basic})
  # Removes lines
  observeEvent(input$reset, {output$mymap <- renderLeaflet({map_basic})}) 
  
  
  # Creates lines between station you clicked on
  observeEvent(input$mymap_marker_click, {
    proxy <- leafletProxy("mymap") %>%
      clearGroup(group = "line")
    click <- input$mymap_marker_click
    if (input$out == TRUE) {
      top <- make_top_stations(click$id, input$slide)
      make_line_map(proxy, top, input$slide)
    }
    
    if (input$incoming == TRUE) {
      top <- make_in_top_stations(click$id, input$slide)
      make_line_map(proxy, top, input$slide)
    }
    
    
  })
  output$predmap <- renderLeaflet(leaflet() %>% 
                                    addProviderTiles(providers$Stamen.Terrain) %>%
                                    setView(lng = -122.6765, lat= 45.5231, zoom = 12))
  
  observeEvent(input$predict1, {
    pred_data <- pred_distribution(input$hour, input$date, input$maxtemp, input$mintemp,input$rain, 
                                   input$predtime)
    
    proxy2 <- leafletProxy("predmap") %>% 
      addProviderTiles(providers$Stamen.Terrain) %>% 
      clearMarkers() %>%
      addCircleMarkers(lng=pred_data$lon, 
                       lat=pred_data$lat,
                       radius = sqrt(20*((abs(pred_data$diff) + 1))), 
                       stroke = F,
                       fillOpacity = .6,
                       color = if_else(pred_data$diff > 0, "green", "red"),
                       popup = paste("Predicted Change in Bikes: ", round(pred_data$diff)))
  })
  
 
  observeEvent(input$current, {
    pred_data <- pred_distribution(input$hour, input$date, input$maxtemp, input$mintemp,input$rain, 
                                   input$predtime)
    proxy3 <- leafletProxy("predmap") %>%
      clearMarkers() %>%
      addCircleMarkers(lng=pred_data$lon, 
                       lat=pred_data$lat,
                       radius = if_else(pred_data$aval == 0, 5, 3*sqrt(pred_data$aval) + 2), 
                       stroke = F,
                       fillOpacity = .6,
                       color = if_else(pred_data$aval < 2, "black", if_else(pred_data$diff > 0, "green", "red")),
                       popup = paste("Predicted Available Bikes: ", round(pred_data$aval)))
    
  })
  
  # adjusting hours
  #trips_filtered <- reactive({
  #  trips_plus %>% 
  #    filter(hour >= input$hours, hour <= input$hours)
  #})
  
  # making the actual model
  lm1 <- reactive({lm(reformulate(input$variable_opts,input$predictor_variable), data = trips_plus)})
  
  # building initial table for use in the output
  output_table <- reactive({tidy(lm1()) %>%
      rename(Predictor = "term") %>%
      select(1,2,5) %>%
      slice(-1) %>%
      mutate(Effect = if_else(estimate > 0, "Positive", "Negative")) %>%
      mutate(Significance = if_else(p.value < .01, "Very Significant", 
                                    if_else(p.value < .05, "Significant", 
                                            if_else(p.value < .1, "Somewhat Significant", "Not Significant")))) %>%
      select(-2,-3)
  })
  # actual output of table
  output$prediction_table <- renderFormattable({
    formattable(output_table(), list(
      Effect = formatter("span",
                         style = x ~ style(color = ifelse(x == "Negative" , "red", "green")),
                         x ~ icontext(ifelse(x == "Negative", "arrow-down", "arrow-up"), x)),
      Significance = formatter(
        "span",
        style = x ~ style(color = ifelse(x %in% c("Very Significant","Significant"), "blue", 
                                         ifelse(x == "Somewhat Significant" , "brown", "black"))))))
  })
  
  output$resid_plot <- renderPlot({
    ggplot(lm1()) + geom_point(aes(x=.fitted, y=.resid))
  })
  # Making the model for use in the prediction
  prediction <- reactive({
    vars <- c("Rainfall","DayOfWeek","Season","Temperature")
    m1 <- lm(reformulate(vars,input$predicted_variable), data = trips_plus)
    function(valRain = 0, valDay = "Wednesday", valSeas = "Autumn", valTemp = 60) {
      
      #Set elements of vect based on inputs 
      vect = c(1,valRain,0,0,0,0,valTemp,0,0,0,0,0,0,0,0)
      vect[3]<-if_else(valDay == "Sunday", 1, 0)
      vect[4]<-if_else(valSeas == "Spring", 1, 0)
      vect[5]<-if_else(valSeas == "Summer", 1, 0)
      vect[6]<-if_else(valSeas == "Winter", 1, 0)
      vect[11]<-if_else(valDay == "Monday", 1, 0)
      vect[12]<-if_else(valDay == "Tuesday", 1, 0)
      vect[13]<-if_else(valDay == "Thursday", 1, 0)
      vect[14]<-if_else(valDay == "Friday", 1, 0)
      vect[15]<-if_else(valDay == "Saturday", 1, 0)
      
      term = c("(Intercept)", "Rainfall","DayOfWeekSunday", "SeasonSpring", "SeasonSummer", "SeasonWinter", "Temperature", "RentalAccessPathkeypad" , "RentalAccessPathkeypad_rfid_card", "RentalAccessPathmobile", "DayOfWeekMonday", "DayOfWeekTuesday", "DayOfWeekThursday", "DayOfWeekFriday", "DayOfWeekSaturday")
      
      #Calculate prediction
      formattable(data_frame(term) %>%
                    left_join(tidy(m1), by = c("term" = "term")) %>%
                    select(1,2) %>%
                    mutate(values = vect) %>%
                    filter(!is.na(estimate)) %>%
                    mutate(n1 = estimate*values) %>%
                    summarise(Prediction = sum(n1)))
    }
  })
  
  # Calling prediction function, assigning to output
  output$prediction <- renderFormattable({
    
    prediction()(valRain = input$rainfall, valDay = input$day, valSeas = input$Season,valTemp = input$temp)
  })
  
  
}

shinyApp(ui, server)














