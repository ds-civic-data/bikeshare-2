# Code for the "prediction" from the model. Just makes a prediction.

#Data/Packages Loading
library(lubridate)
library(tidyverse)
library(readr)
library(broom)
library(formattable)
library(shinydashboard)

# getting data, modifying
setwd("/home/kazanz/bikeshare-2")
trips_temp_rain <- read_csv("Zeki_stuff/trips_temp_rain.csv")

trips_plus <- trips_temp_rain %>%
  #Add variables for Day of Week, Season
  mutate(DayOfWeek = factor(wday(StartDate)),
         Season = if_else(month(StartDate) %in% c(6, 7, 8), "Summer", if_else(month(StartDate) %in% c(9, 10, 11), "Autumn", if_else(month(StartDate) %in% c(12, 1, 2), "Winter", "Spring"))),
         DayOfWeek = recode_factor(DayOfWeek, `1` = "Sunday", `2` = "Monday", `3` = "Tuesday", `4` = "Wednesday", `5` = "Thursday", `6` = "Friday", `7` = "Saturday")) %>%
  #Recode RentalAccessPath
  mutate(RentalAccessPath = if_else(RentalAccessPath %in% c("admin","keypad_phone_number","unknown","web"), "other", RentalAccessPath),
         RentalAccessPath = factor(RentalAccessPath)) %>%
  #Change duration to minutes
  mutate(Length_sec = seconds(Duration),
         Length_sec = substr(Length_sec,1,nchar(Length_sec)-0),
         Length_min = as.numeric(Length_sec)/60) %>%
  #Miscelaneous other stuff
  filter(!is.na(Duration), !is.na(Distance_Miles)) %>%
  select("StartDate","TAVG","Distance_Miles","Length_min","daily_total","DayOfWeek","Season") %>%
  # Aggregating
  group_by(StartDate) %>%
  summarise(avg_dist = mean(Distance_Miles), avg_leng = mean(Length_min), num_trips = n(), Rainfall = mean(daily_total), DayOfWeek = first(DayOfWeek), Season = first(Season), Temperature = mean(TAVG))

#Change ommitted dummies
#trips_plus <- within(trips_plus, RentalAccessPath <- relevel(RentalAccessPath, ref = "other"))
trips_plus <- within(trips_plus, DayOfWeek <- relevel(DayOfWeek, ref = "Wednesday"))



# MAKING PREDICTION FUNCTION


# BUILDING SHINY APP

ui <- dashboardPage(
  
  # Title
  dashboardHeader(title = "Shiny App"),
  dashboardSidebar(),
  # Sidebar to select model variable and Date Range 
  dashboardBody(
    # Where the options go
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

server <- function(input, output) {
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


# Running the application 
shinyApp(ui = ui, server = server)