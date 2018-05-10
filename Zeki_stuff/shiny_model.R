#Data/Packages Loading
library(lubridate)
library(tidyverse)
library(readr)
library(broom)
library(formattable)
library(shiny)

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

  group_by(StartDate) %>%
  summarise(avg_dist = mean(Distance_Miles), avg_leng = mean(Length_min), num_trips = n(), Rainfall = mean(daily_total), DayOfWeek = first(DayOfWeek), Season = first(Season), Temperature = mean(TAVG))

#Change ommitted dummies
#trips_plus <- within(trips_plus, RentalAccessPath <- relevel(RentalAccessPath, ref = "other"))
trips_plus <- within(trips_plus, DayOfWeek <- relevel(DayOfWeek, ref = "Wednesday"))




# Selecting Model Options
select_model_options <- c("Rainfall", "DayOfWeek", "Season", "Temperature")

# max/min year
min_year <- min(trips_plus$StartDate)
max_year <- max(trips_plus$StartDate)





# SHINY APP

ui <- fluidPage(
  
  # Application title
  titlePanel("Modeling Bike Share Use"),
  
  # Sidebar to select model variable and Date Range 
  sidebarLayout(
    # Where the options go
    sidebarPanel(
      selectInput(inputId = "predicted_variable", label = "Select Variable to Predict",
                  choices = c("Average Duration" = "avg_leng", "Average Distance" = "avg_dist", "Number of Trips" = "num_trips")),
      checkboxGroupInput(inputId = "variable_opts", label = "Select Variable for Model",
                  choices = select_model_options, selected = "Rainfall")#,
      #sliderInput(inputID = "hours", label = "Control for Time of Day?",
                  #min = min_year, max = max_year, value = c(0, 23))
    ),
    # Where the prediciton and table will go
    mainPanel(
      #plotOutput("prediction"),
      formattableOutput("prediction_table")
    )
  )
)




server <- function(input, output) {
  
  # adjusting hours
  #trips_filtered <- reactive({
  #  trips_plus %>% 
  #    filter(hour >= input$hours, hour <= input$hours)
  #})
  
  # making the actual model
  lm1 <- reactive({lm(reformulate(input$variable_opts,input$predicted_variable), data = trips_plus)})
  
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
}



# Run the application 
shinyApp(ui = ui, server = server)