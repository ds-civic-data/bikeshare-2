library(lubridate)
library(tidyverse)
library(readr)
library(broom)
library(formattable)
library(shiny)
library(shinydashboard)

# getting data, modifying
trips_temp_rain <- read_csv("~/R-stuff/bikeshare-2/data/trips_temp_rain.csv")

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
