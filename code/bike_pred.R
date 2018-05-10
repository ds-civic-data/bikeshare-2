
library(tidyverse)
library(jsonlite)
library(lubridate)
raw_counts <- read_csv("~/R-stuff/bikeshare-2/data/raw_trips.csv")
full_trips <- read_csv("~/R-stuff/bikeshare-2/data/trips_temp_rain.csv") %>%
  mutate(DayOfWeek = format(factor(wday(StartDate)), format = "%a")) 


stations3 <- read_csv("~/R-stuff/bikeshare-2/data/stations.csv") 

# Editing Stations to be joinable
stations3$StartHub <- as.character(stations3$StartHub)
stations3$EndHub <- as.character(stations3$EndHub)

stations3[13, 5] <- "SE Taylor at Chavez"
stations3[13, 6] <- "SE Taylor at Chavez"
stations3[62, 5] <- "NW 22nd at LoveJoy"
stations3[62, 6] <- "NW 22nd at LoveJoy"

station_end <- stations3 %>% 
  select(EndHub, end_id)

station_start <- stations3 %>% 
  select(StartHub, start_id)

get_current <- function() {
  json_dist <- fromJSON("http://biketownpdx.socialbicycles.com/opendata/station_status.json")
  # Putting current distribution into data frame
  temp_dist <- data_frame(json_dist[["data"]][["stations"]][["station_id"]], json_dist[["data"]][["stations"]][["num_bikes_available"]], json_dist[["data"]][["stations"]][["num_docks_available"]])
  names(temp_dist) <- c("id", "aval", "empty")
  
  stations2 <- stations3 %>%
    select(start_id, lon, lat, StartHub)
  
  names(stations2) <- c("id", "lon", "lat", "name")
  
  distribtion <- inner_join(stations2, temp_dist)
  return(distribtion)
}




full_trips <- full_join(station_end, full_trips) %>%
  full_join(station_start) %>%
  filter(!(is.na(start_id) & is.na(end_id))) %>%
  mutate(start_id = if_else(is.na(start_id), "free", start_id), 
         end_id = if_else(is.na(end_id), "free", end_id))





# Function to filter trips to only include trips taken under 'similar' conditions (weather, season, day of wekk, time)
filter_trips <- function(time, date, temp) {
  date <- ymd(date)
  DOW <- if_else(wday(date) %in% c(1,7), 1, 0)
  Dtseason <- if_else(month(date) %in% c(6, 7, 8), "Summer", if_else(month(date) %in% c(9, 10, 11), "Autumn", if_else(month(date) %in% c(12, 1, 2), "Winter", "Spring")))
  
  new_trips <- full_trips %>% 
    select(StartDate, hour, DayOfWeek, daily_total, Distance_Miles, 
           TAVG, TMAX, TMIN, start_id, end_id, rainfall) %>%
    mutate(wday_group = if_else(DayOfWeek %in% c(1,7), 1, 0), 
           Season = if_else(month(StartDate) %in% c(6, 7, 8), "Summer",
                            if_else(month(StartDate) %in% c(9, 10, 11), "Autumn",
                                    if_else(month(StartDate) %in% c(12, 1, 2), "Winter", "Spring")))) %>%
    mutate(month = format(StartDate, format = "%m")) %>%
    filter(hour > (time - 2) & 
             hour < (time + 2) & 
             temp > (TAVG - 10) & 
             temp < (TAVG + 10) & 
             DOW == wday_group & 
             Dtseason == Season) 
  
  return(new_trips)
}



# Function to calculate trip proportions. Gives dataframe with rows for each start-end combination found in filtered data. Then divides by the sum of those counts to find proportions.

make_prop <- function(trips) {
  trips_counts <- trips %>%
    select(start_id, end_id) %>%
    group_by(start_id, end_id) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    mutate(prop = count/sum(count))
}

# Creates model for number of trips per hour based on filtered data set. Takes temp, rain, time, day of week as predictors
make_model <- function(trip) {
  model_data <- trip %>% group_by(hour, StartDate) %>%
    summarise(trips = n(), 
              tavg = mean(TAVG), 
              tmin = mean(TMIN),
              tmax = mean(TMAX), 
              daily_rain = mean(daily_total),
              wd = mean(as.numeric(DayOfWeek)), 
              hour_rain = mean(rainfall))
  
  m <- lm(trips ~ tmax + tmin + daily_rain + wd^2 + hour^2, data = model_data)
  return(m)
}

# Uses model from last function to predict the number of trips over a period of hours.

make_trip_pred <- function(num_hours, model, tmax, tmin, daily_rain, wd, hour) {
  i <- 0
  num_trips <- 0
  while (i < num_hours) {
    num_trips <- num_trips + predict(model, newdata = data.frame(tmax = tmax, tmin = tmin, daily_rain = daily_rain, wd = wd, hour = (hour + i)))
    
    i = i + 1
  }
  return(num_trips)
}



# Combines previous functions into one that given parameters, gives back a total number of predicted trips
full_predict_trips <- function(hour, date, tmax, tmin, rain, num_hours) {
  i <- 0
  predicted_trips <- 0
  while(i < num_hours) {
    filtered <- filter_trips((hour+i), ymd(date), ((tmin+tmax)/2))
    m <- make_model(filtered)
    predicted_trips <- (predicted_trips + predict(m, newdata = data.frame(tmax = tmax, 
                                      tmin = tmin, daily_rain = rain,
                                      wd = wday(ymd(date)),
                                      hour = (hour + i))))
    
    i <- i + 1
  }
  
  
  return(predicted_trips)
}


# Puts everything together  into one function. Outputs a dataset with each station's lat/long, the predicted change, and the distribution pulled from JSON file.
pred_distribution <- function(hour, date, tmax, tmin, rain, num_hours) {
  filtered <- filter_trips(hour, ymd(date), ((tmin+tmax)/2))
  props <- make_prop(filtered)
  trip_total <- full_predict_trips(hour, date, tmax, tmin, rain, num_hours)
  trip_prediction <- props$prop*trip_total
  total <- cbind(props, trip_prediction)
  
  starts <- total %>%
    group_by(start_id) %>%
    summarise(leaving = sum(trip_prediction))
  ends <- total %>%
    group_by(end_id) %>%
    summarise(incoming = sum(trip_prediction))
  
  trips <- inner_join(starts, ends, by = c("start_id" = "end_id")) %>%
    mutate(diff = (incoming - leaving)) %>% 
    select(start_id, diff)
  
  names(trips) <- c("id", "diff")
  current <- get_current()
  final <- inner_join(trips, current) %>%
    mutate(predicted = aval + diff)
  return(final)
}














