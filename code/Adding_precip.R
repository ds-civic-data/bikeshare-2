library(tidyverse)
library(lubridate)

# all this script does is add hourly precipitaion
full_trips <- read_csv("~/R-stuff/bikeshare-2/raw_data/full_trips.csv")
rain_fall <- read.table("~/R-stuff/bikeshare-2/raw_data/rain.txt", sep = "")
names(rain_fall) <- as.character(c("StartDate", "daily_total", 0:23))

rain_fall_gathered <- rain_fall %>%
  gather(3:26, key = "hour", value = "rainfall")

rain_fall_gathered$StartDate <- dmy(rain_fall_gathered$StartDate)
rain_fall_gathered$hour <- as.numeric(rain_fall_gathered$hour)


trips_rain <- full_trips

trips_rain$StartDate <- mdy(trips_rain$StartDate)
trips_rain$EndDate <- mdy(trips_rain$EndDate)

trips_rain <- trips_rain %>%
  filter(!(is.na(StartDate) | is.na(EndDate))) %>%
  mutate(hour = hour(StartTime))


trips_rain <- full_join(trips_rain, rain_fall_gathered)
write.csv(trips_rain, "~/R-stuff/bikeshare-2/data/trips_rain.csv")
























