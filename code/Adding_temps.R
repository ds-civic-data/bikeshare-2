library(tidyverse)
library(lubridate)

# all this script does is add daily temperatures

trips_rain <- read.csv("~/R-stuff/bikeshare-2/data/trips_rain.csv")
temps <- read.csv("~/R-stuff/bikeshare-2/raw_data/temperature.csv")

temps$DATE <- mdy(temps$DATE)
trips_rain$StartDate <- ymd(trips_rain$StartDate)

temps <- slice(temps, 3:623)

temps <- temps %>%
  select(DATE, PRCP, SNWD, TAVG, TMAX, TMIN)

colnames(temps)[1] <- "StartDate"

trips_temp_rain <- full_join(temps, trips_rain)

write.csv(trips_temp_rain, "~/R-stuff/bikeshare-2/data/trips_temp_rain.csv")