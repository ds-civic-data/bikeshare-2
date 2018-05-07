Preliminary Model
================
Zeki Kazan
5/6/2018

Pre-modeling Setup

``` r
trips_plus <- trips_rain %>%
  
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
  select(2,6,7,16,18,20:24,26) %>%
  rename(Rainfall = rainfall)

#Change ommitted dummies
trips_plus <- within(trips_plus, RentalAccessPath <- relevel(RentalAccessPath, ref = "other"))
trips_plus <- within(trips_plus, DayOfWeek <- relevel(DayOfWeek, ref = "Wednesday"))
```

Filtering

``` r
trips_filtered <- trips_plus %>%
  #Choose time of day to analyze
  filter(hour >= 0, hour <= 23) %>%
  #Choose dates to analyze
  filter(date(StartDate) >= ymd("2016-07-19"), date(StartDate) <= ymd("2018-03-31"))
```

Model 1 (Nonaggregated): Predicting Distance Traveled (in miles) and Length of Trip (in minutes)

``` r
#Choose to regress on Length_min or Distance_Miles
#Choose predictors from: Rainfall, DayOfWeek, Season, PaymentPlan, RentalAccessPath, More???
m <- lm(Distance_Miles ~ Rainfall + DayOfWeek + Season + PaymentPlan + RentalAccessPath, data = trips_filtered)

summary(m)
```

    ## 
    ## Call:
    ## lm(formula = Distance_Miles ~ Rainfall + DayOfWeek + Season + 
    ##     PaymentPlan + RentalAccessPath, data = trips_filtered)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##   -3.3   -1.1   -0.5    0.4 5255.7 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                       2.90818    0.53051   5.482 4.21e-08 ***
    ## Rainfall                         -0.02423    0.02365  -1.025   0.3055    
    ## DayOfWeekSunday                   0.14600    0.12959   1.127   0.2599    
    ## DayOfWeekMonday                  -0.10113    0.13273  -0.762   0.4461    
    ## DayOfWeekTuesday                  0.26281    0.13238   1.985   0.0471 *  
    ## DayOfWeekThursday                -0.15792    0.13208  -1.196   0.2318    
    ## DayOfWeekFriday                  -0.00306    0.12771  -0.024   0.9809    
    ## DayOfWeekSaturday                -0.09776    0.12536  -0.780   0.4355    
    ## SeasonSpring                      0.09112    0.10856   0.839   0.4013    
    ## SeasonSummer                      0.15390    0.08138   1.891   0.0586 .  
    ## SeasonWinter                     -0.07605    0.11733  -0.648   0.5168    
    ## PaymentPlanSubscriber            -0.91939    0.07270 -12.646  < 2e-16 ***
    ## RentalAccessPathkeypad           -0.45502    0.52052  -0.874   0.3820    
    ## RentalAccessPathkeypad_rfid_card -0.69791    0.53670  -1.300   0.1935    
    ## RentalAccessPathmobile           -0.65469    0.53592  -1.222   0.2219    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 23.89 on 497014 degrees of freedom
    ## Multiple R-squared:  0.0004774,  Adjusted R-squared:  0.0004493 
    ## F-statistic: 16.96 on 14 and 497014 DF,  p-value: < 2.2e-16

``` r
#Create Output table (requires broom package)
output_table <- tidy(m) %>%
  rename(Predictor = "term") %>%
  select(1,2,5) %>%
  slice(-1) %>%
  mutate(Effect = if_else(estimate > 0, "Positive", "Negative")) %>%
  mutate(Significance = if_else(p.value < .01, "Very Significant", if_else(p.value < .05, "Significant", if_else(p.value < .1, "Somewhat Significant", "Not Significant")))) %>%
  select(-2,-3)

#Make Output Table Look Fancy (requires formattable package)
formattable(output_table, list(
  Effect = formatter(
    "span",
    style = x ~ style(color = ifelse(x == "Negative" , "red", "green")),
    x ~ icontext(ifelse(x == "Negative", "arrow-down", "arrow-up"), x)),
  Significance = formatter(
    "span",
    style = x ~ style(color = ifelse(x %in% c("Very Significant","Significant") , "blue", ifelse(x == "Somewhat Significant" , "brown", "black"))))))
```

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:right;">
Predictor
</th>
<th style="text-align:right;">
Effect
</th>
<th style="text-align:right;">
Significance
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
Rainfall
</td>
<td style="text-align:right;">
<span style="color: red"> <i class="glyphicon glyphicon-arrow-down"></i> Negative </span>
</td>
<td style="text-align:right;">
<span style="color: black">Not Significant </span>
</td>
</tr>
<tr>
<td style="text-align:right;">
DayOfWeekSunday
</td>
<td style="text-align:right;">
<span style="color: green"> <i class="glyphicon glyphicon-arrow-up"></i> Positive </span>
</td>
<td style="text-align:right;">
<span style="color: black">Not Significant </span>
</td>
</tr>
<tr>
<td style="text-align:right;">
DayOfWeekMonday
</td>
<td style="text-align:right;">
<span style="color: red"> <i class="glyphicon glyphicon-arrow-down"></i> Negative </span>
</td>
<td style="text-align:right;">
<span style="color: black">Not Significant </span>
</td>
</tr>
<tr>
<td style="text-align:right;">
DayOfWeekTuesday
</td>
<td style="text-align:right;">
<span style="color: green"> <i class="glyphicon glyphicon-arrow-up"></i> Positive </span>
</td>
<td style="text-align:right;">
<span style="color: blue">Significant </span>
</td>
</tr>
<tr>
<td style="text-align:right;">
DayOfWeekThursday
</td>
<td style="text-align:right;">
<span style="color: red"> <i class="glyphicon glyphicon-arrow-down"></i> Negative </span>
</td>
<td style="text-align:right;">
<span style="color: black">Not Significant </span>
</td>
</tr>
<tr>
<td style="text-align:right;">
DayOfWeekFriday
</td>
<td style="text-align:right;">
<span style="color: red"> <i class="glyphicon glyphicon-arrow-down"></i> Negative </span>
</td>
<td style="text-align:right;">
<span style="color: black">Not Significant </span>
</td>
</tr>
<tr>
<td style="text-align:right;">
DayOfWeekSaturday
</td>
<td style="text-align:right;">
<span style="color: red"> <i class="glyphicon glyphicon-arrow-down"></i> Negative </span>
</td>
<td style="text-align:right;">
<span style="color: black">Not Significant </span>
</td>
</tr>
<tr>
<td style="text-align:right;">
SeasonSpring
</td>
<td style="text-align:right;">
<span style="color: green"> <i class="glyphicon glyphicon-arrow-up"></i> Positive </span>
</td>
<td style="text-align:right;">
<span style="color: black">Not Significant </span>
</td>
</tr>
<tr>
<td style="text-align:right;">
SeasonSummer
</td>
<td style="text-align:right;">
<span style="color: green"> <i class="glyphicon glyphicon-arrow-up"></i> Positive </span>
</td>
<td style="text-align:right;">
<span style="color: brown">Somewhat Significant</span>
</td>
</tr>
<tr>
<td style="text-align:right;">
SeasonWinter
</td>
<td style="text-align:right;">
<span style="color: red"> <i class="glyphicon glyphicon-arrow-down"></i> Negative </span>
</td>
<td style="text-align:right;">
<span style="color: black">Not Significant </span>
</td>
</tr>
<tr>
<td style="text-align:right;">
PaymentPlanSubscriber
</td>
<td style="text-align:right;">
<span style="color: red"> <i class="glyphicon glyphicon-arrow-down"></i> Negative </span>
</td>
<td style="text-align:right;">
<span style="color: blue">Very Significant </span>
</td>
</tr>
<tr>
<td style="text-align:right;">
RentalAccessPathkeypad
</td>
<td style="text-align:right;">
<span style="color: red"> <i class="glyphicon glyphicon-arrow-down"></i> Negative </span>
</td>
<td style="text-align:right;">
<span style="color: black">Not Significant </span>
</td>
</tr>
<tr>
<td style="text-align:right;">
RentalAccessPathkeypad\_rfid\_card
</td>
<td style="text-align:right;">
<span style="color: red"> <i class="glyphicon glyphicon-arrow-down"></i> Negative </span>
</td>
<td style="text-align:right;">
<span style="color: black">Not Significant </span>
</td>
</tr>
<tr>
<td style="text-align:right;">
RentalAccessPathmobile
</td>
<td style="text-align:right;">
<span style="color: red"> <i class="glyphicon glyphicon-arrow-down"></i> Negative </span>
</td>
<td style="text-align:right;">
<span style="color: black">Not Significant </span>
</td>
</tr>
</tbody>
</table>
``` r
#Function that takes in values for each of the variables and outputs a prediction for the dependent variable
prediction <- function(valRain = 0, valDay = "Wednesday", valSeas = "Autumn", valPaym = "Casual", valPath = "other") {

  #Set elements of vect based on inputs 
  vect = c(1,valRain,0,0,0,0,0,0,0,0,0,0,0,0,0)
  vect[3]<-if_else(valDay == "Sunday", 1, 0)
  vect[4]<-if_else(valSeas == "Spring", 1, 0)
  vect[5]<-if_else(valSeas == "Summer", 1, 0)
  vect[6]<-if_else(valSeas == "Winter", 1, 0)
  vect[7]<-if_else(valPaym == "Subscriber", 1, 0)
  vect[8]<-if_else(valPath == "keypad", 1, 0)
  vect[9]<-if_else(valPath == "keypad_rfid_card", 1, 0)
  vect[10]<-if_else(valPath == "mobile", 1, 0)
  vect[11]<-if_else(valDay == "Monday", 1, 0)
  vect[12]<-if_else(valDay == "Tuesday", 1, 0)
  vect[13]<-if_else(valDay == "Thursday", 1, 0)
  vect[14]<-if_else(valDay == "Friday", 1, 0)
  vect[15]<-if_else(valDay == "Saturday", 1, 0)
  
  term = c("(Intercept)", "Rainfall","DayOfWeekSunday", "SeasonSpring", "SeasonSummer", "SeasonWinter", "PaymentPlanSubscriber", "RentalAccessPathkeypad" , "RentalAccessPathkeypad_rfid_card", "RentalAccessPathmobile", "DayOfWeekMonday", "DayOfWeekTuesday", "DayOfWeekThursday", "DayOfWeekFriday", "DayOfWeekSaturday")
  
  #Calculate prediction
  formattable(data_frame(term) %>%
  left_join(tidy(m), by = c("term" = "term")) %>%
  select(1,2) %>%
  mutate(values = vect) %>%
  filter(!is.na(estimate)) %>%
  mutate(n1 = estimate*values) %>%
  summarise(prediction = sum(n1)))
}

#An example of how the function works
prediction(valRain = 1,valDay = "Saturday",valSeas = "Autumn", valPaym = "Casual", valPath = "keypad")
```

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:right;">
prediction
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
2.331164
</td>
</tr>
</tbody>
</table>
Model 2 (Aggregated): Predicting average distance traveled (in miles), average length of trip (in minutes), or number of trips per day.

``` r
trips_filtered_more <- trips_filtered %>%
  group_by(StartDate) %>%
  summarise(avg_dist = mean(Distance_Miles), avg_leng = mean(Length_min), num_trips = n(), Rainfall = mean(daily_total), DayOfWeek = first(DayOfWeek), Season = first(Season))
#Choose to regress on avg_dist, avg_leng, or num_trips
#Choose predictors from: Rainfall, DayOfWeek, Season, More???
m1 <- lm(num_trips ~ Rainfall + DayOfWeek + Season, data = trips_filtered_more)

summary(m1)
```

    ## 
    ## Call:
    ## lm(formula = num_trips ~ Rainfall + DayOfWeek + Season, data = trips_filtered_more)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -955.79 -191.25  -35.96  163.45 1419.32 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        822.9649    41.6724  19.748  < 2e-16 ***
    ## Rainfall            -4.9208     0.4567 -10.774  < 2e-16 ***
    ## DayOfWeekSunday    133.8693    50.2287   2.665  0.00791 ** 
    ## DayOfWeekMonday     -7.4007    50.0978  -0.148  0.88261    
    ## DayOfWeekTuesday   -19.2780    49.9488  -0.386  0.69967    
    ## DayOfWeekThursday   20.3463    49.9547   0.407  0.68394    
    ## DayOfWeekFriday    117.4267    49.9553   2.351  0.01908 *  
    ## DayOfWeekSaturday  270.5552    50.0760   5.403 9.59e-08 ***
    ## SeasonSpring       -43.8386    41.5369  -1.055  0.29168    
    ## SeasonSummer       723.1615    37.7733  19.145  < 2e-16 ***
    ## SeasonWinter      -468.6076    34.2207 -13.694  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 325.4 on 580 degrees of freedom
    ## Multiple R-squared:  0.7058, Adjusted R-squared:  0.7007 
    ## F-statistic: 139.1 on 10 and 580 DF,  p-value: < 2.2e-16

``` r
#Create Output table (requires broom package)
output_table <- tidy(m1) %>%
  rename(Predictor = "term") %>%
  select(1,2,5) %>%
  slice(-1) %>%
  mutate(Effect = if_else(estimate > 0, "Positive", "Negative")) %>%
  mutate(Significance = if_else(p.value < .01, "Very Significant", if_else(p.value < .05, "Significant", if_else(p.value < .1, "Somewhat Significant", "Not Significant")))) %>%
  select(-2,-3)

#Make Output Table Look Fancy (requires formattable package)
formattable(output_table, list(
  Effect = formatter(
    "span",
    style = x ~ style(color = ifelse(x == "Negative" , "red", "green")),
    x ~ icontext(ifelse(x == "Negative", "arrow-down", "arrow-up"), x)),
  Significance = formatter(
    "span",
    style = x ~ style(color = ifelse(x %in% c("Very Significant","Significant") , "blue", ifelse(x == "Somewhat Significant" , "brown", "black"))))))
```

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:right;">
Predictor
</th>
<th style="text-align:right;">
Effect
</th>
<th style="text-align:right;">
Significance
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
Rainfall
</td>
<td style="text-align:right;">
<span style="color: red"> <i class="glyphicon glyphicon-arrow-down"></i> Negative </span>
</td>
<td style="text-align:right;">
<span style="color: blue">Very Significant</span>
</td>
</tr>
<tr>
<td style="text-align:right;">
DayOfWeekSunday
</td>
<td style="text-align:right;">
<span style="color: green"> <i class="glyphicon glyphicon-arrow-up"></i> Positive </span>
</td>
<td style="text-align:right;">
<span style="color: blue">Very Significant</span>
</td>
</tr>
<tr>
<td style="text-align:right;">
DayOfWeekMonday
</td>
<td style="text-align:right;">
<span style="color: red"> <i class="glyphicon glyphicon-arrow-down"></i> Negative </span>
</td>
<td style="text-align:right;">
<span style="color: black">Not Significant </span>
</td>
</tr>
<tr>
<td style="text-align:right;">
DayOfWeekTuesday
</td>
<td style="text-align:right;">
<span style="color: red"> <i class="glyphicon glyphicon-arrow-down"></i> Negative </span>
</td>
<td style="text-align:right;">
<span style="color: black">Not Significant </span>
</td>
</tr>
<tr>
<td style="text-align:right;">
DayOfWeekThursday
</td>
<td style="text-align:right;">
<span style="color: green"> <i class="glyphicon glyphicon-arrow-up"></i> Positive </span>
</td>
<td style="text-align:right;">
<span style="color: black">Not Significant </span>
</td>
</tr>
<tr>
<td style="text-align:right;">
DayOfWeekFriday
</td>
<td style="text-align:right;">
<span style="color: green"> <i class="glyphicon glyphicon-arrow-up"></i> Positive </span>
</td>
<td style="text-align:right;">
<span style="color: blue">Significant </span>
</td>
</tr>
<tr>
<td style="text-align:right;">
DayOfWeekSaturday
</td>
<td style="text-align:right;">
<span style="color: green"> <i class="glyphicon glyphicon-arrow-up"></i> Positive </span>
</td>
<td style="text-align:right;">
<span style="color: blue">Very Significant</span>
</td>
</tr>
<tr>
<td style="text-align:right;">
SeasonSpring
</td>
<td style="text-align:right;">
<span style="color: red"> <i class="glyphicon glyphicon-arrow-down"></i> Negative </span>
</td>
<td style="text-align:right;">
<span style="color: black">Not Significant </span>
</td>
</tr>
<tr>
<td style="text-align:right;">
SeasonSummer
</td>
<td style="text-align:right;">
<span style="color: green"> <i class="glyphicon glyphicon-arrow-up"></i> Positive </span>
</td>
<td style="text-align:right;">
<span style="color: blue">Very Significant</span>
</td>
</tr>
<tr>
<td style="text-align:right;">
SeasonWinter
</td>
<td style="text-align:right;">
<span style="color: red"> <i class="glyphicon glyphicon-arrow-down"></i> Negative </span>
</td>
<td style="text-align:right;">
<span style="color: blue">Very Significant</span>
</td>
</tr>
</tbody>
</table>
``` r
#Function that takes in values for each of the variables and outputs a prediction for the dependent variable
prediction <- function(valRain = 0, valDay = "Wednesday", valSeas = "Autumn") {

  #Set elements of vect based on inputs 
  vect = c(1,valRain,0,0,0,0,0,0,0,0,0,0,0,0,0)
  vect[3]<-if_else(valDay == "Sunday", 1, 0)
  vect[4]<-if_else(valSeas == "Spring", 1, 0)
  vect[5]<-if_else(valSeas == "Summer", 1, 0)
  vect[6]<-if_else(valSeas == "Winter", 1, 0)
  vect[11]<-if_else(valDay == "Monday", 1, 0)
  vect[12]<-if_else(valDay == "Tuesday", 1, 0)
  vect[13]<-if_else(valDay == "Thursday", 1, 0)
  vect[14]<-if_else(valDay == "Friday", 1, 0)
  vect[15]<-if_else(valDay == "Saturday", 1, 0)
  
  term = c("(Intercept)", "Rainfall","DayOfWeekSunday", "SeasonSpring", "SeasonSummer", "SeasonWinter", "PaymentPlanSubscriber", "RentalAccessPathkeypad" , "RentalAccessPathkeypad_rfid_card", "RentalAccessPathmobile", "DayOfWeekMonday", "DayOfWeekTuesday", "DayOfWeekThursday", "DayOfWeekFriday", "DayOfWeekSaturday")
  
  #Calculate prediction
  formattable(data_frame(term) %>%
  left_join(tidy(m), by = c("term" = "term")) %>%
  select(1,2) %>%
  mutate(values = vect) %>%
  filter(!is.na(estimate)) %>%
  mutate(n1 = estimate*values) %>%
  summarise(prediction = sum(n1)))
}

#An example of how the function works
prediction(valRain = 1, valDay = "Tuesday", valSeas = "Spring")
```

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:right;">
prediction
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
3.237873
</td>
</tr>
</tbody>
</table>
