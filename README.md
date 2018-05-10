# bikeshare-2: Modeling and Visualizing Portland's Bike Share Ride Data

# Nathan Feldman, Zeki Kazan, Canyon Foot

We are creating a shiny app that will allow users to explore Biketown statistics. Biketown generously supplied us with [complete ride data](https://s3.amazonaws.com/biketown-tripdata-public/index.html) for the entire history of the program. Each ride includes distance, start and stop time, payment method, to which we added variables like rainfall and temperature for the hour of the ride.

Our app attempts to be distinct from the one provided on the Bikeshare [landing page](https://www.biketownpdx.com/system-data). Bikeshare's app mostly attempts to graph the data, like showing trips per weekday, most used stations, or where trips end. The shiny dashboard we have created has several tabs that use regression models to give insight into the structure of the data.

# Shiny App Structure

The first tab uses the entire dataset, in order to depict where bikes come and go from.

The second tab uses data aggregated at an hourly level, in order to show how the use of the stations changes over time. From the control panel on the right, the model predicts changes in how individual stations are being used. 

The third tab uses data aggregated at the daily level to table the variables in a simplistic lm() regression. This allows one to easily visualize each elements' importance to the model. One can choose to model either the number of rides that day, average length of the rides, or average distance travelled per ride.

The fourth tab also uses data aggregated at the daily level, to create a regression model that makes predictions based on three variables. The user supplies day of the week, season, and rainfall, and the model uses them to predict either the number of rides that day, average length of the rides, or average distance travelled per ride.

# Timeline of important dates:
* 3/29: Proposal Due
* 4/3: Contact PBOT and Bikeshare
* 4/5: Meet with @kbott
* 5/9: Presentation with Andrew
* 5/10: Presentations