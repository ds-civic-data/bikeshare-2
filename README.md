# bikeshare-2

# Nathan Feldman, Zeki Kazan, Canyon Foot

We are creating a shiny app that will allow users to explore Biketown statistics. Biketown generously supplied us with [complete ride data](https://s3.amazonaws.com/biketown-tripdata-public/index.html) for the entire history of the program. Each ride includes distance, start and stop time, payment method, to which we added variables like rainfall and temperature for the hour of the ride.

Our app attempts to be distinct from the one provided on the Bikeshare [landing page](https://www.biketownpdx.com/system-data). Bikeshare's app mostly attempts to graph the data, like showing trips per weekday, most used stations, or where trips end. The shiny dashboard we have created has several tabs that use statistical models to give insight into the structure of the data.

# Shiny App Structure

The first tab, ____, 

The second tab, ____,

The third tab, ____, tables the variables (aggregated from the data) in a simplistic lm() regression so that one can easily visualize their statistical importance.

The fourth tab, ____, uses a simplistic lm() regression model to make predictions on three variables. User given day of the week, season, and rainfall can predict the number of rides that day, average length of the rides, or average distance travelled per ride.

Timeline of important dates:
* 3/29: Proposal Due
* 4/3: Contact PBOT and Bikeshare
* 4/5: Meet with @kbott
* 5/9: Presentation with Andrew
* 5/10: Presentations