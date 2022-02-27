library(tidyverse)
library(janitor)
# 
# Assignment 7
# In this assignment, you will use R (within R-Studio) to:
#   
#   Take a real life data set and wrangle it into shape
# Use a data dictionary to clean and merge two related data sets
# All file paths should be relative, starting from your Assignment_7 directory!!
#   
#   This means that you need to create a new R-Project named “Assignment_7.Rproj” 
#  in your Assignment_7 directory, and work from scripts within that.
# 
# For credit…
# Push a completed version of your Rproj and R-script (details at end of this assignment) 
# to GitHub
# Your score will also depend on whether any files generated in this workflow are found in 
# your repository
# Your tasks:
#   Import the 4 related datasets found in the Data_Course/Data/flights/ directory. There should be:
# airlines.csv
# airports.csv
# jan_flights.csv
# Jan_snowfall.csv

filenames = c('airlines.csv','airports.csv','jan_flights.csv','Jan_snowfall.csv')

airlines <- read_csv(filenames[1]) %>% 
  clean_names() %>% 
  rename(airline_code = iata_code)

airports <- read_csv(filenames[2]) %>% 
  clean_names()

jan_flights <- read_csv(filenames[3]) %>% 
  clean_names() %>% 
  unite(col = 'date', #name of new column
        sep = '-', #put between united columns
        year,month,day) #names of columns to unite

jan_flights$date <- as.Date(jan_flights$date)

jan_snowfall <- read_csv(filenames[4]) %>% 
  clean_names() %>%
  rename(iata_code = iata)

# Combine the data sets appropriately to investigate whether departure delay was correlated with 
# snowfall amount. You will need to think carefully about column names

airlines %>% names
airports %>% names
jan_flights %>% names
jan_snowfall %>% names



depart_delays <- jan_flights %>% 
  filter(weather_delay > 0) %>% 
  select(c(date,departure_delay,origin_airport,weather_delay)) %>% 
  rename(iata_code = origin_airport)%>% 
  full_join(jan_snowfall) %>% 
  filter(snow_precip_cm > 0) %>% 
  full_join(airports)

arrive_delays <- jan_flights %>% 
  filter(weather_delay > 0) %>% 
  select(c(date,departure_delay,destination_airport,weather_delay)) %>% 
  rename(iata_code = destination_airport)%>% 
  full_join(jan_snowfall) %>% 
  filter(snow_precip_cm > 0) %>% 
  full_join(airports)

depart_delays %>% 
  group_by(state) %>% 
  summarise(ave = mean(departure_delay,na.rm=TRUE)) %>% 
  ggplot(aes(x=state,y=ave)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))



range(jan_flights$weather_delay,na.rm = TRUE)

depart_delays %>% 
  ggplot(aes(x = snow_precip_cm, y = departure_delay)) +
  geom_point() +
  geom_smooth(method = 'lm')


# Plot average departure delays by state over time
# Plot average departure delays by airline over time
# Plot effect of snowfall on departure and arrival delays _____________

