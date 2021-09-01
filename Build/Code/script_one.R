#This is a script for introducing scripts
#Created by: Ethan Rahman
#Created on: 08/25

rm(list=ls())

library(tidyverse)
library(readr)


#Function####
DIF <- function(x){
  return(x - lag(x))
}

covidIL <- read_csv("Data/ILCovid19.csv", 
                      col_types = cols(Date = col_date(format = "%m/%d/%Y")))
covidIL <- covidIL %>%
  mutate(new_cases = DIF(Cases),
         new_tests = DIF(Tests),
         new_deaths= DIF(Deaths))
covidIL <- covidIL %>%
  mutate(pc_cases = new_cases/lag(Cases),
         pc_tests = new_tests/lag(Tests),
         pc_deaths= new_deaths/lag(Deaths))
(covidIL)

# plot(covidIL$Date, 
#      covidIL$pc_cases,
#      main="Percent Cases",
#      xlab="",
#      ylab="",
#      type ="l")

plot1 <-ggplot(data= covidIL, 
               aes(x = Date, y = pc_cases, color = 'Red')) +
  geom_line() + 
  labs(title = 'Daily Percentage Change in New Cases',
       x = 'Date',
       y = 'Percentage Change',) + 
  theme(legend.position = "none")
show(plot1)

plot2 <-ggplot(data= covidIL, 
               aes(x = Date, y = pc_tests, color = 'Blue')) +
  geom_line() + 
  labs(title = 'Daily Percentage Change in Tests',
       x = 'Date',
       y = 'Percentage Change') + 
  theme(legend.position = "none")
show(plot2)

plot3 <-ggplot(data= covidIL, 
               aes(x = Date, y = pc_deaths, color = 'orange')) +
  geom_line() + 
  labs(title = 'Daily Percentage Change in Deaths',
       x = 'Date',
       y = 'Percentage Change',
       ) + 
  theme(legend.position = "none")

show(plot3)
