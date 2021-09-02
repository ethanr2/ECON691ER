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
delta<- function(x){
  return((x - lag(x))/lag(x))
}

covidIL <- read_csv("Data/ILCovid19.csv", 
                      col_types = cols(Date = col_date(format = "%m/%d/%Y")))
covidIL <- covidIL %>%
  mutate(pc_cases = delta(Cases),
         pc_tests = delta(Tests),
         pc_deaths = delta(Deaths),
         new_cases = DIF(Cases),
         new_tests = DIF(Tests),
         new_deaths= DIF(Deaths),
         dpc_cases = delta(new_cases),
         dpc_tests = delta(new_tests),
         dpc_deaths= delta(new_deaths))
(covidIL)

# plot(covidIL$Date, 
#      covidIL$pc_cases,
#      main="Percent Cases",
#      xlab="",
#      ylab="",
#      type ="l")

plot1 <-ggplot(data= covidIL, 
               aes(x = Date, y = dpc_cases, color = 'Red')) +
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
