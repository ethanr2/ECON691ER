#This is a script for introducing scripts
#Created by: Ethan Rahman
#Created on: 08/25

rm(list=ls())

library(tidyverse)
library(readr)

#Function####
delta <- function(x){
  temp<-((x - lag(x))/lag(x))
  return(temp)
}


covidIL <- read_csv("Data/ILCovid19.csv", 
                      col_types = cols(Date = col_date(format = "%m/%d/%Y")))
covidIL <- covidIL %>%
  mutate(pc_cases = delta(Cases),
         pc_tests = delta(Tests),
         pc_deaths= delta(Deaths))

plot(covidIL$Date, 
     covidIL$pc_cases,
     main="Percent Cases",
     xlab="",
     ylab="",
     type ="l")

plot <-ggplot() +
  geom_line(data= covidIL, 
            aes(x = Date, 
                y = pc_cases, 
                color = 'cases')) +
  geom_line(data=covidIL,
            aes(x = Date, 
                y = pc_tests, 
                color = 'tests')) +
  geom_line(data=covidIL,
            aes(x = Date, 
                y = pc_deaths, 
                color = 'deaths')) +
  scale_color_manual(values = c('tests' = 'blue',
                                'cases' = 'orange',
                                'deaths' = 'red')) + 
  labs(title = 'Covid19', 
       x = 'Date', 
       y = 'Percentage Change',
       color = 'Time Series') + 
  theme(legend.position = "right")
show(plot)
