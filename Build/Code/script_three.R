

rm(list=ls())

library(tidyverse)

load("./Build/Output/census.RData") #from census_data_map script
load("./Build/Output/votes.RData") 

census <- census %>%
  mutate(county = str_split_fixed(NAME, ","))