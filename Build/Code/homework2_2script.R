#Econ 691 Assignment #2
#Created by: Ethan Rahman
#Created on: 09/01

rm(list=ls())

library(tidyverse)

load(file="./Build/Output/census.RData") #from homework2_1script.R
#Some county names in VOTES are inconsistent with the census names, so we can fix them here.
CENSUS.1 <- CENSUS.1 %>%
  mutate(county = gsub("city", "City", county))

VOTES$County[which(VOTES$County=="Alexandria")]<-"Alexandria City"
VOTES$County[which(VOTES$County=="Bristol")]<-"Bristol City"
VOTES$County[which(VOTES$County=="Buena Vista")]<-"Buena Vista City"
VOTES$County[which(VOTES$County=="Charlottesville")]<-"Charlottesville City"
VOTES$County[which(VOTES$County=="Chesapeake")]<-"Chesapeake City"
VOTES$County[which(VOTES$County=="Colonial Heights")]<-"Colonial Heights City"
VOTES$County[which(VOTES$County=="Covington")] <- "Covington City"
VOTES$County[which(VOTES$County=="Danville")] <- "Danville City"
VOTES$County[which(VOTES$County=="Emporia")] <- "Emporia City"
VOTES$County[which(VOTES$County=="Falls Church")] <- "Falls Church City"
VOTES$County[which(VOTES$County=="Fredericksburg")] <- "Fredericksburg City"
VOTES$County[which(VOTES$County=="Galax")] <- "Galax City"
# Part 2 ------------------------------------------------------------------
non_nums <- c("GEOID", "state", "county", "geometry") #Non-numeric column labels
CENSUS.3 <- CENSUS.1[non_nums]
cols <- CENSUS.1 %>% #Numeric column labels
    select(!c("GEOID", "state", "county", "geometry")) %>% 
    colnames()

CENSUS.3[cols] = CENSUS.2[cols] - CENSUS.1[cols]

core <- merge(CENSUS.3, 
              VOTES, 
              by.x=c("state", "county"), 
              by.y=c("state", "County"),
              all =TRUE)
null_rows <-core[rowSums(is.na(core))>0,]
print(null_rows[,c("county", "pctClinton")])
il.acs<-core %>%
  subset(state=="maryland")
p1<-ggplot(il.acs)+
  geom_sf(aes(fill = pctClinton))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent Clinton"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
show(p1)
