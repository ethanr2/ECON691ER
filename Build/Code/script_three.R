

rm(list=ls())

library(tidyverse)

load("./Build/Output/census.RData") #from census_data_map script
load("./Build/Output/votes.RData") #from script two

census <- census %>%
  mutate(county = as.data.frame(str_split_fixed(NAME, ",", 2))[,1],
         county = trimws(gsub(" County", "", county)))
head(census)
head(votes)
votes$County[which(votes$County=="DeWitt")]<-"De Witt"
votes$County[which(votes$County=="JoDaviess")]<-"Jo Daviess"
votes$County[which(votes$County=="LaClede")]<-"Laclede"
votes$County[which(votes$County=="LaRue")]<-"Larue"
votes$County[which(votes$County=="St. Louis City")]<-"St. Louis city"
votes$County[which(votes$County=="St. Louis County")]<-"St. Louis"
core <- merge(census, 
              votes, 
              by.x=c("state", "county"), 
              by.y=c("State", "County"),
              all =TRUE)
mod1 <- lm(pctClinton ~ perWhite, data=core)
mod2 <- lm(pctClinton ~ perWhite-1, , data=core)

mod3 <- lm(pctClinton ~ perWhite+perMale+perSameCounty+perSameSt+
             perOthState+perAbroad-1, , data=core)

mod4 <- lm(pctClinton ~ perWhite+perMale+perSameCounty+perSameSt+
             perOthState+perAbroad+factor(state)-1, data=core)

summary(mod4)


library(stargazer)

stargazer(mod1,mod2,mod3,mod4, type = "text", out="./Build/Output/table1")
stargazer(core, summary = TRUE,type = "html", out="./Build/Output/sum.html")

# Graph 

p1 <-ggplot(core, aes(x=perWhite))+
  geom_point(aes(y=pctClinton))+
  xlim(0,1)+
  xlab("Percent Population White")+
  ylim(0,1)+
  ylab("Percent Clinton") +
  geom_line(aes(y=mod1$fitted.values), color ="red")+
  theme_bw()
show(p1)
p2 <-ggplot(mod1, aes(x=seq(1,600)))+
  geom_point(aes(y=mod1$residuals))+
  theme_bw()+
  geom_line(aes(y=0, color="black"))
p2
library(cowplot)

plot_grid(p1,p2)












