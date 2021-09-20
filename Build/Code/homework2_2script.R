
load(file="./Build/Output/census.RData")

#This part is sloppy but for some reason I can't filter the "geometry" column any other way?
CENSUS.3 <- CENSUS.1[c("GEOID", "state", "county", "geometry")]
(cols <- CENSUS.1 %>%
    select(!c("GEOID", "state", "county", "geometry")) %>% #Notice I'm trying to filter geometry, but it remains in the dataframe??
    colnames())

for(col in cols){
  if(col != "geometry"){
    print(CENSUS.2[col])
    CENSUS.3[col] = CENSUS.2[col] - CENSUS.1[col]
  }
}

CENSUS.1 <- CENSUS.1 %>% select(c(1:12))
colnames(CENSUS.1)
