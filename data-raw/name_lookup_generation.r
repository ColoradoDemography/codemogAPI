library(dplyr)
library(tidyr)

load("data-raw/county_est.rdata")
load("data-raw/muni_est.rdata")

#Selects County Names from an older data file
county_names=county_est%>%
  filter(year==2014)%>%
  select(countyfips, county)
#Selects Place Names from an older data file and gets rid of "(Total)" for multicounty places.
place_names=muni_est%>%
  filter(year==2014, placefips!=99990)%>%
  select(placefips, municipality)%>%
  mutate(municipality=sapply(strsplit(.$municipality, split='(', fixed=TRUE),function(x) (x[1])))

#removes the old data sets
rm(county_est, muni_est)

#saves the data to R/sysdata.rda
devtools::use_data(county_names, place_names, internal = TRUE)
