#######################################################################################################################################
#### Resample_survey_data: get grid
####
#######################################################################################################################################

#### clear environment
rm(list = ls())

#read in from indexwc
#get grid
#setwd("C:/Users/Derek.Bolser/Documents/Resample_survey_data/")
path <- fs::path(getwd(),"Resample-survey-data/data", "california_current_grid.rda")
url <- "https://raw.githubusercontent.com/pfmc-assessments/indexwc/main/data/california_current_grid.rda"

downloader::download(
  url = url,
  destfile = path
)

load(path)