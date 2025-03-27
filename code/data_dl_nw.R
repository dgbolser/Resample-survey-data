
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Resample_survey_data: Multiple species, multiple years
## Authors:       Derek Bolser, Office of Science and Technology (derek.bolser@noaa.gov)
## Description:   Resample_survey_data: Multiple species, multiple years.
## Date:          March 2025
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### pull catch data
# Here we list all the packages we will need for this whole process
# We'll also use this in our works cited page. 
PKG <- c(
  
  "devtools", 
  "require",
  "nwfscSurvey", 
  
  # other tidyverse
  "plyr",
  "dplyr",
  "magrittr",
  "tidyr",
  "readr"
)

PKG <- unique(PKG)
for (p in PKG) {
  if(!require(p, character.only = TRUE)) {
    if (p == 'nwfscSurvey') {
      remotes::install_github("pfmc-assessments/nwfscSurvey")
    } else {
      install.packages(p)
    }
    require(p,character.only = TRUE)}
}


# Get cpue data ----------------------------------------------------------------

#define wd
wd <- getwd()
data <-file.path(wd, "Resample-survey-data/data")

# pull BT data for all spp
allcatch <- nwfscSurvey::PullCatch.fn(SurveyName = "NWFSC.Combo") 
# bio<-nwfscSurvey::PullBio.fn(SurveyName = "NWFSC.Combo")

table(allcatch$Common_name)

# filter for the species the survey was stratified for.
species<- c("brown cat shark","Pacific spiny dogfish","Bering skate","longnose skate","Pacific sanddab","arrowtooth flounder","Pacific halibut",
            "petrale sole","English sole","deepsea sole","rex sole","Dover sole","sablefish","Pacific grenadier","giant grenadier",
            "shortspine thornyhead","longspine thornyhead","Pacific ocean perch","darkblotched rockfish","splitnose rockfish",
            "widow rockfish","yellowtail rockfish","chilipepper","shortbelly rockfish","bocaccio","canary rockfish","stripetail rockfish",
            "California slickhead","deepsea smelt","jack mackerel","Pacific herring","Pacific flatnose","lingcod","blacktail snailfish",
            "Pacific hake","white croaker","twoline eelpout","snakehead eelpout","bigfin eelpout","black eelpout")

# FMP species
FMP_species <- c(
  "Big skate",
  "Leopard shark",
  "Longnose skate",
  "Pacific Spiny dogfish",
  "Cabezon",
  "Kelp greenling",
  "Lingcod",
  "Pacific cod",
  "Pacific hake", # in FMP as "Pacific whiting (hake)"
  "Sablefish",
  "Aurora rockfish",
  "Bank rockfish",
  "Black rockfish",
  "Black and yellow rockfish",
  "Blackgill rockfish",
  "Blackspotted rockfish",
  "Blue rockfish",
  "Bocaccio",
  "Bronzespotted rockfish",
  "Brown rockfish",
  "Calico rockfish",
  "California scorpionfish",
  "Canary rockfish",
  "Chameleon rockfish",
  "Chilipepper rockfish",
  "China rockfish",
  "Copper rockfish",
  "Cowcod",
  "Darkblotched rockfish",
  "Deacon rockfish",
  "Dusky rockfish",
  "Dwarf-red rockfish",
  "Flag rockfish",
  "Freckled rockfish",
  "Gopher rockfish",
  "Grass rockfish",
  "Greenblotched rockfish",
  "Greenspotted rockfish",
  "Greenstriped rockfish",
  "Halfbanded rockfish",
  "Harlequin rockfish",
  "Honeycomb rockfish",
  "Kelp rockfish",
  "Longspine thornyhead",
  "Mexican rockfish",
  "Olive rockfish",
  "Pink rockfish",
  "Pinkrose rockfish",
  "Pygmy rockfish",
  "Pacific ocean perch",
  "Quillback rockfish",
  "Redbanded rockfish",
  "Redstripe rockfish",
  "Rosethorn rockfish",
  "Rosy rockfish",
  "Rougheye rockfish",
  "Sharpchin rockfish",
  "Shortraker rockfish",
  "Shortspine thornyhead",
  "Silvergray rockfish",
  "Speckled rockfish",
  "Splitnose rockfish",
  "Squarespot rockfish",
  "Sunset rockfish",
  "Starry rockfish",
  "Stripetail rockfish",
  "Swordspine rockfish",
  "Tiger rockfish",
  "Vermilion rockfish",
  "Widow rockfish",
  "Yelloweye rockfish",
  "Yellowmouth rockfish",
  "Yellowtail rockfish",
  "Arrowtooth flounder", # in FMP as "Arrowtooth flounder (turbot)"
  "Butter sole",
  "Curlfin sole",
  "Dover sole",
  "English sole",
  "Flathead sole",
  "Pacific sanddab",
  "Petrale sole",
  "Rex sole",
  "Rock sole",
  "Sand sole",
  "Starry flounder"
)

# remove non-fmp spp
non_fmp<- species[!tolower(species) %in% tolower(c(FMP_species, "chilipepper"))]

# get final list; remove non-FMP and non-focal spp
final_species<-species[!species %in% non_fmp]

final_species<-final_species[!final_species %in% c("Chilipepper","English sole", "longspine thornyhead", "Pacific hake", "Pacific sanddab",
                                                   "splitnose rockfish", "stripetail rockfish")]

# filter catch
catch<-allcatch[allcatch$Common_name %in% final_species,]

table(catch$Common_name)

test<-species[!species %in% catch$Common_name] #two missing

catch<-unique(catch)

# write csv
setwd(data)
# write.csv(catch,"nwfsc_bt_fmp_spp_updated.csv",row.names = F)


# Get grid ---------------------------------------------------------------------

# clear environment
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

