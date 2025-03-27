#######################################################################################################################################
#### Resample_survey_data: Multiple species, multiple years
####
#######################################################################################################################################

#### clear environment
rm(list = ls())

#### set wds
#setwd("C:/Users/Derek.Bolser/Documents/Resample_survey_data/") #for local testing
wd <- "/home/user/"
basedir<-file.path(wd,'Resample-survey-data')
output <- file.path(wd, "Resample-survey-data/Results")
data <-file.path(wd, "Resample-survey-data/data")
arrowtooth <- file.path(output, "Arrowtooth_flounder")
bocaccio <- file.path(output, "Bocaccio")
canary <- file.path(output, "Canary_rockfish")
darkblotched <- file.path(output, "Darkblotched_rockfish")
dover <- file.path(output, "Dover_sole")
lingcod_n <- file.path(output, "Lingcod_north")
lingcod_s <- file.path(output, "Lingcod_south")
longnose <- file.path(output, "Longnose_skate")
pop <- file.path(output, "Pacific_ocean_perch")
dogfish <- file.path(output, "Pacific_spiny_dogfish")
petrale <- file.path(output, "Petrale_sole")
rex <- file.path(output, "Rex_sole")
sablefish <- file.path(output, "Sablefish")
shortspine <- file.path(output, "Shortspine_thornyhead")
widow <- file.path(output, "Widow_rockfish")
yellowtail <- file.path(output, "Yellowtail_rockfish")

# load packages
library(sampling)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(purrr)
library(readr)
library(tibble)
library(doParallel)
library(sdmTMB)

# read in data
#catch <- read.csv(file.path(data,"nwfsc_bt_fmp_spp.csv"))
catch <- read.csv(file.path(data,"nwfsc_bt_fmp_spp_updated.csv")) #pulled data again to get 2024
load(file.path(data,"california_current_grid.rda"))

source(file.path(basedir, "smaller_functions.R")) #need to edit to specify the code directory if running locally
source(file.path(basedir, "cleanup_by_species.R"))
source(file.path(basedir, "species_sdms.R"))

# set up grid
#rename x and y cols
california_current_grid$Longitude_dd<- california_current_grid$longitude
california_current_grid$Latitude_dd<- california_current_grid$latitude
california_current_grid$Pass<- california_current_grid$pass_scaled
california_current_grid$Depth_m<- california_current_grid$depth

#make gridyrs
grid_yrs <- replicate_df(california_current_grid, "Year", unique(catch$Year))

#### Arrowtooth flounder ##########################################################################################################
arrowtooth_dfs <- cleanup_by_species(df = catch, species = "arrowtooth flounder")
arrowtooth_dfs <- lapply(arrowtooth_dfs, lat_filter_34)
#arrowtooth_dfs <- arrowtooth_dfs[90:91] # reduce DFs for testing

# make the names file
arrowtooth_files <- as.list(names(arrowtooth_dfs))

# Reduce the number of DFs for testing
if (length(arrowtooth_dfs) < 91) {
  stop("Insufficient data frames for the specified range. Check the indices.")
}

# setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1] - 1) # to not overload your computer
registerDoParallel(cl)

setwd(arrowtooth)
# with predefined function
print("Starting parallel SDM processing")
arrowtooth_sdms <- foreach(i = seq_along(arrowtooth_dfs), .combine = "list", .packages = c("foreach", "doParallel", "sdmTMB"), .errorhandling = "remove") %dopar% {
  print(paste("Processing SDM:", i))
  result <- species_sdm_fn(arrowtooth_dfs[[i]], arrowtooth_files[[i]], grid_yrs)
  print(paste("Result for SDM", i, ":", result))
  result
}
stopCluster(cl)
print("Parallel SDM processing complete")

##### read in fit files
arrowtooth_sdms <- pull_files(arrowtooth, "fit")

# Extract outputs and save to csvs
arrowtooth_fits <- lapply(arrowtooth_sdms, fit_df_fn)
arrowtooth_fit_df <- bind_fn(arrowtooth_fits)
write.csv(arrowtooth_fit_df, "arrowtooth_fit_df.csv", row.names = F)

arrowtooth_pars <- lapply(arrowtooth_sdms, fit_pars_fn)
arrowtooth_pars_df <- bind_fn(arrowtooth_pars)
write.csv(arrowtooth_pars_df, "arrowtooth_pars_df.csv", row.names = F)

arrowtooth_fit_check <- lapply(arrowtooth_sdms, fit_check_fn)
arrowtooth_fit_check_df <- bind_fit_check(arrowtooth_fit_check)
write.csv(arrowtooth_fit_check_df, "arrowtooth_fit_check_df.csv", row.names = F)

# Remove some files to help with memory
rm(
  "arrowtooth_fits", "arrowtooth_fit_df", "arrowtooth_pars",
  "arrowtooth_pars_df", "arrowtooth_fit_check", "arrowtooth_fit_check_df"
)

##### read in index files
arrowtooth_indices <- pull_files(arrowtooth, "index")
arrowtooth_indices_df <- bind_index_fn(arrowtooth_indices)
write.csv(arrowtooth_indices_df, "arrowtooth_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("arrowtooth_files", "arrowtooth_indices", "arrowtooth_indices_df")

#### Bocaccio #############################################################################################################
setwd(bocaccio)
bocaccio_dfs <- cleanup_by_species(df = catch, species = "bocaccio")
bocaccio_dfs <- lapply(bocaccio_dfs, depth_filter_500)
bocaccio_dfs <- bocaccio_dfs[90:91] # reduce DFs for testing
# make the names file
bocaccio_files <- as.list(names(bocaccio_dfs))

# setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1] - 1) # to not overload your computer
registerDoParallel(cl)

setwd(bocaccio)
# with predefined function
print("Starting parallel SDM processing")
bocaccio_sdms <- foreach(i = seq_along(bocaccio_dfs), .combine = "list", .packages = c("foreach", "doParallel", "sdmTMB"), .errorhandling = "remove") %dopar% {
  print(paste("Processing SDM:", i))
  result <- species_sdm_fn(bocaccio_dfs[[i]], bocaccio_files[[i]], grid_yrs)
  print(paste("Result for SDM", i, ":", result))
  result
}
stopCluster(cl)
print("Parallel SDM processing complete")

##### read in .rds if already fit
bocaccio_sdms <- pull_files(bocaccio, "fit")

# extract outputs
bocaccio_fits <- lapply(bocaccio_sdms, fit_df_fn)
bocaccio_fit_df <- bind_fn(bocaccio_fits)
write.csv(bocaccio_fit_df, "bocaccio_fit_df.csv", row.names = F)

bocaccio_pars <- lapply(bocaccio_sdms, fit_pars_fn)
bocaccio_pars_df <- bind_fn(bocaccio_pars)
write.csv(bocaccio_pars_df, "bocaccio_pars_df.csv", row.names = F)

bocaccio_fit_check <- lapply(bocaccio_sdms, fit_check_fn)
bocaccio_fit_check_df <- bind_fit_check(bocaccio_fit_check)
write.csv(bocaccio_fit_check_df, "bocaccio_fit_check_df.csv", row.names = F)

# Remove some files to help with memory
rm(
  "bocaccio_fits", "bocaccio_fit_df", "bocaccio_pars", "bocaccio_pars_df",
  "bocaccio_fit_check", "bocaccio_fit_check_df"
)

##### read in index files
bocaccio_indices <- pull_files(bocaccio, "index")
bocaccio_indices_df <- bind_index_fn(bocaccio_indices)
write.csv(bocaccio_indices_df, "bocaccio_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("bocaccio_dfs", "bocaccio_files", "bocaccio_sdms", "bocaccio_indices", "bocaccio_indices_df")

#### Canary rockfish ##########################################################################################################
setwd(canary)
canary_dfs <- cleanup_by_species(df = catch, species = "canary rockfish")
canary_dfs <- lapply(canary_dfs, depth_filter_275)
# canary_dfs <- canary_dfs[90:91] # reduce DFs for testing
# make the names file
canary_files <- as.list(names(canary_dfs))

# setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1] - 1) # to not overload your computer
registerDoParallel(cl)

setwd(canary)
# with predefined function
print("Starting parallel SDM processing")
canary_sdms <- foreach(i = seq_along(canary_dfs), .combine = "list", .packages = c("foreach", "doParallel", "sdmTMB"), .errorhandling = "remove") %dopar% {
  print(paste("Processing SDM:", i))
  result <- canary_sdm_fn(canary_dfs[[i]], canary_files[[i]], grid_yrs)
  print(paste("Result for SDM", i, ":", result))
  result
}
stopCluster(cl)
print("Parallel SDM processing complete")

##### read in .rds if already fit
canary_sdms <- pull_files(canary, "fit")

# extract outputs
canary_fits <- lapply(canary_sdms, fit_df_fn)
canary_fit_df <- bind_fn(canary_fits)
write.csv(canary_fit_df, "canary_fit_df.csv", row.names = F)

canary_pars <- lapply(canary_sdms, fit_pars_fn)
canary_pars_df <- bind_fn(canary_pars)
write.csv(canary_pars_df, "canary_pars_df.csv", row.names = F)

canary_fit_check <- lapply(canary_sdms, fit_check_fn)
canary_fit_check_df <- bind_fit_check(canary_fit_check)
write.csv(canary_fit_check_df, "canary_fit_check_df.csv", row.names = F)

# Remove some files to help with memory
rm(
  "canary_fits", "canary_fit_df", "canary_pars", "canary_pars_df",
  "canary_fit_check", "canary_fit_check_df"
)

##### read in .rds if already fit
canary_indices <- pull_files(canary, "index")
canary_indices_df <- bind_index_fn(canary_indices)
write.csv(canary_indices_df, "canary_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("canary_dfs", "canary_files", "canary_sdms", "canary_indices", "canary_indices_df")

#### Darkblotched rockfish ####################################################################################################
setwd(darkblotched)
darkblotched_dfs <- cleanup_by_species(df = catch, species = "darkblotched rockfish")
darkblotched_dfs <- lapply(darkblotched_dfs, lat_filter_335)
darkblotched_dfs <- lapply(darkblotched_dfs, depth_filter_675)
# darkblotched_dfs <- darkblotched_dfs[90:91] # reduce DFs for testing
# make the names file
darkblotched_files <- as.list(names(darkblotched_dfs))

# setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1] - 1) # to not overload your computer
registerDoParallel(cl)

setwd(darkblotched)
# with predefined function
print("Starting parallel SDM processing")
darkblotched_sdms <- foreach(i = seq_along(darkblotched_dfs), .combine = "list", .packages = c("foreach", "doParallel", "sdmTMB"), .errorhandling = "remove") %dopar% {
  print(paste("Processing SDM:", i))
  result <- darkblotched_sdm_fn(darkblotched_dfs[[i]], darkblotched_files[[i]], grid_yrs)
  print(paste("Result for SDM", i, ":", result))
  result
}
stopCluster(cl)
print("Parallel SDM processing complete")

##### read in .rds if already fit
darkblotched_sdms <- pull_files(darkblotched, "fit")

# extract outputs
darkblotched_fits <- lapply(darkblotched_sdms, fit_df_fn)
darkblotched_fit_df <- bind_fn(darkblotched_fits)
write.csv(darkblotched_fit_df, "darkblotched_fit_df.csv", row.names = F)

darkblotched_pars <- lapply(darkblotched_sdms, fit_pars_fn)
darkblotched_pars_df <- bind_fn(darkblotched_pars)
write.csv(darkblotched_pars_df, "darkblotched_pars_df.csv", row.names = F)

darkblotched_fit_check <- lapply(darkblotched_sdms, fit_check_fn)
darkblotched_fit_check_df <- bind_fit_check(darkblotched_fit_check)
write.csv(darkblotched_fit_check_df, "darkblotched_fit_check_df.csv", row.names = F)

# Remove some files to help with memory
rm(
  "darkblotched_fits", "darkblotched_fit_df", "darkblotched_pars",
  "darkblotched_pars_df", "darkblotched_fit_check", "darkblotched_fit_check_df"
)

##### read in .rds if already fit
darkblotched_indices <- pull_files(darkblotched, "index")
darkblotched_indices_df <- bind_index_fn(darkblotched_indices)
write.csv(darkblotched_indices_df, "darkblotched_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("darkblotched_dfs", "darkblotched_files", "darkblotched_sdms", "darkblotched_indices", "darkblotched_indices_df")

#### Dover sole ###############################################################################################################
setwd(dover)
dover_dfs <- cleanup_by_species(df = catch, species = "Dover sole")
# dover_dfs <- dover_dfs[90:91] # reduce DFs for testing
# make the names file
dover_files <- as.list(names(dover_dfs))

# setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1] - 1) # to not overload your computer
registerDoParallel(cl)

setwd(dover)
# with predefined function
print("Starting parallel SDM processing")
dover_sdms <- foreach(i = seq_along(dover_dfs), .combine = "list", .packages = c("foreach", "doParallel", "sdmTMB"), .errorhandling = "remove") %dopar% {
  print(paste("Processing SDM:", i))
  result <- species_sdm_fn(dover_dfs[[i]], dover_files[[i]], grid_yrs)
  print(paste("Result for SDM", i, ":", result))
  result
}
stopCluster(cl)
print("Parallel SDM processing complete")

##### read in .rds if already fit
dover_sdms <- pull_files(dover, "fit")

# extract outputs
dover_fits <- lapply(dover_sdms, fit_df_fn)
dover_fit_df <- bind_fn(dover_fits)
write.csv(dover_fit_df, "dover_fit_df.csv", row.names = F)

dover_pars <- lapply(dover_sdms, fit_pars_fn)
dover_pars_df <- bind_fn(dover_pars)
write.csv(dover_pars_df, "dover_pars_df.csv", row.names = F)

dover_fit_check <- lapply(dover_sdms, fit_check_fn)
dover_fit_check_df <- bind_fit_check(dover_fit_check)
write.csv(dover_fit_check_df, "dover_fit_check_df.csv", row.names = F)

# Remove some files to help with memory
rm(
  "dover_fits", "dover_fit_df", "dover_pars",
  "dover_pars_df", "dover_fit_check", "dover_fit_check_df"
)

##### read in .rds if already fit
dover_indices <- pull_files(dover, "index")
dover_indices_df <- bind_index_fn(dover_indices)
write.csv(dover_indices_df, "dover_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("dover_dfs", "dover_files", "dover_sdms", "dover_indices", "dover_indices_df")

#### Lingcod North ############################################################################################################
setwd(lingcod_n)
lingcod_n_dfs <- cleanup_by_species(df = catch, species = "lingcod")
lingcod_n_dfs <- lapply(lingcod_n_dfs, lat_filter_34)
lingcod_n_dfs <- lapply(lingcod_n_dfs, depth_filter_450)
# lingcod_n_dfs <- gsub("lingcod", "lingcod_n", lingcod_n_dfs)
# lingcod_n_dfs <- lingcod_n_dfs[90:91] # reduce DFs for testing
# make the names file
lingcod_n_files <- as.list(names(lingcod_n_dfs))

# setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1] - 1) # to not overload your computer
registerDoParallel(cl)

setwd(lingcod_n)
# with predefined function
print("Starting parallel SDM processing")
lingcod_n_sdms <- foreach(i = seq_along(lingcod_n_dfs), .combine = "list", .packages = c("foreach", "doParallel", "sdmTMB"), .errorhandling = "remove") %dopar% {
  print(paste("Processing SDM:", i))
  result <- species_sdm_fn(lingcod_n_dfs[[i]], lingcod_n_files[[i]], grid_yrs)
  print(paste("Result for SDM", i, ":", result))
  result
}
stopCluster(cl)
print("Parallel SDM processing complete")

##### read in .rds if already fit
lingcod_n_sdms <- pull_files(lingcod_n, "fit")

# extract outputs
lingcod_n_fits <- lapply(lingcod_n_sdms, fit_df_fn)
lingcod_n_fit_df <- bind_fn(lingcod_n_fits)
write.csv(lingcod_n_fit_df, "lingcod_n_fit_df.csv", row.names = F)

lingcod_n_pars <- lapply(lingcod_n_sdms, fit_pars_fn)
lingcod_n_pars_df <- bind_fn(lingcod_n_pars)
write.csv(lingcod_n_pars_df, "lingcod_n_pars_df.csv", row.names = F)

lingcod_n_fit_check <- lapply(lingcod_n_sdms, fit_check_fn)
lingcod_n_fit_check_df <- bind_fit_check(lingcod_n_fit_check)
write.csv(lingcod_n_fit_check_df, "lingcod_n_fit_check_df.csv", row.names = F)

# Remove some files to help with memory
rm(
  "lingcod_n_fits", "lingcod_n_fit_df", "lingcod_n_pars",
  "lingcod_n_pars_df", "lingcod_n_fit_check", "lingcod_n_fit_check_df"
)

##### read in .rds if already fit
lingcod_n_indices <- pull_files(lingcod_n, "index")
lingcod_n_indices_df <- bind_index_fn(lingcod_n_indices)
write.csv(lingcod_n_indices_df, "lingcod_n_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("lingcod_n_dfs", "lingcod_n_files", "lingcod_n_sdms", "lingcod_n_indices", "lingcod_n_indices_df")

#### Lingcod South ###############################################################################################
setwd(lingcod_s)
lingcod_s_dfs <- cleanup_by_species(df = catch, species = "lingcod")
lingcod_s_dfs <- lapply(lingcod_s_dfs, lat_filter_34_max)
lingcod_s_dfs <- lapply(lingcod_s_dfs, depth_filter_450)
# lingcod_s_dfs <- lingcod_s_dfs[90:91] # reduce DFs for testing
# make the names file
#lingcod_s_dfs <- gsub("lingcod", "lingcod_s", lingcod_s_dfs)
lingcod_s_files <- as.list(names(lingcod_s_dfs))

# setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1] - 1) # to not overload your computer
registerDoParallel(cl)

setwd(lingcod_s)
# with predefined function
print("Starting parallel SDM processing")
lingcod_s_sdms <- foreach(i = seq_along(lingcod_s_dfs), .combine = "list", .packages = c("foreach", "doParallel", "sdmTMB"), .errorhandling = "remove") %dopar% {
  print(paste("Processing SDM:", i))
  result <- species_sdm_fn(lingcod_s_dfs[[i]], lingcod_s_files[[i]], grid_yrs)
  print(paste("Result for SDM", i, ":", result))
  result
}
stopCluster(cl)
print("Parallel SDM processing complete")

##### read in .rds if already fit
lingcod_s_sdms <- pull_files(lingcod_s, "fit")

# extract outputs
lingcod_s_fits <- lapply(lingcod_s_sdms, fit_df_fn)
lingcod_s_fit_df <- bind_fn(lingcod_s_fits)
write.csv(lingcod_s_fit_df, "lingcod_s_fit_df.csv", row.names = F)

lingcod_s_pars <- lapply(lingcod_s_sdms, fit_pars_fn)
lingcod_s_pars_df <- bind_fn(lingcod_s_pars)
write.csv(lingcod_s_pars_df, "lingcod_s_pars_df.csv", row.names = F)

lingcod_s_fit_check <- lapply(lingcod_s_sdms, fit_check_fn)
lingcod_s_fit_check_df <- bind_fit_check(lingcod_s_fit_check)
write.csv(lingcod_s_fit_check_df, "lingcod_s_fit_check_df.csv", row.names = F)

# Remove some files to help with memory
rm(
  "lingcod_s_fits", "lingcod_s_fit_df", "lingcod_s_pars",
  "lingcod_s_pars_df", "lingcod_s_fit_check", "lingcod_s_fit_check_df"
)

##### read in .rds if already fit
lingcod_s_indices <- pull_files(lingcod_s, "index")
lingcod_s_indices_df <- bind_index_fn(lingcod_s_indices)
write.csv(lingcod_s_indices_df, "lingcod_s_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("lingcod_s_dfs", "lingcod_s_files", "lingcod_s_sdms", "lingcod_s_indices", "lingcod_s_indices_df")

#### Longnose Skate ###########################################################################################################
setwd(longnose)
longnose_dfs <- cleanup_by_species(df = catch, species = "longnose skate")
# longnose_dfs <- longnose_dfs[90:91] # reduce DFs for testing
# make the names file
longnose_files <- as.list(names(longnose_dfs))

# setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1] - 1) # to not overload your computer
registerDoParallel(cl)

setwd(longnose)
# with predefined function
print("Starting parallel SDM processing")
longnose_sdms <- foreach(i = seq_along(longnose_dfs), .combine = "list", .packages = c("foreach", "doParallel", "sdmTMB"), .errorhandling = "remove") %dopar% {
  print(paste("Processing SDM:", i))
  result <- species_sdm_fn(longnose_dfs[[i]], longnose_files[[i]], grid_yrs)
  print(paste("Result for SDM", i, ":", result))
  result
}
stopCluster(cl)
print("Parallel SDM processing complete")

##### read in .rds if already fit
longnose_sdms <- pull_files(longnose, "fit")

# extract outputs
longnose_fits <- lapply(longnose_sdms, fit_df_fn)
longnose_fit_df <- bind_fn(longnose_fits)
write.csv(longnose_fit_df, "longnose_fit_df.csv", row.names = F)

longnose_pars <- lapply(longnose_sdms, fit_pars_fn)
longnose_pars_df <- bind_fn(longnose_pars)
write.csv(longnose_pars_df, "longnose_pars_df.csv", row.names = F)

longnose_fit_check <- lapply(longnose_sdms, fit_check_fn)
longnose_fit_check_df <- bind_fit_check(longnose_fit_check)
write.csv(longnose_fit_check_df, "longnose_fit_check_df.csv", row.names = F)

# Remove some files to help with memory
rm(
  "longnose_fits", "longnose_fit_df", "longnose_pars",
  "longnose_pars_df", "longnose_fit_check", "longnose_fit_check_df"
)

##### read in .rds if already fit
longnose_indices <- pull_files(longnose, "index")
longnose_indices_df <- bind_index_fn(longnose_indices)
write.csv(longnose_indices_df, "longnose_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("longnose_dfs", "longnose_files", "longnose_sdms", "longnose_indices", "longnose_indices_df")

#### Pacific ocean perch ######################################################################################################
setwd(pop)
pop_dfs <- cleanup_by_species(df = catch, species = "Pacific ocean perch")
pop_dfs <- lapply(pop_dfs, lat_filter_35)
pop_dfs <- lapply(pop_dfs, depth_filter_500)
# pop_dfs <- pop_dfs[90:91] # reduce DFs for testing
# make the names file
pop_files <- as.list(names(pop_dfs))

# setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1] - 1) # to not overload your computer
registerDoParallel(cl)

setwd(pop)
# with predefined function
print("Starting parallel SDM processing")
pop_sdms <- foreach(i = seq_along(pop_dfs), .combine = "list", .packages = c("foreach", "doParallel", "sdmTMB"), .errorhandling = "remove") %dopar% {
  print(paste("Processing SDM:", i))
  result <- species_sdm_fn(pop_dfs[[i]], pop_files[[i]], grid_yrs)
  print(paste("Result for SDM", i, ":", result))
  result
}
stopCluster(cl)
print("Parallel SDM processing complete")

##### read in .rds if already fit
pop_sdms <- pull_files(pop, "fit")

# extract outputs
pop_fits <- lapply(pop_sdms, fit_df_fn)
pop_fit_df <- bind_fn(pop_fits)
write.csv(pop_fit_df, "pop_fit_df.csv", row.names = F)

pop_pars <- lapply(pop_sdms, fit_pars_fn)
pop_pars_df <- bind_fn(pop_pars)
write.csv(pop_pars_df, "pop_pars_df.csv", row.names = F)

pop_fit_check <- lapply(pop_sdms, fit_check_fn)
pop_fit_check_df <- bind_fit_check(pop_fit_check)
write.csv(pop_fit_check_df, "pop_fit_check_df.csv", row.names = F)

# Remove some files to help with memory
rm(
  "pop_fits", "pop_fit_df", "pop_pars", "pop_pars_df", "pop_fit_check",
  "pop_fit_check_df"
)

##### read in .rds if already fit
pop_indices <- pull_files(pop, "index")
pop_indices_df <- bind_index_fn(pop_indices)
write.csv(pop_indices_df, "pop_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("pop_dfs", "pop_files", "pop_sdms", "pop_indices", "pop_indices_df")

#### Pacific spiny dogfish ########################################################################################################
setwd(dogfish)
dogfish_dfs <- cleanup_by_species(df = catch, species = "Pacific spiny dogfish")
dogfish_dfs <- lapply(dogfish_dfs, depth_filter_700)
# dogfish_dfs <- dogfish_dfs[90:91] # reduce DFs for testing
# make the names file
dogfish_files <- as.list(names(dogfish_dfs))

# setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1] - 1) # to not overload your computer
registerDoParallel(cl)

setwd(dogfish)
# with predefined function
print("Starting parallel SDM processing")
dogfish_sdms <- foreach(i = seq_along(dogfish_dfs), .combine = "list", .packages = c("foreach", "doParallel", "sdmTMB"), .errorhandling = "remove") %dopar% {
  print(paste("Processing SDM:", i))
  result <- species_sdm_fn(dogfish_dfs[[i]], dogfish_files[[i]], grid_yrs)
  print(paste("Result for SDM", i, ":", result))
  result
}
stopCluster(cl)
print("Parallel SDM processing complete")

##### read in .rds if already fit
dogfish_sdms <- pull_files(dogfish, "fit")

# extract outputs
dogfish_fits <- lapply(dogfish_sdms, fit_df_fn)
dogfish_fit_df <- bind_fn(dogfish_fits)
write.csv(dogfish_fit_df, "dogfish_fit_df.csv", row.names = F)

dogfish_pars <- lapply(dogfish_sdms, fit_pars_fn)
dogfish_pars_df <- bind_fn(dogfish_pars)
write.csv(dogfish_pars_df, "dogfish_pars_df.csv", row.names = F)

dogfish_fit_check <- lapply(dogfish_sdms, fit_check_fn)
dogfish_fit_check_df <- bind_fit_check(dogfish_fit_check)
write.csv(dogfish_fit_check_df, "dogfish_fit_check_df.csv", row.names = F)

# Remove some files to help with memory
rm(
  "dogfish_fits", "dogfish_fit_df", "dogfish_pars", "dogfish_pars_df",
  "dogfish_fit_check", "dogfish_fit_check_df"
)

##### read in .rds if already fit
dogfish_indices <- pull_files(dogfish, "index")
dogfish_indices_df <- bind_index_fn(dogfish_indices)
write.csv(dogfish_indices_df, "dogfish_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("dogfish_dfs", "dogfish_files", "dogfish_sdms", "dogfish_indices", "dogfish_indices_df")

#### Petrale sole #############################################################################################################
setwd(petrale)
petrale_dfs <- cleanup_by_species(df = catch, species = "petrale sole")
petrale_dfs <- lapply(petrale_dfs, depth_filter_675)
# petrale_dfs <- petrale_dfs[90:91] # reduce DFs for testing
# make the names file
petrale_files <- as.list(names(petrale_dfs))

# setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1] - 1) # to not overload your computer
registerDoParallel(cl)

setwd(petrale)
# with predefined function
print("Starting parallel SDM processing")
petrale_sdms <- foreach(i = seq_along(petrale_dfs), .combine = "list", .packages = c("foreach", "doParallel", "sdmTMB"), .errorhandling = "remove") %dopar% {
  print(paste("Processing SDM:", i))
  result <- species_sdm_lognormal_fn(petrale_dfs[[i]], petrale_files[[i]], grid_yrs)
  print(paste("Result for SDM", i, ":", result))
  result
}
stopCluster(cl)
print("Parallel SDM processing complete")

##### read in .rds if already fit
petrale_sdms <- pull_files(petrale, "fit")

# extract outputs
petrale_fits <- lapply(petrale_sdms, fit_df_fn)
petrale_fit_df <- bind_fn(petrale_fits)
write.csv(petrale_fit_df, "petrale_fit_df.csv", row.names = F)

petrale_pars <- lapply(petrale_sdms, fit_pars_fn)
petrale_pars_df <- bind_fn(petrale_pars)
write.csv(petrale_pars_df, "petrale_pars_df.csv", row.names = F)

petrale_fit_check <- lapply(petrale_sdms, fit_check_fn)
petrale_fit_check_df <- bind_fit_check(petrale_fit_check)
write.csv(petrale_fit_check_df, "petrale_fit_check_df.csv", row.names = F)

# Remove some files to help with memory
rm(
  "petrale_fits","petrale_fit_df", "petrale_pars", "petrale_pars_df",
  "petrale_fit_check", "petrale_fit_check_df"
)

##### read in .rds if already fit
petrale_indices <- pull_files(petrale, "index")
petrale_indices_df <- bind_index_fn(petrale_indices)
write.csv(petrale_indices_df, "petrale_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("petrale_dfs", "petrale_files", "petrale_sdms", "petrale_indices", "petrale_indices_df")

#### Rex sole #################################################################################################################
setwd(rex)
rex_dfs <- cleanup_by_species(df = catch, species = "rex sole")
rex_dfs <- lapply(rex_dfs, depth_filter_700)
# rex_dfs <- rex_dfs[90:91] # reduce DFs for testing
# make the names file
rex_files <- as.list(names(rex_dfs))

# setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1] - 1) # to not overload your computer
registerDoParallel(cl)

setwd(rex)
# with predefined function
print("Starting parallel SDM processing")
rex_sdms <- foreach(i = seq_along(rex_dfs), .combine = "list", .packages = c("foreach", "doParallel", "sdmTMB"), .errorhandling = "remove") %dopar% {
  print(paste("Processing SDM:", i))
  result <- species_sdm_fn(rex_dfs[[i]], rex_files[[i]], grid_yrs)
  print(paste("Result for SDM", i, ":", result))
  result
}
stopCluster(cl)
print("Parallel SDM processing complete")

##### read in .rds if already fit
rex_sdms <- pull_files(rex, "fit")

# extract outputs
rex_fits <- lapply(rex_sdms, fit_df_fn)
rex_fit_df <- bind_fn(rex_fits)
write.csv(rex_fit_df, "rex_fit_df.csv", row.names = F)

rex_pars <- lapply(rex_sdms, fit_pars_fn)
rex_pars_df <- bind_fn(rex_pars)
write.csv(rex_pars_df, "rex_pars_df.csv", row.names = F)

rex_fit_check <- lapply(rex_sdms, fit_check_fn)
rex_fit_check_df <- bind_fit_check(rex_fit_check)
write.csv(rex_fit_check_df, "rex_fit_check_df.csv", row.names = F)

# Remove some files to help with memory
rm(
  "rex_fits", "rex_fit_df", "rex_pars", "rex_pars_df", "rex_fit_check",
  "rex_fit_check_df"
)

##### read in .rds if already fit
rex_indices <- pull_files(rex, "index")
rex_indices_df <- bind_index_fn(rex_indices)
write.csv(rex_indices_df, "rex_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("rex_dfs", "rex_files", "rex_sdms", "rex_indices", "rex_indices_df")

#### Sablefish ##################################################################################################################
setwd(sablefish)
sablefish_dfs <- cleanup_by_species(df = catch, species = "sablefish")
# sablefish_dfs <- sablefish_dfs[90:91] # reduce DFs for testing
# make the names file
sablefish_files <- as.list(names(sablefish_dfs))

# setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1] - 1) # to not overload your computer
registerDoParallel(cl)

setwd(sablefish)
# with predefined function
print("Starting parallel SDM processing")
sablefish_sdms <- foreach(i = seq_along(sablefish_dfs), .combine = "list", .packages = c("foreach", "doParallel", "sdmTMB"), .errorhandling = "remove") %dopar% {
  print(paste("Processing SDM:", i))
  result <- species_sdm_lognormal_fn(sablefish_dfs[[i]], sablefish_files[[i]], grid_yrs)
  print(paste("Result for SDM", i, ":", result))
  result
}
stopCluster(cl)
print("Parallel SDM processing complete")

##### read in .rds if already fit
sablefish_sdms <- pull_files(sablefish, "fit")

# extract outputs
sablefish_fits <- lapply(sablefish_sdms, fit_df_fn)
sablefish_fit_df <- bind_fn(sablefish_fits)
write.csv(sablefish_fit_df, "sablefish_fit_df.csv", row.names = F)

sablefish_pars <- lapply(sablefish_sdms, fit_pars_fn)
sablefish_pars_df <- bind_fn(sablefish_pars)
write.csv(sablefish_pars_df, "sablefish_pars_df.csv", row.names = F)

sablefish_fit_check <- lapply(sablefish_sdms, fit_check_fn)
sablefish_fit_check_df <- bind_fit_check(sablefish_fit_check)
write.csv(sablefish_fit_check_df, "sablefish_fit_check_df.csv", row.names = F)

# Remove some files to help with memory
rm(
  "sablefish_fits", "sablefish_fit_df", "sablefish_pars",
  "sablefish_pars_df", "sablefish_fit_check", "sablefish_fit_check_df"
)

##### read in .rds if already fit
sablefish_indices <- pull_files(sablefish, "index")
sablefish_indices_df <- bind_index_fn(sablefish_indices)
write.csv(sablefish_indices_df, "sablefish_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("sablefish_dfs", "sablefish_files", "sablefish_sdms", "sablefish_indices", "sablefish_indices_df")

#### Shortspine thornyhead ####################################################################################################
# This took over 7.5 hours to run and still didn't finish
setwd(shortspine)
shortspine_dfs <- cleanup_by_species(df = catch, species = "shortspine thornyhead")
# shortspine_dfs <- shortspine_dfs[90:91] # reduce DFs for testing
# make the names file
shortspine_files <- as.list(names(shortspine_dfs))

# setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1] - 1) # to not overload your computer
registerDoParallel(cl)

setwd(shortspine)
# with predefined function
print("Starting parallel SDM processing")
shortspine_sdms <- foreach(i = seq_along(shortspine_dfs), .combine = "list", .packages = c("foreach", "doParallel", "sdmTMB"), .errorhandling = "remove") %dopar% {
  print(paste("Processing SDM:", i))
  result <- shortspine_sdm_fn(shortspine_dfs[[i]], shortspine_files[[i]], grid_yrs)
  print(paste("Result for SDM", i, ":", result))
  result
}
stopCluster(cl)
print("Parallel SDM processing complete")

##### read in .rds if already fit
shortspine_sdms <- pull_files(shortspine, "fit")

# extract outputs
shortspine_fits <- lapply(shortspine_sdms, fit_df_fn)
shortspine_fit_df <- bind_fn(shortspine_fits)
write.csv(shortspine_fit_df, "shortspine_fit_df.csv", row.names = F)

shortspine_pars <- lapply(shortspine_sdms, fit_pars_fn)
shortspine_pars_df <- bind_fn(shortspine_pars)
write.csv(shortspine_pars_df, "shortspine_pars_df.csv", row.names = F)

shortspine_fit_check <- lapply(shortspine_sdms, fit_check_fn)
shortspine_fit_check_df <- bind_fit_check(shortspine_fit_check)
write.csv(shortspine_fit_check_df, "shortspine_fit_check_df.csv", row.names = F)

# Remove some files to help with memory
rm(
  "shortspine_fits", "shortspine_fit_df", "shortspine_pars",
  "shortspine_pars_df", "shortspine_fit_check", "shortspine_fit_check_df"
)
##### read in .rds if already fit
shortspine_indices <- pull_files(shortspine, "index")
shortspine_indices_df <- bind_index_fn(shortspine_indices)
write.csv(shortspine_indices_df, "shortspine_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("shortspine_dfs", "shortspine_files", "shortspine_sdms", "shortspine_indices", "shortspine_indices_df")

#### Widow rockfish ###########################################################################################################
setwd(widow)
widow_dfs <- cleanup_by_species(df = catch, species = "widow rockfish")
widow_dfs <- lapply(widow_dfs, lat_filter_335)
widow_dfs <- lapply(widow_dfs, depth_filter_675)
# widow_dfs <- widow_dfs[90:91] # reduce DFs for testing
# make the names file
widow_files <- as.list(names(widow_dfs))

# setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1] - 1) # to not overload your computer
registerDoParallel(cl)

setwd(widow)
# with predefined function
print("Starting parallel SDM processing")
widow_sdms <- foreach(i = seq_along(widow_dfs), .combine = "list", .packages = c("foreach", "doParallel", "sdmTMB"), .errorhandling = "remove") %dopar% {
  print(paste("Processing SDM:", i))
  result <- widow_sdm_fn(widow_dfs[[i]], widow_files[[i]], grid_yrs)
  print(paste("Result for SDM", i, ":", result))
  result
}
stopCluster(cl)
print("Parallel SDM processing complete")

##### read in .rds if already fit
widow_sdms <- pull_files(widow, "fit")

# extract outputs
widow_fits <- lapply(widow_sdms, fit_df_fn)
widow_fit_df <- bind_fn(widow_fits)
write.csv(widow_fit_df, "widow_fit_df.csv", row.names = F)

widow_pars <- lapply(widow_sdms, fit_pars_fn)
widow_pars_df <- bind_fn(widow_pars)
write.csv(widow_pars_df, "widow_pars_df.csv", row.names = F)

widow_fit_check <- lapply(widow_sdms, fit_check_fn)
widow_fit_check_df <- bind_fit_check(widow_fit_check)
write.csv(widow_fit_check_df, "widow_fit_check_df.csv", row.names = F)

# Remove some files to help with memory
rm(
  "widow_fits", "widow_fit_df", "widow_pars", "widow_pars_df",
  "widow_fit_check", "widow_fit_check_df"
)

##### read in .rds if already fit
widow_indices <- pull_files(widow, "index")
widow_indices_df <- bind_index_fn(widow_indices)
write.csv(widow_indices_df, "widow_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("widow_dfs", "widow_files", "widow_sdms", "widow_indices", "widow_indices_df")

#### Yellowtail rockfish ######################################################################################################
setwd(yellowtail)
yellowtail_dfs <- cleanup_by_species(df = catch, species = "yellowtail rockfish")
yellowtail_dfs <- lapply(yellowtail_dfs, lat_filter_335)
yellowtail_dfs <- lapply(yellowtail_dfs, depth_filter_425)
# yellowtail_dfs <- yellowtail_dfs[90:91] # reduce DFs for testing
# make the names file
yellowtail_files <- as.list(names(yellowtail_dfs))

# setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1] - 1) # to not overload your computer
registerDoParallel(cl)

setwd(yellowtail)
# with predefined function
print("Starting parallel SDM processing")
yellowtail_sdms <- foreach(i = seq_along(yellowtail_dfs), .combine = "list", .packages = c("foreach", "doParallel", "sdmTMB"), .errorhandling = "remove") %dopar% {
  print(paste("Processing SDM:", i))
  result <- species_sdm_fn(yellowtail_dfs[[i]], yellowtail_files[[i]], grid_yrs)
  print(paste("Result for SDM", i, ":", result))
  result
}
stopCluster(cl)
print("Parallel SDM processing complete")

##### read in .rds if already fit
yellowtail_sdms <- pull_files(yellowtail, "fit")

# extract outputs
yellowtail_fits <- lapply(yellowtail_sdms, fit_df_fn)
yellowtail_fit_df <- bind_fn(yellowtail_fits)
write.csv(yellowtail_fit_df, "yellowtail_fit_df.csv", row.names = F)

yellowtail_pars <- lapply(yellowtail_sdms, fit_pars_fn)
yellowtail_pars_df <- bind_fn(yellowtail_pars)
write.csv(yellowtail_pars_df, "yellowtail_pars_df.csv", row.names = F)

yellowtail_fit_check <- lapply(yellowtail_sdms, fit_check_fn)
yellowtail_fit_check_df <- bind_fit_check(yellowtail_fit_check)
write.csv(yellowtail_fit_check_df, "yellowtail_fit_check_df.csv", row.names = F)

# Remove some files to help with memory
rm(
  "yellowtail_fits", "yellowtail_fit_df", "yellowtail_pars",
  "yellowtail_pars_df", "yellowtail_fit_check", "yellowtail_fit_check_df"
)

##### read in .rds if already fit
yellowtail_indices <- pull_files(yellowtail, "index")
yellowtail_indices_df <- bind_index_fn(yellowtail_indices)
write.csv(yellowtail_indices_df, "yellowtail_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("yellowtail_dfs", "yellowtail_files", "yellowtail_sdms", "yellowtail_indices", "yellowtail_indices_df")