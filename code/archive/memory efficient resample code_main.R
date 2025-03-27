#######################################################################################################################################
#### Resample_survey_data: Multiple species, multiple years
####
#######################################################################################################################################

#### clear environment
rm(list = ls())

#### set wds
#setwd("C:/Users/Derek.Bolser/Documents/Resample_survey_data/") #for local testing
wd<- "Z:/Projects"
# wd <- "/home/user"
basedir<-file.path(wd,'Resample-survey-data')
output <- file.path(wd, "Resample-survey-data/output")
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
library(future.apply)
library(furrr)
library(future.callr)
library(readr)
library(arrow)
library(data.table)
library(TMB)

# read in data
#catch <- read.csv(file.path(data,"nwfsc_bt_fmp_spp.csv"))
catch <- read.csv(file.path(data,"nwfsc_bt_fmp_spp_updated.csv")) #pulled data again to get 2024
load(file.path(data,"california_current_grid.rda"))

source(file.path(basedir, "code/smaller_functions.R")) #need to edit to specify the code directory if running locally
source(file.path(basedir, "code/cleanup_by_species.R"))
source(file.path(basedir, "code/species_sdms.R"))

# set up grid
#rename x and y cols
california_current_grid$Longitude_dd<- california_current_grid$longitude
california_current_grid$Latitude_dd<- california_current_grid$latitude
california_current_grid$Pass<- california_current_grid$pass_scaled
california_current_grid$Depth_m<- california_current_grid$depth

#make gridyrs
grid_yrs <- replicate_df(california_current_grid, "Year", unique(catch$Year))
setwd(basedir)
#saveRDS(grid_yrs,"grid_yrs.rds")
write_parquet(grid_yrs, "data/grid_yrs.parquet")
rm(grid_yrs,california_current_grid)

#get rid of memory limits
options(future.globals.maxSize = 1 * 1024^4)  # Allow up to 1 TB for globals

#### Arrowtooth flounder ##########################################################################################################
arrowtooth_dfs <- cleanup_by_species(df = catch, species = "arrowtooth flounder")
arrowtooth_dfs <- lapply(arrowtooth_dfs, lat_filter_34)
arrowtooth_dfs <- arrowtooth_dfs[90:91] # reduce DFs for testing

# make the names file
arrowtooth_files <- as.list(names(arrowtooth_dfs))

#save some space
setwd(arrowtooth)
#saveRDS(arrowtooth_dfs,"arrowtooth_dfs.rds")
# Save each dataframe separately
for (i in seq_along(arrowtooth_dfs)) {
  write_parquet(arrowtooth_dfs[[i]], file.path(arrowtooth, paste0("df_", i, ".parquet")))
}

# Optional: Remove from memory
rm(arrowtooth_dfs)
gc()

#set up parallel processing
plan(callr, workers = 6)  # Adjust the number of workers based on available memory

# Remove large objects before parallel execution
gc()

print("Starting parallel SDM processing")
setwd(arrowtooth)

# Run SDMs in parallel
future_map(seq_along(arrowtooth_files), function(i) {
  gc()  # Free memory
  # Load only the required dataframe
  arrowtooth_df <- read_parquet(file.path(arrowtooth, paste0("df_", i, ".parquet")))
  # Load grid_yrs once
  grid_yrs <- read_parquet(file.path(basedir, "grid_yrs.parquet"))
  # Run species SDM function
  species_sdm_fn(arrowtooth_df, arrowtooth_files[[i]], grid_yrs)
  # Explicitly remove objects after processing
  rm(arrowtooth_df, grid_yrs)
  gc()
  NULL
}, .progress = TRUE,.options = furrr_options(seed = TRUE))

print("Parallel SDM processing complete")

##### process fit files
process_and_save_fits(arrowtooth,"arrowtooth")

##### process index files
arrowtooth_indices <- pull_files(arrowtooth, "index")
arrowtooth_indices_df <- bind_index_fn(arrowtooth_indices)
write.csv(arrowtooth_indices_df, "arrowtooth_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("arrowtooth_files", "arrowtooth_indices", "arrowtooth_indices_df")

#remove from memory
files_to_keep <- c("arrowtooth_fit_check_df.csv", "arrowtooth_fit_df.csv", "arrowtooth_pars_df.csv", "arrowtooth_indices_df.csv")
all_files <- list.files(path = ".", full.names = TRUE)  # Get all files
files_to_remove <- setdiff(all_files, file.path(".", files_to_keep))  # Exclude files to keep
file.remove(files_to_remove)  # Delete the files

#### Bocaccio #############################################################################################################
#### some models did not converge; likely a true result but double check #####
setwd(bocaccio)
bocaccio_dfs <- cleanup_by_species(df = catch, species = "bocaccio")
bocaccio_dfs <- lapply(bocaccio_dfs, depth_filter_500)
#bocaccio_dfs <- bocaccio_dfs[90:91] # reduce DFs for testing

# make the names file
bocaccio_files <- as.list(names(bocaccio_dfs))

#save some space
setwd(bocaccio)
#saveRDS(bocaccio_dfs,"bocaccio_dfs.rds")
# Save each dataframe separately
for (i in seq_along(bocaccio_dfs)) {
  write_parquet(bocaccio_dfs[[i]], file.path(bocaccio, paste0("df_", i, ".parquet")))
}

# Optional: Remove from memory
rm(bocaccio_dfs)
gc()

# Set up parallel processing
plan(callr, workers = 6)  # Adjust workers based on memory

gc()  # Free memory before execution

print("Starting parallel SDM processing")
setwd(bocaccio)

# Load grid_yrs once outside the parallel loop
grid_yrs <- read_parquet(file.path(basedir, "grid_yrs.parquet"))

# Run SDMs in parallel
future_imap(bocaccio_files, function(file_name, i) {
  gc()  # Free memory
  
  # Load only the required dataframe
  bocaccio_df <- read_parquet(file.path(bocaccio, paste0("df_", i, ".parquet")))
  
  # Run species SDM function
  species_sdm_fn(bocaccio_df, file_name, grid_yrs)
  
  # Explicitly remove objects after processing
  rm(bocaccio_df)
  gc()
  
  NULL  # Ensure no result storage
}, .progress = TRUE, .options = furrr_options(seed = TRUE))

print("SDM processing complete")
rm(grid_yrs)

##### process fit files
process_and_save_fits(bocaccio,"bocaccio")

##### process index files
bocaccio_indices <- pull_files(bocaccio, "index")
bocaccio_indices_df <- bind_index_fn(bocaccio_indices)
write.csv(bocaccio_indices_df, "bocaccio_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("bocaccio_files", "bocaccio_indices", "bocaccio_indices_df")

#remove from memory
files_to_keep <- c("bocaccio_fit_check_df.csv", "bocaccio_fit_df.csv", "bocaccio_pars_df.csv", "bocaccio_indices_df.csv")
all_files <- list.files(path = ".", full.names = TRUE)  # Get all files
files_to_remove <- setdiff(all_files, file.path(".", files_to_keep))  # Exclude files to keep
file.remove(files_to_remove)  # Delete the files

#### Canary rockfish ##########################################################################################################
#### double check those that did not run, e.g., everything at 0.3 effort
setwd(canary)
canary_dfs <- cleanup_by_species(df = catch, species = "canary rockfish")
canary_dfs <- lapply(canary_dfs, depth_filter_275)
# canary_dfs <- canary_dfs[90:91] # reduce DFs for testing

# make the names file
canary_files <- as.list(names(canary_dfs))

#save some space
setwd(canary)
#saveRDS(canary_dfs,"canary_dfs.rds")
# Save each dataframe separately
for (i in seq_along(canary_dfs)) {
  write_parquet(canary_dfs[[i]], file.path(canary, paste0("df_", i, ".parquet")))
}

# Optional: Remove from memory
rm(canary_dfs)
gc()

# Set up parallel processing
plan(callr, workers = 6)  # Adjust workers based on memory

gc()  # Free memory before execution

print("Starting parallel SDM processing")
setwd(canary)

# Load grid_yrs once outside the parallel loop
grid_yrs <- read_parquet(file.path(basedir, "grid_yrs.parquet"))

# Run SDMs in parallel
future_imap(canary_files, function(file_name, i) {
  gc()  # Free memory
  
  # Load only the required dataframe
  canary_df <- read_parquet(file.path(canary, paste0("df_", i, ".parquet")))
  
  # Run species SDM function
  canary_sdm_fn(canary_df, file_name, grid_yrs)
  
  # Explicitly remove objects after processing
  rm(canary_df)
  gc()
  
  NULL  # Ensure no result storage
}, .progress = TRUE, .options = furrr_options(seed = TRUE))

print("SDM processing complete")

##### process fit files
process_and_save_fits(canary,"canary")

##### process index files
canary_indices <- pull_files(canary, "index")
canary_indices_df <- bind_index_fn(canary_indices)
write.csv(canary_indices_df, "canary_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("canary_files", "canary_indices", "canary_indices_df")

#remove from memory
files_to_keep <- c("canary_fit_check_df.csv", "canary_fit_df.csv", "canary_pars_df.csv", "canary_indices_df.csv")
all_files <- list.files(path = ".", full.names = TRUE)  # Get all files
files_to_remove <- setdiff(all_files, file.path(".", files_to_keep))  # Exclude files to keep
file.remove(files_to_remove)  # Delete the files

#### Darkblotched rockfish ####################################################################################################
setwd(darkblotched)
darkblotched_dfs <- cleanup_by_species(df = catch, species = "darkblotched rockfish")
darkblotched_dfs <- lapply(darkblotched_dfs, lat_filter_335)
darkblotched_dfs <- lapply(darkblotched_dfs, depth_filter_675)
# darkblotched_dfs <- darkblotched_dfs[90:91] # reduce DFs for testing
# make the names file
darkblotched_files <- as.list(names(darkblotched_dfs))

#save some space
setwd(darkblotched)
#saveRDS(darkblotched_dfs,"darkblotched_dfs.rds")
# Save each dataframe separately
for (i in seq_along(darkblotched_dfs)) {
  write_parquet(darkblotched_dfs[[i]], file.path(darkblotched, paste0("df_", i, ".parquet")))
}

# Optional: Remove from memory
rm(darkblotched_dfs)
gc()

# Set up parallel processing
plan(callr, workers = 6)  # Adjust workers based on memory

gc()  # Free memory before execution

print("Starting parallel SDM processing")
setwd(darkblotched)

# Load grid_yrs once outside the parallel loop
grid_yrs <- read_parquet(file.path(basedir, "grid_yrs.parquet"))

# Run SDMs in parallel
future_imap(darkblotched_files, function(file_name, i) {
  gc()  # Free memory
  
  # Load only the required dataframe
  darkblotched_df <- read_parquet(file.path(darkblotched, paste0("df_", i, ".parquet")))
  
  # Run species SDM function
  darkblotched_sdm_fn(darkblotched_df, file_name, grid_yrs)
  
  # Explicitly remove objects after processing
  rm(darkblotched_df)
  gc()
  
  NULL  # Ensure no result storage
}, .progress = TRUE, .options = furrr_options(seed = TRUE))

print("SDM processing complete")

##### process fit files
process_and_save_fits(darkblotched,"darkblotched")

##### process index files
darkblotched_indices <- pull_files(darkblotched, "index")
darkblotched_indices_df <- bind_index_fn(darkblotched_indices)
write.csv(darkblotched_indices_df, "darkblotched_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("darkblotched_files", "darkblotched_indices", "darkblotched_indices_df")

#remove from memory
files_to_keep <- c("darkblotched_fit_check_df.csv", "darkblotched_fit_df.csv", "darkblotched_pars_df.csv", "darkblotched_indices_df.csv")
all_files <- list.files(path = ".", full.names = TRUE)  # Get all files
files_to_remove <- setdiff(all_files, file.path(".", files_to_keep))  # Exclude files to keep
file.remove(files_to_remove)  # Delete the files

#### Dover sole ###############################################################################################################
setwd(dover)
dover_dfs <- cleanup_by_species(df = catch, species = "Dover sole")
# dover_dfs <- dover_dfs[90:91] # reduce DFs for testing
# make the names file
dover_files <- as.list(names(dover_dfs))

#save some space
setwd(dover)
#saveRDS(dover_dfs,"dover_dfs.rds")
# Save each dataframe separately
for (i in seq_along(dover_dfs)) {
  write_parquet(dover_dfs[[i]], file.path(dover, paste0("df_", i, ".parquet")))
}

# Optional: Remove from memory
rm(dover_dfs)
gc()

# Set up parallel processing
plan(callr, workers = 6)  # Adjust workers based on memory

gc()  # Free memory before execution

print("Starting parallel SDM processing")
setwd(dover)

# Load grid_yrs once outside the parallel loop
grid_yrs <- read_parquet(file.path(basedir, "grid_yrs.parquet"))

# Run SDMs in parallel
future_imap(dover_files, function(file_name, i) {
  gc()  # Free memory
  
  # Load only the required dataframe
  dover_df <- read_parquet(file.path(dover, paste0("df_", i, ".parquet")))
  
  # Run species SDM function
  species_sdm_fn(dover_df, file_name, grid_yrs)
  
  # Explicitly remove objects after processing
  rm(dover_df)
  gc()
  
  NULL  # Ensure no result storage
}, .progress = TRUE, .options = furrr_options(seed = TRUE))

print("SDM processing complete")

##### process fit files
process_and_save_fits(dover,"dover")

##### process index files
dover_indices <- pull_files(dover, "index")
dover_indices_df <- bind_index_fn(dover_indices)
write.csv(dover_indices_df, "dover_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("dover_files", "dover_indices", "dover_indices_df")

#remove from memory
files_to_keep <- c("dover_fit_check_df.csv", "dover_fit_df.csv", "dover_pars_df.csv", "dover_indices_df.csv")
all_files <- list.files(path = ".", full.names = TRUE)  # Get all files
files_to_remove <- setdiff(all_files, file.path(".", files_to_keep))  # Exclude files to keep
file.remove(files_to_remove)  # Delete the files

#### Lingcod North ############################################################################################################
setwd(lingcod_n)
lingcod_n_dfs <- cleanup_by_species(df = catch, species = "lingcod")
lingcod_n_dfs <- lapply(lingcod_n_dfs, lat_filter_34)
lingcod_n_dfs <- lapply(lingcod_n_dfs, depth_filter_450)
# lingcod_n_dfs <- gsub("lingcod", "lingcod_n", lingcod_n_dfs)
# lingcod_n_dfs <- lingcod_n_dfs[90:91] # reduce DFs for testing
# make the names file
lingcod_n_files <- as.list(names(lingcod_n_dfs))

#save some space
setwd(lingcod_n)
#saveRDS(lingcod_n_dfs,"lingcod_n_dfs.rds")
# Save each dataframe separately
for (i in seq_along(lingcod_n_dfs)) {
  write_parquet(lingcod_n_dfs[[i]], file.path(lingcod_n, paste0("df_", i, ".parquet")))
}

# Optional: Remove from memory
rm(lingcod_n_dfs)
gc()

# Set up parallel processing
plan(callr, workers = 6)  # Adjust workers based on memory

gc()  # Free memory before execution

print("Starting parallel SDM processing")
setwd(lingcod_n)

# Load grid_yrs once outside the parallel loop
grid_yrs <- read_parquet(file.path(basedir, "grid_yrs.parquet"))

# Run SDMs in parallel
future_imap(lingcod_n_files, function(file_name, i) {
  gc()  # Free memory
  
  # Load only the required dataframe
  lingcod_n_df <- read_parquet(file.path(lingcod_n, paste0("df_", i, ".parquet")))
  
  # Run species SDM function
  species_sdm_fn(lingcod_n_df, file_name, grid_yrs)
  
  # Explicitly remove objects after processing
  rm(lingcod_n_df)
  gc()
  
  NULL  # Ensure no result storage
}, .progress = TRUE, .options = furrr_options(seed = TRUE))

print("SDM processing complete")

##### process fit files
process_and_save_fits(lingcod_n,"lingcod_n")

##### process index files
lingcod_n_indices <- pull_files(lingcod_n, "index")
lingcod_n_indices_df <- bind_index_fn(lingcod_n_indices)
write.csv(lingcod_n_indices_df, "lingcod_n_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("lingcod_n_files", "lingcod_n_indices", "lingcod_n_indices_df")

#remove from memory
files_to_keep <- c("lingcod_n_fit_check_df.csv", "lingcod_n_fit_df.csv", "lingcod_n_pars_df.csv", "lingcod_n_indices_df.csv")
all_files <- list.files(path = ".", full.names = TRUE)  # Get all files
files_to_remove <- setdiff(all_files, file.path(".", files_to_keep))  # Exclude files to keep
file.remove(files_to_remove)  # Delete the files

#### Lingcod South ###############################################################################################
setwd(lingcod_s)
lingcod_s_dfs <- cleanup_by_species(df = catch, species = "lingcod")
lingcod_s_dfs <- lapply(lingcod_s_dfs, lat_filter_34_max)
lingcod_s_dfs <- lapply(lingcod_s_dfs, depth_filter_450)
# lingcod_s_dfs <- lingcod_s_dfs[90:91] # reduce DFs for testing
#lingcod_s_dfs <- gsub("lingcod", "lingcod_s", lingcod_s_dfs)
# make the names file
lingcod_s_files <- as.list(names(lingcod_s_dfs))

#save some space
setwd(lingcod_s)
#saveRDS(lingcod_s_dfs,"lingcod_s_dfs.rds")
# Save each dataframe separately
for (i in seq_along(lingcod_s_dfs)) {
  write_parquet(lingcod_s_dfs[[i]], file.path(lingcod_s, paste0("df_", i, ".parquet")))
}

# Optional: Remove from memory
rm(lingcod_s_dfs)
gc()

# Set up parallel processing
plan(callr, workers = 6)  # Adjust workers based on memory

gc()  # Free memory before execution

print("Starting parallel SDM processing")
setwd(lingcod_s)

# Load grid_yrs once outside the parallel loop
grid_yrs <- read_parquet(file.path(basedir, "grid_yrs.parquet"))

# Run SDMs in parallel
future_imap(lingcod_s_files, function(file_name, i) {
  gc()  # Free memory
  
  # Load only the required dataframe
  lingcod_s_df <- read_parquet(file.path(lingcod_s, paste0("df_", i, ".parquet")))
  
  # Run species SDM function
  species_sdm_fn(lingcod_s_df, file_name, grid_yrs)
  
  # Explicitly remove objects after processing
  rm(lingcod_s_df)
  gc()
  
  NULL  # Ensure no result storage
}, .progress = TRUE, .options = furrr_options(seed = TRUE))

print("SDM processing complete")


##### process fit files
process_and_save_fits(lingcod_s,"lingcod_s")

##### process index files
lingcod_s_indices <- pull_files(lingcod_s, "index")
lingcod_s_indices_df <- bind_index_fn(lingcod_s_indices)
write.csv(lingcod_s_indices_df, "lingcod_s_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("lingcod_s_files", "lingcod_s_indices", "lingcod_s_indices_df")

#remove from memory
files_to_keep <- c("lingcod_s_fit_check_df.csv", "lingcod_s_fit_df.csv", "lingcod_s_pars_df.csv", "lingcod_s_indices_df.csv")
all_files <- list.files(path = ".", full.names = TRUE)  # Get all files
files_to_remove <- setdiff(all_files, file.path(".", files_to_keep))  # Exclude files to keep
file.remove(files_to_remove)  # Delete the files

#### Longnose Skate ###########################################################################################################
setwd(longnose)
longnose_dfs <- cleanup_by_species(df = catch, species = "longnose skate")
# longnose_dfs <- longnose_dfs[90:91] # reduce DFs for testing
# make the names file
longnose_files <- as.list(names(longnose_dfs))

#save some space
setwd(longnose)
#saveRDS(longnose_dfs,"longnose_dfs.rds")
# Save each dataframe separately
for (i in seq_along(longnose_dfs)) {
  write_parquet(longnose_dfs[[i]], file.path(longnose, paste0("df_", i, ".parquet")))
}

# Optional: Remove from memory
rm(longnose_dfs)
gc()

# Set up parallel processing
plan(callr, workers = 6)  # Adjust workers based on memory

gc()  # Free memory before execution

print("Starting parallel SDM processing")
setwd(longnose)

# Load grid_yrs once outside the parallel loop
grid_yrs <- read_parquet(file.path(basedir, "grid_yrs.parquet"))

# Run SDMs in parallel
future_imap(longnose_files, function(file_name, i) {
  gc()  # Free memory
  
  # Load only the required dataframe
  longnose_df <- read_parquet(file.path(longnose, paste0("df_", i, ".parquet")))
  
  # Run species SDM function
  species_sdm_fn(longnose_df, file_name, grid_yrs)
  
  # Explicitly remove objects after processing
  rm(longnose_df)
  gc()
  
  NULL  # Ensure no result storage
}, .progress = TRUE, .options = furrr_options(seed = TRUE))

print("SDM processing complete")

##### process fit files
process_and_save_fits(longnose,"longnose")

##### process index files
longnose_indices <- pull_files(longnose, "index")
longnose_indices_df <- bind_index_fn(longnose_indices)
write.csv(longnose_indices_df, "longnose_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("longnose_files", "longnose_indices", "longnose_indices_df")

#remove from memory
files_to_keep <- c("longnose_fit_check_df.csv", "longnose_fit_df.csv", "longnose_pars_df.csv", "longnose_indices_df.csv")
all_files <- list.files(path = ".", full.names = TRUE)  # Get all files
files_to_remove <- setdiff(all_files, file.path(".", files_to_keep))  # Exclude files to keep
file.remove(files_to_remove)  # Delete the files

#### Pacific ocean perch ######################################################################################################
setwd(pop)
pop_dfs <- cleanup_by_species(df = catch, species = "Pacific ocean perch")
pop_dfs <- lapply(pop_dfs, lat_filter_35)
pop_dfs <- lapply(pop_dfs, depth_filter_500)
# pop_dfs <- pop_dfs[90:91] # reduce DFs for testing
# make the names file
pop_files <- as.list(names(pop_dfs))

#save some space
setwd(pop)
#saveRDS(pop_dfs,"pop_dfs.rds")
# Save each dataframe separately
for (i in seq_along(pop_dfs)) {
  write_parquet(pop_dfs[[i]], file.path(pop, paste0("df_", i, ".parquet")))
}

# Optional: Remove from memory
rm(pop_dfs)
gc()

# Set up parallel processing
plan(callr, workers = 6)  # Adjust workers based on memory

gc()  # Free memory before execution

print("Starting parallel SDM processing")
setwd(pop)

# Load grid_yrs once outside the parallel loop
grid_yrs <- read_parquet(file.path(basedir, "grid_yrs.parquet"))

# Run SDMs in parallel
future_imap(pop_files, function(file_name, i) {
  gc()  # Free memory
  
  # Load only the required dataframe
  pop_df <- read_parquet(file.path(pop, paste0("df_", i, ".parquet")))
  
  # Run species SDM function
  species_sdm_fn(pop_df, file_name, grid_yrs)
  
  # Explicitly remove objects after processing
  rm(pop_df)
  gc()
  
  NULL  # Ensure no result storage
}, .progress = TRUE, .options = furrr_options(seed = TRUE))

print("SDM processing complete")

##### process fit files
process_and_save_fits(pop,"pop")

##### process index files
pop_indices <- pull_files(pop, "index")
pop_indices_df <- bind_index_fn(pop_indices)
write.csv(pop_indices_df, "pop_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("pop_files", "pop_indices", "pop_indices_df")

#remove from memory
files_to_keep <- c("pop_fit_check_df.csv", "pop_fit_df.csv", "pop_pars_df.csv", "pop_indices_df.csv")
all_files <- list.files(path = ".", full.names = TRUE)  # Get all files
files_to_remove <- setdiff(all_files, file.path(".", files_to_keep))  # Exclude files to keep
file.remove(files_to_remove)  # Delete the files

#### Pacific spiny dogfish ########################################################################################################
setwd(dogfish)
dogfish_dfs <- cleanup_by_species(df = catch, species = "Pacific spiny dogfish")
dogfish_dfs <- lapply(dogfish_dfs, depth_filter_700)
# dogfish_dfs <- dogfish_dfs[90:91] # reduce DFs for testing
# make the names file
dogfish_files <- as.list(names(dogfish_dfs))

#save some space
setwd(dogfish)
#saveRDS(dogfish_dfs,"dogfish_dfs.rds")
# Save each dataframe separately
for (i in seq_along(dogfish_dfs)) {
  write_parquet(dogfish_dfs[[i]], file.path(dogfish, paste0("df_", i, ".parquet")))
}

# Optional: Remove from memory
rm(dogfish_dfs)
gc()

# Set up parallel processing
plan(callr, workers = 6)  # Adjust workers based on memory

gc()  # Free memory before execution

print("Starting parallel SDM processing")
setwd(dogfish)

# Load grid_yrs once outside the parallel loop
grid_yrs <- read_parquet(file.path(basedir, "grid_yrs.parquet"))

# Run SDMs in parallel
future_imap(dogfish_files, function(file_name, i) {
  gc()  # Free memory
  
  # Load only the required dataframe
  dogfish_df <- read_parquet(file.path(dogfish, paste0("df_", i, ".parquet")))
  
  # Run species SDM function
  species_sdm_fn(dogfish_df, file_name, grid_yrs)
  
  # Explicitly remove objects after processing
  rm(dogfish_df)
  gc()
  
  NULL  # Ensure no result storage
}, .progress = TRUE, .options = furrr_options(seed = TRUE))

print("SDM processing complete")

##### process fit files
process_and_save_fits(dogfish,"dogfish")

##### process index files
dogfish_indices <- pull_files(dogfish, "index")
dogfish_indices_df <- bind_index_fn(dogfish_indices)
write.csv(dogfish_indices_df, "dogfish_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("dogfish_files", "dogfish_indices", "dogfish_indices_df")

#remove from memory
files_to_keep <- c("dogfish_fit_check_df.csv", "dogfish_fit_df.csv", "dogfish_pars_df.csv", "dogfish_indices_df.csv")
all_files <- list.files(path = ".", full.names = TRUE)  # Get all files
files_to_remove <- setdiff(all_files, file.path(".", files_to_keep))  # Exclude files to keep
file.remove(files_to_remove)  # Delete the files

#### Petrale sole #############################################################################################################
setwd(petrale)
petrale_dfs <- cleanup_by_species(df = catch, species = "petrale sole")
petrale_dfs <- lapply(petrale_dfs, depth_filter_675)
# petrale_dfs <- petrale_dfs[90:91] # reduce DFs for testing
# make the names file
petrale_files <- as.list(names(petrale_dfs))

#save some space
setwd(petrale)
#saveRDS(petrale_dfs,"petrale_dfs.rds")
# Save each dataframe separately
for (i in seq_along(petrale_dfs)) {
  write_parquet(petrale_dfs[[i]], file.path(petrale, paste0("df_", i, ".parquet")))
}

# Optional: Remove from memory
rm(petrale_dfs)
gc()

# Set up parallel processing
plan(callr, workers = 6)  # Adjust workers based on memory

gc()  # Free memory before execution

print("Starting parallel SDM processing")
setwd(petrale)

# Load grid_yrs once outside the parallel loop
grid_yrs <- read_parquet(file.path(basedir, "grid_yrs.parquet"))

# Run SDMs in parallel
future_imap(petrale_files, function(file_name, i) {
  gc()  # Free memory
  
  # Load only the required dataframe
  petrale_df <- read_parquet(file.path(petrale, paste0("df_", i, ".parquet")))
  
  # Run species SDM function
  species_sdm_lognormal_fn(petrale_df, file_name, grid_yrs)
  
  # Explicitly remove objects after processing
  rm(petrale_df)
  gc()
  
  NULL  # Ensure no result storage
}, .progress = TRUE, .options = furrr_options(seed = TRUE))

print("SDM processing complete")

##### process fit files
process_and_save_fits(petrale,"petrale")

##### process index files
petrale_indices <- pull_files(petrale, "index")
petrale_indices_df <- bind_index_fn(petrale_indices)
write.csv(petrale_indices_df, "petrale_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("petrale_files", "petrale_indices", "petrale_indices_df")

#remove from memory
files_to_keep <- c("petrale_fit_check_df.csv", "petrale_fit_df.csv", "petrale_pars_df.csv", "petrale_indices_df.csv")
all_files <- list.files(path = ".", full.names = TRUE)  # Get all files
files_to_remove <- setdiff(all_files, file.path(".", files_to_keep))  # Exclude files to keep
file.remove(files_to_remove)  # Delete the files

#### Rex sole #################################################################################################################
setwd(rex)
rex_dfs <- cleanup_by_species(df = catch, species = "rex sole")
rex_dfs <- lapply(rex_dfs, depth_filter_700)
# rex_dfs <- rex_dfs[90:91] # reduce DFs for testing
# make the names file
rex_files <- as.list(names(rex_dfs))

#save some space
setwd(rex)
#saveRDS(rex_dfs,"rex_dfs.rds")
# Save each dataframe separately
for (i in seq_along(rex_dfs)) {
  write_parquet(rex_dfs[[i]], file.path(rex, paste0("df_", i, ".parquet")))
}

# Optional: Remove from memory
rm(rex_dfs)
gc()

# Set up parallel processing
plan(callr, workers = 6)  # Adjust workers based on memory

gc()  # Free memory before execution

print("Starting parallel SDM processing")
setwd(rex)

# Load grid_yrs once outside the parallel loop
grid_yrs <- read_parquet(file.path(basedir, "grid_yrs.parquet"))

# Run SDMs in parallel
future_imap(rex_files, function(file_name, i) {
  gc()  # Free memory
  
  # Load only the required dataframe
  rex_df <- read_parquet(file.path(rex, paste0("df_", i, ".parquet")))
  
  # Run species SDM function
  species_sdm_fn(rex_df, file_name, grid_yrs)
  
  # Explicitly remove objects after processing
  rm(rex_df)
  gc()
  
  NULL  # Ensure no result storage
}, .progress = TRUE, .options = furrr_options(seed = TRUE))

print("SDM processing complete")

##### process fit files
process_and_save_fits(rex,"rex")

##### process index files
rex_indices <- pull_files(rex, "index")
rex_indices_df <- bind_index_fn(rex_indices)
write.csv(rex_indices_df, "rex_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("rex_files", "rex_indices", "rex_indices_df")

#remove from memory
files_to_keep <- c("rex_fit_check_df.csv", "rex_fit_df.csv", "rex_pars_df.csv", "rex_indices_df.csv")
all_files <- list.files(path = ".", full.names = TRUE)  # Get all files
files_to_remove <- setdiff(all_files, file.path(".", files_to_keep))  # Exclude files to keep
file.remove(files_to_remove)  # Delete the files

#### Sablefish ##################################################################################################################
setwd(sablefish)
sablefish_dfs <- cleanup_by_species(df = catch, species = "sablefish")
# sablefish_dfs <- sablefish_dfs[90:91] # reduce DFs for testing
# make the names file
sablefish_files <- as.list(names(sablefish_dfs))

#save some space
setwd(sablefish)
#saveRDS(sablefish_dfs,"sablefish_dfs.rds")
# Save each dataframe separately
for (i in seq_along(sablefish_dfs)) {
  write_parquet(sablefish_dfs[[i]], file.path(sablefish, paste0("df_", i, ".parquet")))
}

# Optional: Remove from memory
rm(sablefish_dfs)
gc()

# Set up parallel processing
plan(callr, workers = 6)  # Adjust workers based on memory

gc()  # Free memory before execution

print("Starting parallel SDM processing")
setwd(sablefish)

# Load grid_yrs once outside the parallel loop
grid_yrs <- read_parquet(file.path(basedir, "grid_yrs.parquet"))

# Run SDMs in parallel
future_imap(sablefish_files, function(file_name, i) {
  gc()  # Free memory
  
  # Load only the required dataframe
  sablefish_df <- read_parquet(file.path(sablefish, paste0("df_", i, ".parquet")))
  
  # Run species SDM function
  species_sdm_lognormal_fn(sablefish_df, file_name, grid_yrs)
  
  # Explicitly remove objects after processing
  rm(sablefish_df)
  gc()
  
  NULL  # Ensure no result storage
}, .progress = TRUE, .options = furrr_options(seed = TRUE))

print("SDM processing complete")

##### process fit files
process_and_save_fits(sablefish,"sablefish")

##### process index files
sablefish_indices <- pull_files(sablefish, "index")
sablefish_indices_df <- bind_index_fn(sablefish_indices)
write.csv(sablefish_indices_df, "sablefish_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("sablefish_files", "sablefish_indices", "sablefish_indices_df")

#remove from memory
files_to_keep <- c("sablefish_fit_check_df.csv", "sablefish_fit_df.csv", "sablefish_pars_df.csv", "sablefish_indices_df.csv")
all_files <- list.files(path = ".", full.names = TRUE)  # Get all files
files_to_remove <- setdiff(all_files, file.path(".", files_to_keep))  # Exclude files to keep
file.remove(files_to_remove)  # Delete the files

#### Shortspine thornyhead ####################################################################################################
# This took over 7.5 hours to run and still didn't finish
setwd(shortspine)
shortspine_dfs <- cleanup_by_species(df = catch, species = "shortspine thornyhead")
# shortspine_dfs <- shortspine_dfs[90:91] # reduce DFs for testing
# make the names file
shortspine_files <- as.list(names(shortspine_dfs))

#save some space
setwd(shortspine)
#saveRDS(shortspine_dfs,"shortspine_dfs.rds")
# Save each dataframe separately
for (i in seq_along(shortspine_dfs)) {
  write_parquet(shortspine_dfs[[i]], file.path(shortspine, paste0("df_", i, ".parquet")))
}

# Optional: Remove from memory
rm(shortspine_dfs)
gc()

# Set up parallel processing
plan(callr, workers = 6)  # Adjust workers based on memory

gc()  # Free memory before execution

print("Starting parallel SDM processing")
setwd(shortspine)

# Load grid_yrs once outside the parallel loop
grid_yrs <- read_parquet(file.path(basedir, "grid_yrs.parquet"))

# Run SDMs in parallel
future_imap(shortspine_files, function(file_name, i) {
  gc()  # Free memory
  
  # Load only the required dataframe
  shortspine_df <- read_parquet(file.path(shortspine, paste0("df_", i, ".parquet")))
  
  # Run species SDM function
  shortspine_sdm_fn(shortspine_df, file_name, grid_yrs)
  
  # Explicitly remove objects after processing
  rm(shortspine_df)
  gc()
  
  NULL  # Ensure no result storage
}, .progress = TRUE, .options = furrr_options(seed = TRUE))

print("SDM processing complete")

##### process fit files
process_and_save_fits(shortspine,"shortspine")

##### process index files
shortspine_indices <- pull_files(shortspine, "index")
shortspine_indices_df <- bind_index_fn(shortspine_indices)
write.csv(shortspine_indices_df, "shortspine_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("shortspine_files", "shortspine_indices", "shortspine_indices_df")

#remove from memory
files_to_keep <- c("shortspine_fit_check_df.csv", "shortspine_fit_df.csv", "shortspine_pars_df.csv", "shortspine_indices_df.csv")
all_files <- list.files(path = ".", full.names = TRUE)  # Get all files
files_to_remove <- setdiff(all_files, file.path(".", files_to_keep))  # Exclude files to keep
file.remove(files_to_remove)  # Delete the files


#### Yellowtail rockfish ######################################################################################################
setwd(yellowtail)
yellowtail_dfs <- cleanup_by_species(df = catch, species = "yellowtail rockfish")
yellowtail_dfs <- lapply(yellowtail_dfs, lat_filter_335)
yellowtail_dfs <- lapply(yellowtail_dfs, depth_filter_425)
# yellowtail_dfs <- yellowtail_dfs[90:91] # reduce DFs for testing
# make the names file
yellowtail_files <- as.list(names(yellowtail_dfs))

#save some space
setwd(yellowtail)
#saveRDS(yellowtail_dfs,"yellowtail_dfs.rds")
# Save each dataframe separately
for (i in seq_along(yellowtail_dfs)) {
  write_parquet(yellowtail_dfs[[i]], file.path(yellowtail, paste0("df_", i, ".parquet")))
}

# Optional: Remove from memory
rm(yellowtail_dfs)
gc()

# Set up parallel processing
plan(callr, workers = 6)  # Adjust workers based on memory

gc()  # Free memory before execution

print("Starting parallel SDM processing")
setwd(yellowtail)

# Load grid_yrs once outside the parallel loop
grid_yrs <- read_parquet(file.path(basedir, "grid_yrs.parquet"))

# Run SDMs in parallel
future_imap(yellowtail_files, function(file_name, i) {
  gc()  # Free memory
  
  # Load only the required dataframe
  yellowtail_df <- read_parquet(file.path(yellowtail, paste0("df_", i, ".parquet")))
  
  # Run species SDM function
  species_sdm_fn(yellowtail_df, file_name, grid_yrs)
  
  # Explicitly remove objects after processing
  rm(yellowtail_df)
  gc()
  
  NULL  # Ensure no result storage
}, .progress = TRUE, .options = furrr_options(seed = TRUE))

print("SDM processing complete")


##### process fit files
process_and_save_fits(yellowtail,"yellowtail")

##### process index files
yellowtail_indices <- pull_files(yellowtail, "index")
yellowtail_indices_df <- bind_index_fn(yellowtail_indices)
write.csv(yellowtail_indices_df, "yellowtail_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("yellowtail_files", "yellowtail_indices", "yellowtail_indices_df")

#remove from memory
files_to_keep <- c("yellowtail_fit_check_df.csv", "yellowtail_fit_df.csv", "yellowtail_pars_df.csv", "yellowtail_indices_df.csv")
all_files <- list.files(path = ".", full.names = TRUE)  # Get all files
files_to_remove <- setdiff(all_files, file.path(".", files_to_keep))  # Exclude files to keep
file.remove(files_to_remove)  # Delete the files

#### Widow rockfish ###########################################################################################################
setwd(widow)
widow_dfs <- cleanup_by_species(df = catch, species = "widow rockfish")
widow_dfs <- lapply(widow_dfs, lat_filter_335)
widow_dfs <- lapply(widow_dfs, depth_filter_675)
# widow_dfs <- widow_dfs[90:91] # reduce DFs for testing
# make the names file
widow_files <- as.list(names(widow_dfs))

#save some space
setwd(widow)
#saveRDS(widow_dfs,"widow_dfs.rds")
# Save each dataframe separately
for (i in seq_along(widow_dfs)) {
  write_parquet(widow_dfs[[i]], file.path(widow, paste0("df_", i, ".parquet")))
}

# Optional: Remove from memory
rm(widow_dfs)
gc()

# Set up parallel processing
plan(callr, workers = 6)  # Adjust workers based on memory

gc()  # Free memory before execution

print("Starting parallel SDM processing")
setwd(widow)

# Load grid_yrs once outside the parallel loop
grid_yrs <- read_parquet(file.path(basedir, "grid_yrs.parquet"))

# Run SDMs in parallel
future_imap(widow_files, function(file_name, i) {
  gc()  # Free memory
  
  # Load only the required dataframe
  widow_df <- read_parquet(file.path(widow, paste0("df_", i, ".parquet")))
  
  # Run species SDM function
  species_sdm_fn(widow_df, file_name, grid_yrs)
  
  # Explicitly remove objects after processing
  rm(widow_df)
  gc()
  
  NULL  # Ensure no result storage
}, .progress = TRUE, .options = furrr_options(seed = TRUE))

print("SDM processing complete")


##### process fit files
process_and_save_fits(widow,"widow")

##### process index files
widow_indices <- pull_files(widow, "index")
widow_indices_df <- bind_index_fn(widow_indices)
write.csv(widow_indices_df, "widow_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("widow_files", "widow_indices", "widow_indices_df")

#remove from memory
files_to_keep <- c("widow_fit_check_df.csv", "widow_fit_df.csv", "widow_pars_df.csv", "widow_indices_df.csv")
all_files <- list.files(path = ".", full.names = TRUE)  # Get all files
files_to_remove <- setdiff(all_files, file.path(".", files_to_keep))  # Exclude files to keep
file.remove(files_to_remove)  # Delete the files

