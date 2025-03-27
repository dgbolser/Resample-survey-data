#### Functions ####

#' Tow Function
#'
#' Create a vector of tows for including or excluding.
#'
#' @param x catch_split data frame
#'
tow_fn <- function(x) {
  tows <- as.data.frame(x$Trawl_id)
  tows <- unique(tows)
  tows <- as.data.frame(tows[!is.na(tows)])
  names(tows) <- "Trawl_id"
  return(tows)
}


#' Include or Exclude
#'
#' Specify how to downsample. For simple random sampling, a proportion of
#' stations should do.
#'
#' @param df tows data frame
#' @param proportions proportions developed using: props <- as.data.frame(seq
#' (0.1,1.0, by = 0.1)) replicated by the length of the tows dataframe. The name
#' of the props is "Trawl_id".
#'
include_or_exclude <- function(df, proportions, replicate_num = 10) {
  # Get the number of rows in the dataframe
  num_rows <- nrow(df)
  
  # Use lapply to create a list of dataframes
  result_list <- lapply(proportions, function(p) {
    # Generate a random vector of 1s and 0s based on the specified proportion
    set.seed(1)
    random_vectors <- replicate(replicate_num, sample(c(1, 0), size = num_rows, replace = TRUE, prob = c(p, 1 - p)), simplify = F)
    
    # Create a new dataframe with the random assignments
    lapply(random_vectors, function(rv) {
      cbind(df, RandomAssignment = rv)
    })
  })
  
  # flatten into single list
  result_list <- do.call(c, result_list)
  
  # Set names for the list elements based on proportions
  names(result_list) <- paste0(rep(proportions, each = 10), "_", rep(1:10, times = length(proportions)))
  
  # Return the list of dataframes
  return(result_list)
}


#' Join Data Frames
#'
#' Function to join a list of data frames to a main data frame using a shared
#' column.
#'
#' @param list_of_d.fs The list of data frames that you would like to join to
#' the main data frame
#' @param main_df The main data frame that you want to join the list of data
#' frames
#' @param shared_column The column that all the data frames share which will be
#' used to join the data frames together.
#'
join_dfs <- function(list_of_dfs, main_df, shared_column) {
  merged_dfs <- lapply(list_of_dfs, function(df) {
    merged_df <- merge(df, main_df, by = shared_column)
    return(merged_df)
  })
  return(merged_dfs)
}

####### calculate model based indices for all species and effort levels ######################################
####### generic functions
#' Create data frame of the species distribution model fit.
#'
#' Function to take the species distribution model fit and write it to a csv.
#'
#' @param fit Species distribution model fit from whatever smd_fn was used
#' (see species_sdms.R file for specifics on those sdm files).
#' @return main model fit data frame
#'
fit_df_fn <- function(fit) {
  fit_df <- tidy(fit, conf.int = T)
  return(fit_df)
  
  filename <- paste0(name(fit), "_fit_df.csv")
  write.csv(fit_df, file = filename, row.names = F)
}

#' Create data frame of the species distribution model parameter estimates.
#'
#' Function to get the species distribution model parameter estimates and write
#' it to a csv.
#'
#' @param fit Species distribution model fit from whatever smd_fn was used
#' (see species_sdms.R file for specifics on those sdm files).
#' @return parameter estimates data frame
#'
fit_pars_fn <- function(fit) {
  fit_pars <- tidy(fit, effects = "ran_pars", conf.int = T)
  return(fit_pars)
  
  filename <- paste0(name(fit), "_fit_pars.csv")
  write.csv(index, file = filename, row.names = F)
}

#' Create data frame of the species distribution model diagnostics.
#'
#' Function to get the species distribution model diagnostics and write
#' it to a csv.
#'
#' @param fit Species distribution model fit from whatever smd_fn was used
#' (see species_sdms.R file for specifics on those sdm files).
#' @return diagnostics data frame
#'
fit_check_fn <- function(fit) {
  fit_check <- sanity(fit)
  return(fit_check)
  
  filename <- paste0(name(fit), "_fit_check.csv")
  write.csv(index, file = filename, row.names = F)
}

#' Bind function for species distribution model fits and parameters
#'
#' Function to rbind and bring effort and replicates in as columns from
#' rownames of species distribution model fits and parameters.
#'
#' @param x Species distribution model fits or parameters data frame after
#' being read in by fit_df_fn or fit_pars_fn respectively.
#' @return New data frame of fits or parameters with the rownames as columns
#'
bind_fn <- function(x) {
  y <- do.call(rbind, x)
  y$effort <- gsub("fit_", "", row.names(y))
  y$replicate <- gsub(".*_", "", y$effort)
  y$effort <- gsub("_.*", "", y$effort)
  y$replicate <- gsub("\\..*", "", y$replicate)
  y$effort <- as.numeric(y$effort)
  y$replicate <- as.numeric(y$replicate)
  return(y)
}

#' Bind function for species distribution model diagnostics
#'
#' Function to rbind and bring effort and replicates in as columns from
#' rownames of species distribution model diagnostics.
#'
#' @param x Species distribution model diagnostics data frame after being read in
#' by fit_df_fn.
#' @return New data frame of diagnostics with the rownames as columns
#'
# special bind function for fit check dfs
bind_fit_check <- function(x) {
  y <- do.call(rbind, x)
  y <- as.data.frame(y)
  y$effort <- gsub("fit_", "", row.names(y))
  y$replicate <- gsub(".*_", "", y$effort)
  y$effort <- gsub("_.*", "", y$effort)
  y$replicate <- gsub("\\..*", "", y$replicate)
  y$effort <- as.numeric(y$effort)
  y$replicate <- as.numeric(y$replicate)
  y <- apply(y, 2, as.character)
  return(y)
}

#' Bind function for species distribution model index
#'
#' Function to rbind and bring effort and replicates in as columns from
#' rownames of species distribution model index.
#'
#' @param x Species distribution model index data frame after being read in.
#' @return New data frame of index with the rownames as columns
#'
bind_index_fn <- function(x) {
  y <- do.call(rbind, x)
  y$effort <- gsub(".rds.*", "", row.names(y))
  y$effort <- gsub("index_", "", y$effort)
  y$replicate <- gsub(".*_", "", y$effort)
  y$effort <- gsub("_.*", "", y$effort)
  y$effort <- as.numeric(y$effort)
  y$replicate <- as.numeric(y$replicate)
  return(y)
}

#' Pull files with specified character strings
#'
#' Function to read (pull) files from the set species directory containing
#' specified character strings.
#'
#' @param directory Species directory to look for files in.
#' @param string Character string to search files for.
#' @return Read in list of all files containing the character string searched for.
#'
pull_files <- function(directory, string) {
  # List all RDS files in the directory
  files <- list.files(directory, pattern = "\\.rds$", full.names = TRUE)
  
  # Filter files that contain the search string
  filtered_files <- files[grepl(string, files)]
  
  # Read in files and avoid unnecessary memory copies
  data_list <- vector("list", length(filtered_files))
  names(data_list) <- basename(filtered_files)
  
  for (i in seq_along(filtered_files)) {
    data_list[[i]] <- readRDS(filtered_files[i])
  }
  
  gc()  # Force garbage collection
  return(data_list)
}

process_and_save_fits <- function(directory, name) {
  # Get all RDS files that contain "fit" in their names
  files <- list.files(directory, pattern = ".*fit.*\\.rds$", full.names = TRUE)
  
  # Define output file paths
  fit_df_path <- file.path(directory, paste0(name, "_fit_df.csv"))
  fit_pars_path <- file.path(directory, paste0(name, "_pars_df.csv"))
  fit_check_path <- file.path(directory, paste0(name, "_fit_check_df.csv"))
  
  # Initialize empty lists to store results
  all_fit_df <- list()
  all_fit_pars <- list()
  all_fit_check <- list()
  
  # Process each file
  for (file in files) {
    fit <- readRDS(file)
    
    # Ensure extracted objects are dataframes
    fit_df <- as.data.frame(fit_df_fn(fit))
    fit_pars <- as.data.frame(fit_pars_fn(fit))
    fit_check <- as.data.frame(fit_check_fn(fit))
    
    # Store results in lists
    all_fit_df[[file]] <- fit_df
    all_fit_pars[[file]] <- fit_pars
    all_fit_check[[file]] <- fit_check
    
    # Free memory
    rm(fit, fit_df, fit_pars, fit_check)
    gc()
  }
  
  # Combine lists into single dataframes
  final_fit_df <- if (length(all_fit_df) > 0) rbindlist(all_fit_df, fill = TRUE) else NULL
  final_fit_pars <- if (length(all_fit_pars) > 0) rbindlist(all_fit_pars, fill = TRUE) else NULL
  final_fit_check <- if (length(all_fit_check) > 0) rbindlist(all_fit_check, fill = TRUE) else NULL
  
  # Write to CSV if there is data
  if (!is.null(final_fit_df)) fwrite(final_fit_df, file = fit_df_path)
  if (!is.null(final_fit_pars)) fwrite(final_fit_pars, file = fit_pars_path)
  if (!is.null(final_fit_check)) fwrite(final_fit_check, file = fit_check_path)
}

###### modified function to summarize fit files ################
process_and_save_fits <- function(directory, name) {
  # Get all RDS files that contain "fit" in their names
  files <- list.files(directory, pattern = ".*fit.*\\.rds$", full.names = TRUE)
  
  # Define output file paths
  fit_df_path <- file.path(directory, paste0(name, "_fit_df.csv"))
  fit_pars_path <- file.path(directory, paste0(name, "_pars_df.csv"))
  fit_check_path <- file.path(directory, paste0(name, "_fit_check_df.csv"))
  
  # Initialize empty lists to store results
  all_fit_df <- list()
  all_fit_pars <- list()
  all_fit_check <- list()
  
  # Process each file
  for (file in files) {
    fit <- readRDS(file)
    
    # Ensure extracted objects are dataframes
    fit_df <- as.data.frame(fit_df_fn(fit))
    fit_pars <- as.data.frame(fit_pars_fn(fit))
    fit_check <- as.data.frame(fit_check_fn(fit))
    
    # Store results in lists
    all_fit_df[[file]] <- fit_df
    all_fit_pars[[file]] <- fit_pars
    all_fit_check[[file]] <- fit_check
    
    # Free memory
    rm(fit, fit_df, fit_pars, fit_check)
    gc()
  }
  
  # Combine lists into single dataframes
  final_fit_df <- if (length(all_fit_df) > 0) rbindlist(all_fit_df, fill = TRUE) else NULL
  final_fit_pars <- if (length(all_fit_pars) > 0) rbindlist(all_fit_pars, fill = TRUE) else NULL
  final_fit_check <- if (length(all_fit_check) > 0) rbindlist(all_fit_check, fill = TRUE) else NULL
  
  # Write to CSV if there is data
  if (!is.null(final_fit_df)) fwrite(final_fit_df, file = fit_df_path)
  if (!is.null(final_fit_pars)) fwrite(final_fit_pars, file = fit_pars_path)
  if (!is.null(final_fit_check)) fwrite(final_fit_check, file = fit_check_path)
}