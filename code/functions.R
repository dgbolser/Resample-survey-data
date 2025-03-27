##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Resample_survey_data: Multiple species, multiple years
## Authors:       Derek Bolser, Office of Science and Technology (derek.bolser@noaa.gov)
##                Em Markowitz, Alaska Fisheries Science Center (emily.markowitz@noaa.gov)
## Description:   Resample_survey_data: Multiple species, multiple years.
## Date:          March 2025
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Cleanup species function
#'
#' Filter catch by species, create a vector of tows and give tows a random
#' assignment to be resampled,
#'
#' @param df full catch df
#' @param species The species which you want to do data cleanup for.
#' @param tot_dataframes the number of data frames you want to output. effort x replicates - (replicates - 1). 5x3-2
#' @return List of resampled catch data frames
#'
cleanup_by_species <- function(catch, 
                               test_species, 
                               seq_from = 0.1, 
                               seq_to = 1.0, 
                               seq_by = 0.1, 
                               tot_dataframes = 91, 
                               replicate_num = 10) {
  
  df <- catch %>% 
    dplyr::filter(
      Common_name == test_species$common_name)
  
  if (!is.na(test_species$filter_lat_lt) | is.null(test_species$filter_lat_lt)) {
    df <- df %>% dplyr::filter(Latitude_dd < test_species$filter_lat_lt)
  }
  if (!is.na(test_species$filter_lat_gt) | is.null(test_species$filter_lat_gt)) {
    df <- df %>% dplyr::filter(Latitude_dd > test_species$filter_lat_gt)
  }
  if (!is.na(test_species$filter_depth) | is.null(test_species$filter_depth)) {
    df <- df %>% dplyr::filter(Depth_m < test_species$filter_depth)
  }
  
  catch_split <- split(df, df$Year)
  
  tows <- lapply(catch_split, tow_fn)
  
  # Assign random 1s and 0s based on the specified proportions to a list of dataframes
  props <- as.data.frame(seq(from = seq_from, to = seq_to, by = seq_by))
  names(props) <- "Trawl_id"
  
  # match the structure of the catch data
  props <- rep(props, length(tows))
  
  tows_assigned <- map2(tows, props, include_or_exclude)
  # tows_assigned <- purrr::pmap(list(.x = tows, .y = props, .z = replicate_num), include_or_exclude)
  
  # remove replicates of the 1 effort level
  tows_assigned <- lapply(tows_assigned, function(x) {
    x <- x[1:tot_dataframes]
    return(x)
  })
  
  tows_assigned_resampled <- purrr::map(tows_assigned, function(x) {
    purrr::map(x, function(y) {
      y[y$RandomAssignment == 1, ]
    })
  })
  
  tows_assigned_resampled <- unlist(tows_assigned_resampled, recursive = F)
  
  alldata_resampled <- join_dfs(tows_assigned_resampled, df, "Trawl_id")
  
  names(alldata_resampled) <- substr(names(alldata_resampled), 6, 50) # it would be good to replace 50 with a logical indicating the end
  
  species_all_yrs <- alldata_resampled %>%
    bind_rows(.id = "source")
  
  species_all_yrs <- split(species_all_yrs, species_all_yrs$source)
  names(species_all_yrs) <- gsub(x = names(species_all_yrs), pattern = ".", replacement = "", fixed = TRUE)
  
  rm("df", "catch_split", "tows", "props", "tows_assigned", "alldata_resampled")
  
  return(species_all_yrs)
}


resample_tests <- function (spp_dfs, test_species, grid_yrs, dir_out) {
  # set directories for outputs
  dir_spp <- paste0(dir_out, paste0(test_species$srvy, "_", test_species$file_name, "/"))
  dir.create(dir_spp, showWarnings = FALSE)
  
  fit_df <- fit_pars <- fit_check <- index <- data.frame()
  
  spp_dfs <- spp_dfs[names(spp_dfs)[(length(names(spp_dfs))-1):length(names(spp_dfs))]] # reduce DFs for testing
  spp_files <- as.list(names(spp_dfs)) # make the names file
  for (i in seq_along(spp_dfs)) { # Save each dataframe separately
    write_parquet(spp_dfs[[i]], paste0(dir_spp, paste0("df_", i, ".parquet")))
  }
  rm(spp_dfs) # Optional: Remove from memory
  gc()
  
  #set up parallel processing
  plan(callr, workers = 6)  # Adjust the number of workers based on available memory
  
  # Remove large objects before parallel execution
  gc()
  
  print("...Starting parallel SDM processing")
  
  assign(value = get(test_species$model_fn), x = "model_function")
  
  # Run SDMs in parallel
  future_map(seq_along(spp_files), function(i) {
    message(spp_files[[i]])
    gc()  # Free memory
    # Load only the required dataframe
    spp_df <- read_parquet(paste0(dir_spp, paste0("df_", i, ".parquet")))
    # Run species SDM function
    fit0 <- model_function(x = spp_df, y = spp_files[[i]], z = grid_yrs, dir_spp = dir_spp)
    # fit <- readRDS(file = paste0(dir_spp, "fit_", spp_files[[i]], ".rds")) # oor testing
    # Ensure extracted objects are dataframes, Store results in lists
    fit_df <- fit_df %>% 
      dplyr::bind_rows(
        data.frame(
          species = test_species$file_name,
          effort = spp_files[[i]],
          data.frame(fit_df_fn(fit0$fit))))
    fwrite(fit_df, file = paste0(dir_spp, "fit_df.csv"))
    fit_pars <- fit_pars %>% 
      dplyr::bind_rows(
        data.frame(
          species = test_species$file_name,
          effort = spp_files[[i]],
          data.frame(fit_pars_fn(fit0$fit))))
    fwrite(fit_pars, file = paste0(dir_spp, "fit_pars.csv"))
    fit_check <- fit_check %>% 
      dplyr::bind_rows(
        data.frame(
          species = test_species$file_name,
          effort = spp_files[[i]],
          data.frame(fit_check_fn(fit0$fit))))
    fwrite(fit_check, file = paste0(dir_spp, "fit_check.csv"))
    index <- index %>% 
      dplyr::bind_rows(
        data.frame(
          species = test_species$file_name,
          effort = spp_files[[i]],
          data.frame(fit0$index)))
    fwrite(index, file = paste0(dir_spp, "index.csv"))
    # Explicitly remove objects after processing
    rm("fit0", "spp_df")
    gc()
    # NULL
  }, .progress = TRUE, .options = furrr_options(seed = TRUE))
  
  print("...Parallel SDM processing complete")
}

plot_results <- function(srvy, dir_out) {
  
  # create directory for images to be saved to
  dir_fig <- paste0(dir_out, paste0(srvy, "_0figures/"))
  dir.create(dir_fig, showWarnings = FALSE)
  
  # find files
  aaa <- list.files(path = dir_out, pattern = srvy, full.names = TRUE)
  aaa <- aaa[!grepl(pattern = "figures", x = aaa)]
    
  # compile files for each org
  fit_df <- fit_pars <- fit_check <- index <- data.frame()
  for (i in 1:length(aaa)) {
    if (file.exists(paste0(aaa[i], "/fit_df.csv"))) {
      fit_df <- fit_df %>% 
        dplyr::bind_rows(read.csv(paste0(aaa[i], "/fit_df.csv")))
    }    
    if (file.exists(paste0(aaa[i], "fit_pars.csv"))) {
      fit_pars <- fit_pars %>% 
        dplyr::bind_rows(read.csv(paste0(aaa[i], "/fit_pars.csv")))
    }
    if (file.exists(paste0(aaa[i], "/fit_check.csv"))) {
      fit_check <- fit_check %>% 
        dplyr::bind_rows(read.csv(paste0(aaa[i], "/fit_check.csv")))
    }
    if (file.exists(paste0(aaa[i], "/index.csv"))) {
      index <- index %>% 
        dplyr::bind_rows(read.csv(paste0(aaa[i], "/index.csv")))
    }
  }
  
  write.csv(x = fit_df, file = paste0(dir_fig, "/fit_df.csv"))
  write.csv(x = fit_pars, file = paste0(dir_fig, "/fit_pars.csv"))
  write.csv(x = fit_check, file = paste0(dir_fig, "/fit_check.csv"))
  write.csv(x = index, file = paste0(dir_fig, "/index.csv"))
  
  # plotting -------------------------------------------------------------------
  
  plot_list <- c()
  i <- 0
  theme_custom <- theme_bw() + 
    theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
  
  ## log biomass estimates boxplot ---------------------------------------------
  p1 <- ggplot(index, aes(x = as.factor(effort), y = log_est)) +
    geom_boxplot() +
    facet_wrap(~ species) +
    labs(x = "Proprotion of effort",
         y = "Log biomass estimate") +
    theme_minimal() +
    theme_custom
  
  i <- i + 1
  plot_list[[i]] <- p1
  names(plot_list)[i] <- 'index_boxplot_log_biomass.png'
  
  # log(?) SE boxplot ----------------------------------------------------------
  p1 <- ggplot(index, aes(x = as.factor(effort), y = se)) +
    geom_boxplot() +
    facet_wrap(~ species) +
    labs(x = "Proprotion of effort",
         y = "Standard error of log biomass estimate") +
    theme_minimal() +
    theme_custom
  
  i <- i + 1
  plot_list[[i]] <- p1
  names(plot_list)[i] <- 'index_boxplot_log_biomass_SE.png'
  
  # biomass estimates boxplot --------------------------------------------------
  p1 <- ggplot(index, aes(x = as.factor(effort), y = est)) +
    geom_boxplot() +
    facet_wrap(~ species) +
    labs(x = "Proprotion of effort",
         y = "Biomass estimate") +
    theme_minimal() +
    theme_custom
  
  i <- i + 1
  plot_list[[i]] <- p1
  names(plot_list)[i] <- 'index_boxplot_biomass.png'
  
  # biomass estimates over time ---------------------------------------------------
  p1 <- ggplot(index, aes(x = Year, y = est, color = effort)) +
    geom_line() +
    facet_wrap(~ species) +
    labs(x = "Proprotion of effort",
         y = "Biomass estimate") +
    theme_minimal() +
    theme_custom
  
  i <- i + 1
  plot_list[[i]] <- p1
  names(plot_list)[i] <- 'index_timeseries_biomass.png'
  
  for (ii in 1:length(plot_list)) {
  ggsave(filename = names(plot_list)[ii],
         plot = plot_list[[ii]], 
         path = dir_fig, 
         width = 8, 
         height = 8, 
         device = 'png', 
         dpi = 300)
  }
  
  save(plot_list, file = paste0(dir_fig, "figures.rdata"))
}




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



