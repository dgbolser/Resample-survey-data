#' Species distribution model function
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Used for arrowtooth flounder, bocaccio, dover sole, lingcod north, lingcod
#' south, longnose skate, pacific ocean perch (pop), pacific spiny dogfish, rex
#' sole, yellowtail.
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-processing
#' of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @import sdmTMB
#'
species_sdm_fn <- function(x, y) {
  # make mesh
  mesh <- sdmTMB::make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 500)

  # fit model
  fit <- sdmTMB::sdmTMB(
    total_catch_wt_kg ~ 0 + factor(Year) + Pass,
    data = x,
    mesh = mesh,
    family = delta_gamma(),
    time = "Year",
    anisotropy = TRUE,
    spatiotemporal = as.list(c("iid", "iid")),
    do_index = TRUE,
    predict_args = list(newdata = x),
    index_args = list(area = 4) # this should be based on actual values?
  )

  # get the index
  index <- sdmTMB::get_index(fit, bias_correct = TRUE)

  # save file
  saveRDS(fit, paste0("fit_", y, ".rds"))
  saveRDS(index, paste0("index_", y, ".rds"))
}


#' Species distribution model function using delta_lognormal family
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Used for petrale sole, sablefish.
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-processing
#' of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @import sdmTMB
#'
species_sdm_lognormal_fn <- function(x, y) {
  # make mesh
  mesh <- sdmTMB::make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 500)

  # fit model
  fit <- sdmTMB::sdmTMB(
    total_catch_wt_kg ~ 0 + factor(Year) + Pass,
    data = x,
    mesh = mesh,
    family = delta_lognormal(),
    time = "Year",
    anisotropy = TRUE,
    spatiotemporal = as.list(c("iid", "iid")),
    do_index = TRUE,
    predict_args = list(newdata = x),
    index_args = list(area = 4) # this should be based on actual values?
  )

  # get the index
  index <- sdmTMB::get_index(fit, bias_correct = FALSE) #these are running very slow, so turning bias correction off for now. 

  # save file
  saveRDS(fit, paste0("fit_", y, ".rds"))
  saveRDS(index, paste0("index_", y, ".rds"))
}

#' Canary species distribution model function
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Different from species_sdm_lognormal_fn in that the spatiotemporal
#' list is c("iid", "off").
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-processing
#' of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @import sdmTMB
#'
canary_sdm_fn <- function(x, y) {
  # make mesh
  mesh <- sdmTMB::make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 200)

  # fit model
  fit <- sdmTMB::sdmTMB(
    total_catch_wt_kg ~ 0 + factor(Year) + Pass,
    data = x,
    mesh = mesh,
    family = delta_lognormal(),
    time = "Year",
    anisotropy = FALSE,
    spatiotemporal = as.list(c("iid", "off")),
    do_index = TRUE,
    predict_args = list(newdata = x),
    index_args = list(area = 4) # this should be based on actual values?
  )

  # get the index
  index <- sdmTMB::get_index(fit, bias_correct = TRUE)

  # save file
  saveRDS(fit, paste0("fit_", y, ".rds"))
  saveRDS(index, paste0("index_", y, ".rds"))
}

#' Darkbloched species distribution model function
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Different from species_sdm_lognormal_fn in that the spatiotemporal
#' list is c("off", "iid").
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-processing
#' of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @import sdmTMB
#'
darkblotched_sdm_fn <- function(x, y) {
  # make mesh
  mesh <- sdmTMB::make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 250)

  # fit model
  fit <- sdmTMB::sdmTMB(
    total_catch_wt_kg ~ 0 + factor(Year) + Pass,
    data = x,
    mesh = mesh,
    family = delta_lognormal(),
    time = "Year",
    anisotropy = TRUE,
    spatiotemporal = as.list(c("off", "iid")),
    do_index = TRUE,
    predict_args = list(newdata = x),
    index_args = list(area = 4) # this should be based on actual values?
  )

  # get the index
  index <- sdmTMB::get_index(fit, bias_correct = TRUE)

  # save file
  saveRDS(fit, paste0("fit_", y, ".rds"))
  saveRDS(index, paste0("index_", y, ".rds"))
}

#' Shortspine species distribution model function
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Different from species_sdm_lognormal_fn in that the model contains
#' Depth_m and Depth_m^2
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-processing
#' of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @import sdmTMB
#'
shortspine_sdm_fn <- function(x, y) {
  # make mesh
  mesh <- sdmTMB::make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 500)

  # fit model
  fit <- sdmTMB::sdmTMB(
    total_catch_wt_kg ~ 0 + factor(Year) + Pass + Depth_m + (Depth_m^2),
    data = x,
    mesh = mesh,
    family = delta_lognormal(),
    time = "Year",
    anisotropy = TRUE,
    spatiotemporal = as.list(c("iid", "iid")),
    do_index = TRUE,
    predict_args = list(newdata = x),
    index_args = list(area = 4) # this should be based on actual values?
  )

  # get the index
  index <- sdmTMB::get_index(fit, bias_correct = FALSE) # turn bias correct off because this might be causing model to run for a long time

  # save file
  saveRDS(fit, paste0("fit_", y, ".rds"))
  saveRDS(index, paste0("index_", y, ".rds"))
}


#' Shortspine species distribution model function
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Different from species_sdm_fn in that the mesh is 200 n_knots and the
#' spatiotemporal list is c("off", "off")
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-processing
#' of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @import sdmTMB
#'
widow_sdm_fn <- function(x, y) {
  # make mesh
  mesh <- sdmTMB::make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 200)

  # fit model
  fit <- sdmTMB::sdmTMB(
    total_catch_wt_kg ~ 0 + factor(Year) + Pass, # fyear and pass_scaled were specified in the configs doc.
    data = x,
    mesh = mesh,
    family = delta_gamma(),
    time = "Year",
    anisotropy = TRUE,
    spatiotemporal = as.list(c("off", "off")),
    do_index = TRUE,
    predict_args = list(newdata = x),
    index_args = list(area = 4) # this should be based on actual values?
  )

  # get the index
  index <- sdmTMB::get_index(fit, bias_correct = TRUE)

  # save file
  saveRDS(fit, paste0("fit_", y, ".rds"))
  saveRDS(index, paste0("index_", y, ".rds"))
}
