##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Resample_survey_data: Multiple species, multiple years
## Authors:       Em Markowitz, Alaska Fisheries Science Center (emily.markowitz@noaa.gov)
## Description:   Resample_survey_data: Multiple species, multiple years.
## Date:          March 2025
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Install Libraries ------------------------------------------------------------

# Here we list all the packages we will need for this whole process
# We'll also use this in our works cited page!!!
PKG <- c(
  "devtools", 
  "plyr",
  "dplyr",
  "magrittr",
  "tidyr",
  "here",
  "readr",
  "tibble",
  "janitor", 
  "data.table", 
  "jsonlite", 
  "httr", 
  "sp", 
  "RODBC"
)

PKG <- unique(PKG)
for (p in PKG) {
  if(!require(p, character.only = TRUE)) {
    if (p == 'coldpool') {
      devtools::install_github("afsc-gap-products/coldpool")
    } else if (p == "akgfmapas") {
      devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
    } else {
      install.packages(p)
    }
    require(p,character.only = TRUE)}
}

# CPUE/Catch Load data ---------------------------------------------------------

## Load haul data --------------------------------------------------------------
api_link_haul <- 'https://apps-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey_haul/'

dat <- data.frame()
for (i in seq(0, 500000, 10000)){
  ## find how many iterations it takes to cycle through the data
  print(i)
  ## query the API link
  res <- httr::GET(url = paste0(api_link_haul, "?offset=",i,"&limit=10000"))
  ## convert from JSON format
  data <- jsonlite::fromJSON(base::rawToChar(res$content)) 
  
  ## if there are no data, stop the loop
  if (is.null(nrow(data$items))) {
    break
  }
  
  ## bind sub-pull to dat data.frame
  dat <- dplyr::bind_rows(dat, 
                          data$items %>%
                            dplyr::select(-links)) # necessary for API accounting, but not part of the dataset)
}

dat_haul <- dat

## Load species data --------------------------------------------------------------
api_link_species <- 'https://apps-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey_species/'

## query the API link
res <- httr::GET(url = paste0(api_link_species, "?offset=0&limit=10000"))
## convert from JSON format
data <- jsonlite::fromJSON(base::rawToChar(res$content)) 

## bind sub-pull to dat data.frame
dat_species <- data$items %>%
  dplyr::select(-links) # necessary for API accounting, but not part of the dataset)

## Load catch data from FOSS ---------------------------------------------------
api_link_catch <- 'https://apps-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey_catch/'

dat <- data.frame()
for (ii in 1:nrow(test_species)) {
  print(test_species$common_name[ii])
  for (i in seq(0, 1000000, 10000)){
    ## find how many iterations it takes to cycle through the data
    print(i)
    ## query the API link
    # res <- httr::GET(url = paste0(api_link_catch, "?offset=",i,"&limit=10000"))
    # res <- httr::GET(url = paste0(api_link_haul, '?limit=10000&q={"species_code":2023,"srvy":"EBS"}'))
    res <- httr::GET(url = paste0(api_link_catch, 
                                  '?offset=',i,'&limit=10000&q={"species_code":',test_species$species_code[ii],'}'))
    # res <- httr::GET(url = 'https://apps-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey_catch/?offset=0&limit=10000&q={%22species_code%22:21740,%22species_code%22:21720}')
    ## convert from JSON format
    data <- jsonlite::fromJSON(base::rawToChar(res$content)) 
    
    ## if there are no data, stop the loop
    if (is.null(nrow(data$items))) {
      break
    }
    
    ## bind sub-pull to dat data.frame
    dat <- dplyr::bind_rows(dat, 
                            data$items %>%
                              dplyr::select(-links)) # necessary for API accounting, but not part of the dataset)
  }
}
dat_catch <- dat

## Zero filled data ------------------------------------------------------------

noaa_afsc_cpue <- dplyr::full_join(
  # find all species that have been caught, by survey
  x = dplyr::left_join(dat_catch, dat_haul, by = "hauljoin") %>%
    dplyr::select(survey_definition_id, species_code) %>%
    dplyr::distinct(),
  # find all haul events (hauljoins), by survey
  y = dat_haul %>%
    dplyr::select(survey_definition_id, hauljoin) %>%
    dplyr::distinct(),
  relationship = "many-to-many",
  by = "survey_definition_id"
) %>% 
  dplyr::select(-survey_definition_id) %>% # now, redundant
  # get full data 
  dplyr::left_join(dat_catch) %>% 
  dplyr::left_join(dat_haul)  %>% 
  dplyr::left_join(dat_species %>% 
                     dplyr::select(species_code, common_name) %>% 
                     dplyr::mutate(species_code = as.numeric(species_code))) %>% 
  dplyr::filter(srvy == "EBS") %>% 
  dplyr::mutate(
    Pass = 1, 
                Area_swept_ha = area_swept_km2/100) %>%
  dplyr::select(
    srvy, 
    Trawl_id = hauljoin, 
    Common_name = common_name, 
    species_code, 
    total_catch_numbers = count, 
    total_catch_wt_kg = weight_kg, 
    Latitude_dd = latitude_dd_start, 
    Longitude_dd = longitude_dd_start, 
    Year = year, 
    Area_swept_ha, 
    Pass, 
    depth_m)

save(noaa_afsc_cpue, file = paste0(wd, "/data/noaa_afsc_cpue.rda"))

# Extrapolation grid -----------------------------------------------------------

load(paste0(wd,"data/pred_grid_ebs.rda"))
crs_latlon <- "+proj=longlat +datum=WGS84" # decimal degrees

noaa_afsc_ebs_pred_grid <- pred_grid_ebs %>% 
  dplyr::rename(Longitude_dd = lon, 
                Latitude_dd = lat) %>% 
  dplyr::mutate(Pass = 1, # original Shape_area is in m^2
    # sx = ((lon - mean(lon, na.rm = TRUE))/1000),
    # sy = ((lat - mean(lat, na.rm = TRUE))/1000), 
    area_km2 = Shape_Area*0.0001) %>% 
  dplyr::select(-Shape_Area) %>% 
  dplyr::mutate(srvy = "EBS")

sp_extrap_raster <- SpatialPoints(
  coords = coordinates(as.matrix(noaa_afsc_ebs_pred_grid[,c("Longitude_dd", "Latitude_dd")])), 
  proj4string = CRS(crs_latlon) )

## Add depth -------------------------------------------------------------------

x <- noaa_afsc_cpue %>%
  dplyr::filter(Year > 2000) %>% # lets assume our ability to assess depth has improved, technologically since before 2000. Year negotiable if anyone has strong opinions
  dplyr::select(Longitude_dd, Latitude_dd, depth_m) %>%
  stats::na.omit()  %>% 
  sf::st_as_sf(x = ., 
               coords = c(x = "Longitude_dd", y = "Latitude_dd"), 
               crs = sf::st_crs(crs_latlon))

idw_fit <- gstat::gstat(formula = depth_m ~ 1,
                        locations = x,
                        nmax = 4)

noaa_afsc_ebs_depth_raster <- raster::predict(
  idw_fit, sp_extrap_raster) %>%
  # as(sp_extrap_raster, Class = "SpatialPoints")) %>%
  sf::st_as_sf() %>%
  sf::st_transform(crs = crs_latlon)  %>%
  stars::st_rasterize() 

# Just so we can see what we are looking at:
# plot(extrap_data0, main = "Interpolated Bottom Depths") 

save(noaa_afsc_ebs_depth_raster, file = paste0("data/noaa_afsc_ebs_depth_raster.rdata"))

noaa_afsc_ebs_pred_grid_depth <- stars::st_extract(
  x = noaa_afsc_ebs_depth_raster,
  at = as.matrix(noaa_afsc_ebs_pred_grid[,c("Longitude_dd", "Latitude_dd")])) %>% 
  dplyr::bind_cols(noaa_afsc_ebs_pred_grid) %>% 
  dplyr::rename(Depth_m = var1.pred) %>% 
  dplyr::select(-var1.var) %>%
  stats::na.omit()

save(noaa_afsc_ebs_pred_grid_depth, file = paste0("data/noaa_afsc_ebs_pred_grid_depth.rdata"))

## Add temperature -------------------------------------------------------------

# ### Coldpool temperature data --------------------------------------------------
# 
# ### Data that varies over space and time (bottom temperature)
# 
# # Here, bottom temperature, and thereby the cold pool extent, have been show to drive the distribution of many species. This is especially true for walleye pollock. 
# # For this we are going to lean on our in-house prepared validated and pre-prepared [{coldpool} R package](https://github.com/afsc-gap-products/coldpool) (S. Rohan, L. Barnett, and N. Charriere). This data interpolates over the whole area of the survey so there are no missing data. 
# 
# noaa_afsc_ebs_pred_grid_temperature <-
#   dplyr::bind_cols(terra::unwrap(coldpool::ebs_bottom_temperature) %>%
#   terra::project(crs_latlon) %>%
#   terra::extract(st_as_sf(sp_extrap_raster)))
# 
# names(noaa_afsc_ebs_pred_grid_temperature) <- paste0("GEAR_TEMPERATURE_", names(noaa_afsc_ebs_pred_grid_temperature))
# 
# noaa_afsc_ebs_pred_grid_depth_temperature <-
#   dplyr::bind_cols(noaa_afsc_ebs_pred_grid_depth,
#                    noaa_afsc_ebs_pred_grid_temperature)
# 
# save(noaa_afsc_ebs_pred_grid_depth_temperature, file = paste0("data/noaa_afsc_ebs_pred_grid_depth_temperature.rdata"))

# Load Design-based biomass data for comparison --------------------------------

locations <- c("Z:/Projects/ConnectToOracle.R")
for (i in 1:length(locations)){
  if (file.exists(locations[i])){
    source(locations[i])
  }
}

noaa_afsc_biomass_estimates <- RODBC::sqlQuery(
  channel = channel_products, 
  query = 
    paste0("SELECT DISTINCT 
bb.SURVEY_DEFINITION_ID,
bb.AREA_ID,
bb.SPECIES_CODE,
bb.YEAR,
bb.BIOMASS_MT,
bb.BIOMASS_VAR,
bb.POPULATION_COUNT,
bb.POPULATION_VAR
FROM GAP_PRODUCTS.AKFIN_BIOMASS bb

WHERE bb.AREA_ID IN (99901, 99902)
AND bb.SPECIES_CODE IN (", paste0(test_species$species_code, collapse = ","),") 
")) %>% 
  #   -- WHERE bb.SURVEY_DEFINITION_ID = 98 
  # -- AND bb.SPECIES_CODE IN (21740, 10210, 69322) 
  # -- AND AREA_ID = 99901
  # -- AND bb.YEAR >= 1982
  janitor::clean_names()

# Save table to local directory
save(noaa_afsc_biomass_estimates, file = paste0(wd, "data/noaa_afsc_biomass_estimates.rda"))

