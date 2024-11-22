# # survey catch already extracted and filtered for the index
catch <- nwfscSurvey::pull_catch(
  common_name = "petrale sole",
  survey = "NWFSC.Combo"
)


# extract survey biological samples
bio <- nwfscSurvey::pull_bio(
  common_name = "petrale sole",
  survey = "NWFSC.Combo",
)
saveRDS(catch, "data-raw/petrale_catch.rds")
saveRDS(bio, "data-raw/petrale_bio.rds")
bio <- readRDS("data-raw/petrale_bio.rds")
catch <- readRDS("data-raw/petrale_catch.rds")

# apply vector of tows for each run to the bio data
# TODO: add the filtering using cleanup_by_species()
# temporarily using full data set
bio_filtered <- bio
catch_filtered <- catch

# strata from petrale assessment
# TODO: needs to be species-specific
strata <- nwfscSurvey::CreateStrataDF.fn(
  names = c("shallow_s", "mid_s", "shallow_n", "mid_n"),
  depths.shallow = c(55, 200, 55, 200),
  depths.deep = c(200, 400, 200, 400),
  lats.south = c(32, 32, 42, 42),
  lats.north = c(42, 42, 49, 49)
)

# read model input files
# temporarily hardwire for petrale
dir <- "ss3/petrale/original/"
ss3_inputs <- r4ss::SS_read(dir)
# get fleet number for WCGBTS (TODO: will be model-specific)
fleet_number <- which(ss3_inputs$dat$fleetnames == "WCGBTS")

# calculate length compositions from resampled survey data
len_comp_new <- nwfscSurvey::get_expanded_comps(
  bio_data = bio_filtered,
  catch_data = catch_filtered,
  comp_bins = ss3_inputs$dat$lbin_vector,
  comp_column_name = "Length_cm",
  strata = strata
)
input_n <- nwfscSurvey::get_input_n(
  data = bio_filtered,
  species_group = "flatfish" # TODO: species specific setting
)
len_comp_new <- len_comp_new$sexed
# change capitalization and a few headers to match r4ss notation
names(len_comp_new) <- tolower(names(len_comp_new))
len_comp_new <- len_comp_new |> dplyr::rename(part = "partition", Nsamp = "input_n")

# modify length data
age_comp_new$fleet <- fleet_number
age_comp_new$month <- 7
age_comp_new$Nsamp <- input_n |>
  dplyr::filter(sex_grouped == "sexed") |>
  dplyr::pull(input_n)


# conditional-age-at-length comps
caal <- nwfscSurvey::SurveyAgeAtLen.fn(
  datAL = bio_filtered,
  datTows = catch_filtered,
  strat.df = strata,
  lgthBins = ss3_inputs$dat$lbin_vector,
  ageBins = ss3_inputs$dat$agebin_vector
)
names(caal$female) <- names(ss3_inputs$dat$agecomp)
names(caal$male) <- names(ss3_inputs$dat$agecomp)
caal <- rbind(caal$female, caal$male)
caal$month <- 7
caal$fleet <- fleet_number
# figure out year-specific ageing error type
# (petrale may be only species with multiple types due to WDFW ageing the survey fish in a few years)
for (y in unique(caal$year)) {
  ageerr_y <- ss3_inputs$dat$agecomp |>
    dplyr::filter(year == y & fleet == fleet_number) |>
    dplyr::pull(ageerr) |>
    unique()
  caal$ageerr[caal$year == y] <- ageerr_y
}

# update length comps in the model
ss3_inputs$dat$lencomp <-
  rbind(
    ss3_inputs$dat$lencomp |> dplyr::filter(fleet != fleet_number), # leave all other as they were
    len_comp_new # new length comps for WCGBTS fleet
  )

# update age comps in the model
ss3_inputs$dat$agecomp <-
  rbind(
    ss3_inputs$dat$agecomp |> dplyr::filter(abs(fleet) != fleet_number), # leave all other as they were
    caal # new conditional-age-at-length comps for WCGBTS fleet
  )


# write the modified SS3 files
r4ss::SS_write(ss3_inputs, dir = file.path(dir, "../full"), overwrite = TRUE)

# run SS3
r4ss::run(file.path(dir, "../full"))

# get output and summarize
