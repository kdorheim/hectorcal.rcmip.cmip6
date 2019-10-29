## B2.Ensemble_center_scale.R
## Using the large ensembleof Hector output calcualte the center and scale information
## for the runs.

# 0. Set Up ---------------------------------------------------------------------------------
# Load the required libraries
library(dplyr)
library(tidyr)

# Define the number of sucessful runs to keep in the ensemble, some runs terminate early.
n_runs <- 1000

# 1. Import Hector Ensembles --------------------------------------------------------------
here::here('data-raw', 'B.Hector_enesmble_runs') %>%
  list.files(full.names = TRUE) %>%
  lapply(readRDS) ->
  ensemble_results


# First we have to determine which runs actually worked and which terminate early because
# of bad parameter values.
ensemble_results %>%
  lapply(function(input){
    # Drop the NA entries, these are the runs that did not make it to 2100.
    input %>%
      na.omit %>%
      select(scenario, run) %>%
      distinct() %>%
      mutate(keep = TRUE)
  }) %>%
  bind_rows() %>%
  spread(scenario, keep) %>%
  na.omit ->
  good_runs

# Keep the first set of the good runs
runs_to_keep         <- good_runs$run[1:n_runs]
good_hector_ensemble <- bind_rows(lapply(ensemble_results, function(input){filter(input, run %in% runs_to_keep)}))


# For wahtever reason it looks like the Rcmip histoircal inputs stop at 2005 but they
# should continue to 2014. Save the historical values from one of the ssp runs
# instead.
good_hector_ensemble %>%
  filter(scenario != 'concentration.historical-cmip6') ->
  without_historical

good_hector_ensemble %>%
  filter(scenario == 'concentration.ssp370') %>%
  filter(year <= 2014) %>%
  mutate(scenario = 'concentration.historical-cmip6') ->
  complete_historical_data

# Combine the historical and non historical ensemble
# back together.
without_historical %>%
  bind_rows(complete_historical_data) %>%
  group_by(scenario, year, variable) %>%
  # Caluclate the per year / scenario / variable mean and
  # sd. These values will be used to standardize the
  # Hector and ESM output in the single ESM calibration.
  summarise(center = mean(value),
            scale = sd(value)) %>%
  ungroup %>%
  mutate(variable = if_else(variable == hector::GLOBAL_TEMP(), 'tas', variable)) %>%
  arrange(scenario, variable, year) %>%
  # Make sure the the name column consists of strings with
  # the following naming pattern: scenario.varible.year, any
  # sort of spaces cause the single esm calibration function to error out!
  mutate(name = paste0(scenario, '.', variable, '.', year)) ->
  center_scale_df

# Format the center and scale values into a list and save as internal
# package data.
center_values        <- center_scale_df$center
names(center_values) <- center_scale_df$name

scale_values        <- center_scale_df$scale
names(scale_values) <- center_scale_df$name
cmip6_center_scale  <- list(center = center_values, scale = scale_values)

usethis::use_data(cmip6_center_scale, overwrite = TRUE)




