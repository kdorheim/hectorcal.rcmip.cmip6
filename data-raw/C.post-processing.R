## C.post-processing.R
## Prepare the ESM CMIP6 data to be used as comparison data in Hector calibration.

# 0. Set Up ------------------------------------------------------------------------------
# Load the required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Set up the paths
INPUT_DIR <- here::here('data-raw', 'C.cmip6-outputs')

# Define the scenarios to keep.
scenarios <- c("historical", "ssp126", "ssp245" , "ssp370", "ssp585")

# 1. Import and Format Temp Data ----------------------------------------------------------
# Import the temperature data.
read.csv(file.path(INPUT_DIR, 'cmip6_global_means_tas.csv'), stringsAsFactors = FALSE) %>%
  filter(experiment %in% scenarios) %>%
  # Format a year column
  mutate(year = substr(x = time, start = 1, stop = 4),
         year = as.integer(year)) %>%
  # Determine the global annual average
  group_by(era, year, variable, domain, model, experiment, ensemble) %>%
  summarise(value = mean(value)) %>%
  ungroup ->
  cmip6_tas

# Determine the reference preindustrial temperature for each model.
cmip6_tas %>%
  filter(year <= 1860) %>%
  group_by(variable, domain, model, ensemble) %>%
  summarise(preInd = mean(value)) %>%
  ungroup ->
  preindustiral_temp

# Remove the reference temperature from model results to convert from
# absolute temperature to temperature anomaly.
cmip6_tas %>%
  left_join(preindustiral_temp, by = c("variable", "domain", "model", "ensemble")) %>%
  mutate(value = value - preInd) %>%
  na.omit %>%
  select(era, year, value, variable, domain, model, experiment,
         ensemble) %>%
  mutate(unit = 'C') %>%
  mutate(experiment = if_else(experiment == 'historical', 'concentration.historical-cmip6', paste0('concentration.', experiment))) %>%
  select(era, year, value, variable, model, experiment, ensemble) ->
  formatted_tas

# Remove the outlier data in the historical data.
formatted_tas %>%
  mutate(keep = TRUE) %>%
  mutate(keep = if_else(experiment == 'concentration.historical-cmip6' & year > 2000 & value <= 0.25, FALSE, TRUE)) %>%
  filter(keep) %>%
  select(-keep) ->
  formatted_tas

# 2. Import and Format Ocean Heat Flux Data ------------------------------------------------
# Import the heat flux data.
read.csv(file.path(INPUT_DIR, 'cmip6_global_means_hfds.csv'), stringsAsFactors = FALSE) %>%
  # Add a year column.
  filter(experiment %in% scenarios) %>%
  mutate(year = substr(x = time, start = 1, stop = 4),
         year = as.integer(year)) %>%
  # Determine the global annual average
  group_by(era, year, variable, domain, model, experiment, ensemble) %>%
  summarise(value = mean(value)) %>%
  ungroup %>%
  mutate(experiment = if_else(experiment == 'historical', 'concentration.historical-cmip6', paste0('concentration.', experiment))) %>%
  select(era, year, value, variable, model, experiment, ensemble) %>%
  # Change the variable name to match the Hector variable name.
  mutate(variable = 'heatflux') ->
  formatted_HF

# Remove the heat flux outliers.
formatted_HF %>%
  filter(value >= - 4 & value < 10) ->
  formatted_HF

# 3. Save Data ----------------------------------------------------------------------
# Concatenate temperature and heat flux data together and
# only keep values up until 2100.
formatted_tas %>%
  bind_rows(formatted_HF) %>%
  filter(year <= 2100) ->
  cmip6_data

usethis::use_data(cmip6_data, overwrite = TRUE)
