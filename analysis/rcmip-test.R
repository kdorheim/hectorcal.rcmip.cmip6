## rcmip-test.R
## This script tests the ability to use the hectorcal functions to
## to calibrate Hector to emulate the CMIP6 runs.

# 0. Set up ----------------------------------------
# Load required libraries
library(hectorcal)
library(dplyr)
library(tidyr)
# Make sure that the correct version of Hector is loaded!
# Must be the branch from AS that includes the ability to
# perscribe GHG concentrations.
devtools::load_all('~/Documents/2019/AS/hector/')

# Create the output dir.
OUTPUT_DIR <- here::here('analysis', 'rcmip-test')
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Define the parameter to fit.
fit_params <- c(hector::ECS(), hector::DIFFUSIVITY(), hector::AERO_SCALE(), hector::VOLCANIC_SCALE())



# 1. Calibration 1  ----------------------------------------
# Select the esm data.
cmip6_data %>%
  filter(experiment %in% to_calibrate) %>%
  filter(model == 'IPSL-CM6A-LR' & variable == 'tas') ->
  esm_data

# Define the inital parameter values
initial_param <- c(3, 2.3, 1, 1)
names(initial_param) <- fit_params

# Set up the cmip6 hector cores.
all_runs     <- names(cmip6_Hector_inputs)
to_calibrate <- all_runs[all_runs %in% unique(cmip6_data$experiment)]
cores <- lapply(to_calibrate, newCMIP6core)

# Make the to minimize function.
fn <- hectorcal::make_minimize_function(hector_cores = cores,
                             esm_data = esm_data,
                             normalize = cmip6_center_scale,
                             param = initial_param,
                             showMessages = TRUE,
                             intermediateOutput = FALSE)

# Find the param values that will minimze the distance
# between Hector and CMIP6.
out <- stats::optim(par = initial_param, fn = fn)
saveRDS(out, file = file.path(OUTPUT_DIR, 'ipsl_temp.rds'))
# The temp only calibration protocol did not converge which is not surprising considering what
# we saw during the CMIP5 calibration experiments.

# 2. Calibration 2  ----------------------------------------
# Now try calibrating with a combination of temp and heat flux dataa/
cmip6_data %>%
  filter(experiment %in% to_calibrate) %>%
  filter(model == 'IPSL-CM6A-LR') %>%
  mutate(variable = if_else(variable == 'hfds', 'heatflux', variable)) ->
  esm_data

# Define the inital parameter values (I selected these because they
# are around default Hector).
initial_param <- c(3, 2.3, 1, 1)
names(initial_param) <- fit_params

# Set up the cmip6 hector cores.
all_runs     <- names(cmip6_Hector_inputs)
to_calibrate <- all_runs[all_runs %in% unique(cmip6_data$experiment)]
cores <- lapply(to_calibrate, newCMIP6core)

# Make the to minimize function.
fn <- hectorcal::make_minimize_function(hector_cores = cores,
                                        esm_data = esm_data,
                                        normalize = cmip6_center_scale,
                                        param = initial_param,
                                        showMessages = TRUE,
                                        intermediateOutput = FALSE)

# Find the param values that will minimze the distance
# between Hector and CMIP6.
out <- stats::optim(par = initial_param, fn = fn)
saveRDS(out, file = file.path(OUTPUT_DIR, 'ipsl_temp_heatflux.rds'))


# Plot the calibration restults vs the ESM training data!
params <- initial_param
names(params) <- names(initial_param)

lapply(cores[2:5], reset)

lapply(cores, hectorcal::parameterize_core, params = params)
lapply(cores[2:5], reset)
lapply(cores[2:5], run)


lapply(cores[2:5], function(input){
  fetchvars(input, dates = 1850:2100, vars = c(GLOBAL_TEMP(), HEAT_FLUX())) %>%
    mutate(variable = if_else(variable == GLOBAL_TEMP(), 'tas', 'heatflux'))
}) %>%
  bind_rows() ->
  hector_rslts

ggplot() +
  geom_point(data = esm_data, aes(year, value)) +
  geom_point(data = hector_rslts, aes(year, value, color = 'red')) +
  facet_wrap('variable')


