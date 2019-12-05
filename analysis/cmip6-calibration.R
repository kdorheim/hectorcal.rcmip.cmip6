## cmip6-calibration.R
## Use CMIP6 ESM data to calibrate Hector.

# 0. SXet Up ----------------------------------------------
# Load required libs
library(hectorcal)
library(dplyr)
library(tidyr)
devtools::load_all()
devtools::load_all('~/Documents/2019/AS/hector/')

# Set up the directories
OUTPUT_DIR  <- here::here('analysis', 'output-sspTempHF'); dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Define the parameters to fit
fit_params <- c(hector::ECS(), hector::DIFFUSIVITY(), hector::AERO_SCALE(), hector::VOLCANIC_SCALE())

# 1. Prepare the ESM Comparison Data -----------------------------
# The models that have historical temp.
hectorcal.rcmip.cmip6::cmip6_data %>%
  filter(experiment == 'concentration.historical-cmip6') %>%
  pull(model) %>%
  unique() ->
  temp_historical_models

hectorcal.rcmip.cmip6::cmip6_data %>%
  filter(experiment != 'concentration.historical-cmip6') %>%
  pull(model) %>%
  unique() ->
  temp_ssp_models

complete_temp <- intersect(temp_historical_models, temp_ssp_models)

hectorcal.rcmip.cmip6::cmip6_data %>%
  filter(model %in% complete_temp) %>%
  filter(experiment != 'concentration.historical-cmip6') %>%
  filter(variable == 'heatflux') %>%
  pull(model) %>%
  unique() ->
  hfds_models

models_to_calibrate <- intersect(temp_ssp_models, hfds_models)

hectorcal.rcmip.cmip6::cmip6_data %>%
  filter(model %in% models_to_calibrate) %>%
 filter(experiment != 'concentration.historical-cmip6') ->
  esm_comparison_data

esm_data_list <- split(esm_comparison_data, esm_comparison_data$model, drop = TRUE)


# 2. Calibrate Hector -----------------------------

# Set up the Hector cores to run
all_runs <- names(cmip6_Hector_inputs)
runs     <- all_runs[grepl(pattern = 'concentration.ssp|concentration.historical-cmip6', x = all_runs)]

# Calibrate Hector to the CMIP6 comparison data and save results.
lapply(names(esm_data_list), function(esm){

  best_guess <- c( 3, 2.3, 1, 0)
  names(best_guess) <- c('S', 'diff', 'alpha', 'volscl')

  rslt <- single_calibration_cmip6(cmip6_runs = runs,
                                   esm_data = esm_data_list[[esm]],
                                   normalize = cmip6_center_scale,
                                   initial_param = best_guess,
                                   cmip_range = NULL,
                                   param_penalty = NULL,
                                   maxit = 600,
                                   n_parallel = 1,
                                   showMessages = FALSE)


  rslt$comparison_data <- esm_data_list[[esm]]
  rslt$model           <- esm

  filename <- paste0(esm, '.rds')
  saveRDS(rslt, file = file.path(OUTPUT_DIR, filename))
})




