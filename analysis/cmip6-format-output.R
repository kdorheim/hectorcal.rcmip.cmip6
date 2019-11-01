## cmip6-format-output.R
## Concatenate and visulaize the calibration results. How well do the
## fitted values emulate ESM output.

# 0. Set Up -------------------------------------------------
# Load required libs
library(dplyr)
library(tidyr)
library(ggplot2)
library(hectorcal)

devtools::load_all()
devtools::load_all('~/Documents/2019/AS/hector/')

# Set up the directories.
INPUT_DIR  <- here::here('analysis', 'output-sspTempHF')
OUTPUT_DIR <- here::here('analysis')
FIGS_DIR   <- here::here('analysis', 'figs'); dir.create(FIGS_DIR)


# 1. Import Data -------------------------------------------------
# Format the calibration results into a sinlge table. This is the
# table to share for rcmip parameterizations and will be used to
# paramterize Hector to check the calibration results.
list.files(INPUT_DIR, full.names = TRUE) %>%
  lapply(function(input){

    object <- readRDS(input)

    data.frame(t(object$par)) %>%
      mutate(convergence = object$convergence,
             model = object$model)

  }) %>%
  bind_rows() ->
  parameter_fits

# Only save the calibration results that were able to converge.
parameter_fits %>%
  filter(convergence == 0) ->
  parameter_fits

write.csv(parameter_fits, file = file.path(OUTPUT_DIR, 'cmip6_paramter_fits.csv'), row.names = FALSE)

all_runs <- names(cmip6_Hector_inputs)
runs     <- all_runs[grepl(pattern = 'concentration.ssp', x = all_runs)]
cores    <- lapply(runs, newCMIP6core)


# 2. Use Calibration Results -------------------------------------------------
# Use the fitted paramter values in Hector to emulate ESM data.
apply(parameter_fits, MARGIN = 1, function(input){

  params        <- as.numeric(c(input[['S']], input[['diff']], input[['alpha']], input[['volscl']]))
  names(params) <- c('S', 'diff', 'alpha', 'volscl')


  bind_rows(lapply(cores, function(c){

    parameterize_core(params = params, core = c)
    reset(core = c)
    run(core = c, runtodate = 2100)
    fetchvars(core = c, dates = 1850:2100, vars = c(GLOBAL_TEMP(), HEAT_FLUX())) %>%
      mutate(model = input[['model']])

  }))

}) %>%
  bind_rows() %>%
  mutate(experiment = scenario) %>%
  mutate(experiment = if_else(year <= 2014, "concentration.historical-cmip6", experiment)) %>%
  mutate(variable = if_else(variable == GLOBAL_TEMP(), 'tas', variable)) ->
  emulated_hector_results

# Combine Hector and ESM data.
emulated_hector_results %>%
  rename(hector = value) %>%
  left_join(cmip6_data, by = c('experiment', 'model', 'year', 'variable')) %>%
  na.omit() ->
  hector_cmip_data

# Plot the Hector vs ESM data.
split(hector_cmip_data, hector_cmip_data$model) %>%
  lapply(function(input){
    ggplot(input) +
      geom_line(aes(year, value, color = 'cmip6', group = interaction(variable, experiment, ensemble))) +
      geom_line(aes(year, hector, color = 'hector', group = interaction(variable, experiment, ensemble))) +
      facet_wrap('variable') +
      labs(title = unique(input$model))
  }) ->
  plots

# Save the plots as pdf files.
mapply(function(input, name){

  ggsave(input, file = file.path(FIGS_DIR, paste0(name, '.pdf')))

},
       input = plots, name = names(plots))




