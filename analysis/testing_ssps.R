
# What does the ssp output look like for Hector?
# Does the data look funky to you? Could Hector be missing a concetnration input?
devtools::load_all()
devtools::load_all('~/Documents/2019/AS/hector/')
library(dplyr)
library(tidyr)
library(ggplot2)

vars <- c(GLOBAL_TEMP(), ATMOSPHERIC_CO2(), ATMOSPHERIC_CH4(), ATMOSPHERIC_N2O(), EMISSIONS_BC(), EMISSIONS_OC(), EMISSIONS_SO2())
#vars <- unique(cmip6_Hector_inputs$concentration.ssp119$variable)

# Figure out all of the concetnration run names
all_runs           <- names(cmip6_Hector_inputs)
conc_ssp_runs <- all_runs[grepl('concentration.ssp', all_runs)]

# Run Hector then format the results using the default hector
# parameter values for the different cmip6 ssps.
lapply(conc_ssp_runs, function(run){

  tryCatch({
    core <- newCMIP6core(run)
    reset(core)
    run(core, runtodate = 2100)
    fetchvars(core, 1850:2100, vars = vars) %>%
      mutate(run = run)

  }, error = function(e){NULL})
}) %>%
  bind_rows() ->
  results

# Plot the Hector results, do they look werid to you?
# why is there a dip between the historical period and the ssp
# period? Would we expect that to be continious? Also
# why are there so many scallops?
ggplot(data = results) +
  geom_line(aes(year, value, color = run))

# Let's checkout the CMIP6 data does it look any different?
cmip6_data %>%
  filter(variable == 'tas') %>%
  filter(model == 'IPSL-CM6A-LR') %>%
  filter(grepl(experiment, pattern = 'concentration')) %>%
  ggplot(aes(year, value, group = interaction(model, experiment, ensemble), color = model)) +
  geom_line()
# Although there is some wiggle room in the time series there the cmip6 temperature
# output is missing the big dip in temp we see in Hector output near 2015.


# What are you using for historical data?
# Can you plot one of the rcps from hector  on top?
# Have we run alexey's concentration converter for the rcps to see if it reproduces hectors concentrations?


rcp_runs <- c(system.file('input/hector_rcp26_constrained.ini', package = 'hector'),
              system.file('input/hector_rcp45_constrained.ini', package = 'hector'),
              system.file('input/hector_rcp60_constrained.ini', package = 'hector'),
              system.file('input/hector_rcp85_constrained.ini', package = 'hector'))

lapply(rcp_runs, function(run){

  tryCatch({
  core <- newcore(inifile = run)
  run(core)
  fetchvars(core, 1850:2100, vars) %>%
    mutate(run = basename(run))

  }, error = function(e){NULL})

}) %>%
  bind_rows() ->
  rcp_rslts


#results <- cmip6_Hector_inputs[["concentration.ssp119"]]
rcp_rslts$era <- 'cmip5'
results$era   <- 'cmip6'
results %>%
  bind_rows(rcp_rslts) %>%
#  filter(era == 'cmip5') %>%
  filter(variable == GLOBAL_TEMP()) %>%
  ggplot(aes(year, value, color = era, group = run)) +
  geom_line() +
  facet_wrap('variable')
  labs(title = 'N2O concentrations')


# How do the results compare to the cmip6 inputs??
results %>%
  rename(hector = value) ->
  hector_results


cmip6_Hector_inputs[names(cmip6_Hector_inputs) %in% hector_results$scenario]  %>%
  bind_rows() %>%
  filter(variable == EMISSIONS_SO2()) %>%
  ggplot(aes(year, value, group = scenario)) +
  geom_line()




cmip6_Hector_inputs[names(cmip6_Hector_inputs) %in% hector_results$scenario] %>%
  bind_rows() %>%
  filter(variable %in% EMISSIONS_BC()) %>%
  mutate(variable = if_else(variable == 'Ca_constrain', 'Ca', variable)) %>%
  rename(inputs = value) %>%
  select(-units) %>%
  mutate(scenario = paste0(driven, '.', scenario)) ->
  hector_inputs


left_join(hector_results, hector_inputs) %>%
  na.omit() %>%
  mutate(dif = inputs - hector) %>%
  group_by(scenario, variable) %>%
  pull(dif) %>%
  summary()





