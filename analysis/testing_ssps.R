
# What does the ssp output look like for Hector?
# Does the data look funky to you? Could Hector be missing a concetnration input?
devtools::load_all()
devtools::load_all('~/Documents/2019/AS/hector/')
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
    fetchvars(core, 1850:2100, vars = GLOBAL_TEMP()) %>%
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
