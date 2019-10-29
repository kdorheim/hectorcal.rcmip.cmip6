##

library(dplyr)
library(tidyr)
library(ggplot2)


INPUT_DIR <- here::here('data-raw', 'C.cmip6-outputs')


scenarios <- c("historical", "esm-hist", "ssp126", "ssp245" , "ssp370", "ssp585")

read.csv(file.path(INPUT_DIR, 'cmip6_global_means.csv'), stringsAsFactors = FALSE) %>%
  filter(experiment %in% scenarios) %>%
  mutate(year = substr(x = time, start = 1, stop = 4),
         year = as.integer(year)) %>%
  group_by(era, year, variable, domain, model, experiment, ensemble) %>%
  summarise(value = mean(value)) %>%
  ungroup ->
  cmip6_tas

cmip6_tas %>%
  filter(year <= 1860) %>%
  group_by(variable, domain, model, ensemble) %>%
  summarise(preInd = mean(value)) %>%
  ungroup ->
  preindustiral_temp

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

read.csv(file.path(INPUT_DIR, 'cmip6_global_means_hfds.csv'), stringsAsFactors = FALSE) %>%
  filter(experiment %in% scenarios) %>%
  mutate(year = substr(x = time, start = 1, stop = 4),
         year = as.integer(year)) %>%
  group_by(era, year, variable, domain, model, experiment, ensemble) %>%
  summarise(value = mean(value)) %>%
  ungroup %>%
  mutate(experiment = if_else(experiment == 'historical', 'concentration.historical-cmip6', paste0('concentration.', experiment))) %>%
  select(era, year, value, variable, model, experiment, ensemble) ->
  formatted_HF

cmip6_data <- bind_rows(formatted_tas, formatted_HF)
usethis::use_data(cmip6_data, overwrite = TRUE)




