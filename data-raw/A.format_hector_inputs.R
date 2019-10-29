## 1.format_hector_inputs.R
## The point of this script is to convert the rcmip cmip6 scenarios into inputs that can easily be used by
## R Hector for the emulation and calibration runs.
##
## Not all of the CMIP6 emissions / concentrations are compatible with Hector and in some cases Hector
## has extra emissions compared to the the other models. For now we are going to use the
## inputs associated with the CMIP5 rcps although this may change in the future.
##
## Because the SSPs use historical inputs until the 2015 time the spp input time series
## must include the cmip6 historical inputs as well.
##
## This script should be sourced from the project data-raw directory.

# 0. Set Up --------------------------------------------------------------------------------------
# Load the libraries
library(dplyr)
library(tidyr)

# Define the paths
INPUT_DIR  <- here::here('data-raw', 'A.cmip6-inputs')

# 1. Import and Format Data ---------------------------------------------------------------------------------
# The data is read in as wide data and there seems to be something funky going on with the rcmip_variable
# entries. Before matching them we will need to make sure the names are consistent. WHen the data is
# read then there seems to be issues with spaces and the capital letters.

# Import and format the rcmip concentration data
read.csv(list.files(INPUT_DIR, 'rcmip-concentrations-annual-means-v1-0-0.csv', full.names = TRUE), stringsAsFactors = FALSE) %>%
  gather(key = year, value = value, -Model, -Scenario, -Region, -Variable, -Unit, -Activity_Id, -Mip_Era) %>%
  mutate(year = gsub(pattern = 'X', replacement = '', year),
         year = as.integer(year)) %>%
  filter(Region == 'World') %>%
  mutate(Variable = tolower(gsub(pattern = ' ', replacement = '', x = Variable)),
         Unit = tolower(gsub(pattern = ' ', replacement = '', x = Unit))) %>%
  na.omit %>%
  rename(rcmip_variable = Variable,
         rcmip_units = Unit) ->
  rcmip_conc

# Import and format the rcmip emission data
read.csv(list.files(INPUT_DIR, 'rcmip-emissions-annual-means-v3-0-0.csv', full.names = TRUE), stringsAsFactors = FALSE) %>%
  gather(key = year, value = value, -Model, -Scenario, -Region, -Variable, -Unit, -Activity_Id, -Mip_Era) %>%
  mutate(year = gsub(pattern = 'X', replacement = '', year),
         year = as.integer(year)) %>%
  filter(Region == 'World') %>%
  mutate(Variable = tolower(gsub(pattern = ' ', replacement = '', x = Variable)),
         Unit = tolower(gsub(pattern = ' ', replacement = '', x = Unit))) %>%
  na.omit %>%
  rename(rcmip_variable = Variable,
         rcmip_units = Unit) ->
  rcmip_emiss

# Import the conversion data.
read.csv(list.files(INPUT_DIR, 'variable-conversion.csv', full.names = TRUE), stringsAsFactors = FALSE, comment.char = '#') %>%
  mutate(rcmip_variable = tolower(gsub(pattern = ' ', replacement = '', x = rcmip_variable)),
         rcmip_units = tolower(gsub(pattern = ' ', replacement = '', x = rcmip_units))) %>%
  mutate(hector_variable = gsub(pattern = ' ', replacement = '', x = hector_variable),
         hector_unit = gsub(pattern = ' ', replacement = '', x = hector_unit)) ->
  conversion_table


# 2. Convert CMIP6 Data ---------------------------------------------------------------------------------
# The CMIP6 data needs to be converted to the units and the values recognized by Hector.
rcmip_conc %>%
  filter(Mip_Era == 'CMIP6') %>%
  left_join(conversion_table, by = c('rcmip_variable')) %>%
  na.omit  %>%
  mutate(value =  udunits2::ud.convert(value, rcmip_udunits, hector_udunits)) %>%
  select(model = Model, scenario = Scenario, year, value, hector_component, variable = hector_variable, units = hector_unit) %>%
  group_by(model, scenario, year, hector_component, variable, units) %>%
  summarise(value = sum(value)) %>%
  ungroup ->
  CMIP6_concentrations

rcmip_emiss %>%
  filter(Mip_Era == 'CMIP6') %>%
  left_join(conversion_table, by = c('rcmip_variable')) %>%
  na.omit %>%
  mutate(value =  udunits2::ud.convert(value, rcmip_udunits, hector_udunits)) %>%
  select(model = Model, scenario = Scenario, year, value, hector_component, variable = hector_variable, units = hector_unit) %>%
  group_by(model, scenario, year, hector_component, variable, units) %>%
  summarise(value = sum(value)) %>%
  ungroup ->
  CMIP6_emissions

# Subset the CMIP6 emissions for the aerosol and ozone precursors are missing from the concentration
# driven runs so we will use emissions there.
missing_from_conc <- setdiff(CMIP6_emissions$hector_component, CMIP6_concentrations$hector_component)

CMIP6_emissions %>%
# Problem the emissions driven scenarios do not include the CMIP6 ssps just the
# some idealized runs, therefore we cannot do the concentration driven runs at the
# the moment.
  filter(hector_component %in% missing_from_conc) %>%
  bind_rows(CMIP6_concentrations) ->
  CMIP6_concentrations

# 3. Save Data ---------------------------------------------------------------------------------
CMIP6_concentrations$driven <- 'concentration'
CMIP6_emissions$driven      <- 'emissions'

# The list of concentration values that cannot be perscripbed and can be
# removed accoring to Slack conversation with SJS.
nogo <- c('C3F8_concentration', 'C3F8_concentration', 'C4F10_concentration',
          'C5F12_concentration', 'C6F14_concentration', 'C7F16_concentration',
          'C8F18_concentration', 'cC4F8_concentration', 'CH2Cl2_concentration',
          'CHCl3_concentration', 'CHCl3_concentration', 'HCFC141b_concentration',
          'HCFC142b_concentration', 'HCFC22_concentration', 'HFC152a_concentration',
          'HFC236fa_concentration', 'HFC245_concentration', 'HFC365_concentration',
          'NF3_concentration', 'SO2F2_concentration')


CMIP6_emissions %>%
  bind_rows(CMIP6_concentrations) %>%
  filter(!variable %in% nogo) ->
  CMIP6_inputs

cmip6_Hector_inputs <- split(CMIP6_inputs, interaction(CMIP6_inputs$driven, CMIP6_inputs$scenario, drop = TRUE))
usethis::use_data(cmip6_Hector_inputs, overwrite = TRUE)


# 4. Pair SSP with an ini files configurations ---------------------------------------------------------------------

tibble(cmip6 = names(cmip6_Hector_inputs)) %>%
  mutate(ini_name =  'hector_rcp85.ini') ->
  cmip6_ini

usethis::use_data(cmip6_ini, overwrite = TRUE)

