context('Test the Hector support functions.')

library(dplyr)
devtools::load_all('/Users/dorh012/Documents/2019/AS/hector')


testthat::test_that('correct version of Hector is being used', {

  years <- 1850:2100

  ini <- system.file('input/hector_rcp45_constrained.ini', package = 'hector')
  core <- newcore(inifile = ini, name = 'cmip5')
  run(core)
  default_rslt <- fetchvars(core = core, dates = years, vars = c(GLOBAL_TEMP(), ATMOSPHERIC_CH4()))

  default_rslt %>%
    filter(variable == ATMOSPHERIC_CH4()) %>%
    mutate(value = value * 1/3) ->
    reduced_CH4_conc

  setvar(core = core, dates = reduced_CH4_conc$year, values = reduced_CH4_conc$value,
         var = reduced_CH4_conc$variable, unit = reduced_CH4_conc$units)
  reset(core)
  run(core)

  fetchvars(core = core, dates = years, vars = c(GLOBAL_TEMP(), ATMOSPHERIC_CH4())) %>%
    rename(new_value = value) ->
    reduced_CH4_out

  comparison_df <- left_join(reduced_CH4_out, default_rslt, by = c("scenario", "year", "variable", "units"))
  testthat::expect_true(all(c(comparison_df$value - comparison_df$new_value) != 0))
})


testthat::test_that('newCMIP6core works',{

  # First check to make sure that the newCMIPcore will throw errors.
  testthat::expect_error(newCMIP6core('bogus_sceanrio'), 'cmip_scenario not recognized.')


  rcp45_core <- newcore(system.file('input/hector_rcp45_constrained.ini', package = 'hector'))
  run(rcp45_core)
  rcp45_results <-  fetchvars(rcp45_core, 1850:2100,
                              c(hector::GLOBAL_TEMP(), hector::ATMOSPHERIC_CH4())) %>%
    dplyr::rename(cmip5_output = value)
  shutdown(rcp45_core)

  # Make a new core that uses the CMIP6 inputs
  cmip6_run  <- 'concentration.ssp434'
  cmip6_core <- newCMIP6core(cmip6_run)
  reset(cmip6_core)
  run(cmip6_core)
  cmip6_results <-fetchvars(cmip6_core, 1850:2100,
                           c(hector::GLOBAL_TEMP(), hector::ATMOSPHERIC_CH4())) %>%
    dplyr::rename(cmip6_output = value)

  # First check to make sure that the atmospheric concentrations are what we expected by comparing with the CMIP6_inputs data.
  cmip6_results %>%
    left_join(rcp45_results, by = c('year', 'variable')) %>%
    filter(variable != 'Tgav') %>%
    mutate(dif = cmip6_output - cmip5_output) %>%
    pull(dif) ->
    difference

  testthat::expect_true(all(difference!= 0))

  })
