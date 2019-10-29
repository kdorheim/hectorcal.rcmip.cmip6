#' Set up CMIP6 Hector core
#'
#' Make a new Hector core and set it up with emissions and/or concentrations from the CMIP6 scenarios.
#' This function depends on a specific R version package being installed, it must have the ability to
#' perscibe GHG concentrations.
#'
#' @param cmip_scenario The name of the CMIP6 scenario to set up, it must match package CMIP6 nomenclature.
#' emissions/concenstraiton.CMIP6epxeriment.
#' @return A Hector core that is ready to run
#' @export
newCMIP6core <- function(cmip_scenario){

  # Make sure that the cmip_scenario string is reconized by the package data.
  assertthat::assert_that(any(cmip_scenario %in% names(cmip6_Hector_inputs)), msg = 'cmip_scenario not recognized.')
  assertthat::assert_that(any(grepl(pattern = cmip_scenario, x = cmip6_ini$cmip6)), msg = 'cmip_scenario not recognized.')

  # Create a Hector core using the appropriate existin ini file.
  ini_name <- cmip6_ini$ini_name[cmip6_ini$cmip6 == cmip_scenario]
  ini      <- system.file(file.path('input', ini_name), package = 'hector')
  core     <- newcore(inifile = ini, name = cmip_scenario)

  # Set up the Hector core with the CMIP6 specific emissions/concenatrion time series.
  time_series_inputs <- cmip6_Hector_inputs[[cmip_scenario]]

  split(time_series_inputs, time_series_inputs$variable) %>%
    lapply(function(input){

          setvar(core = core, dates = input$year, values = input$value,
           var = input$variable, unit =  input$units)
  })
  reset(core = core)

  # Return the CMIP6 core
  return(core = core)

}








