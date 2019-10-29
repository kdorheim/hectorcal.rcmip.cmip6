#' A list of dataframes containing CMIP6 emissions and concentrations for each scenario.
#'
#' A list of datafame that contain the the CMIP6 emissions and concentration inputs for each
#' of the different CMIP6 scenarios labled by driver, either concentration or emissions.CMIP
#' scenario name.
#'
#'
#' @format A list of data frames
#' \describe{
#' \item{driver.CMIP6scenario}{Is a dataframe containing a year, variable, value, and units column. All of the
#' contents of the data frame should be formatted as correct Hector inputs. }
#' }
#' @family CMIP6
'cmip6_Hector_inputs'


#' A mapping tibble of the CMIP6 scenario and relevant Hector CMIP5 ini file.
#'
#' A mapping tibble of the CMIP6 scenario name and the CMIP5 ini file. Because
#' Hector has not been updated with CMIP6 ini files this mapping tibble
#' can be used to generate Hector cores until the CMIP6 ini files have been
#' created.
#'
#'
#' @format A data frame of the CMIP6 scenario name and the relvant hecotr CMIP5 ini file.
#' \describe{
#' \item{cmip6}{A column of the cmip6 scenario frame with the driver.CMIP6 nomenclature.}
#' \item{ini_name}{The name of a Hector CMIP5 ini file name, these ini files should live in the hector package
#' input direcotry}
#' }
#' @family CMIP6
'cmip6_ini'

#' A list of the CMIP6 scenario, variable, and year center and scale weights.
#'
#' A list of the values from the large ensemble of Hector values to use to
#' center and scale the Hector and CMIP6 output by. This is an important
#' part of the single ESM calibration process.
#'
#'
#' @format A list of the center and scale values to normalize Hector and ESM output data.
#' \describe{
#' \item{center}{A vector the Hector ensemble means for each scenario, variable, and year to use when noramlizing data.}
#' \item{scale}{A vector the Hector ensemble standard deviation for each scenario, variable, and year to use when noramlizing data.}
#' }
#' @family CMIP6
'cmip_center_scale'


#' A data frame of the global mean CMIP6 values
#'
#' The data frame containing the global mean value from each ESM to use as comparison data during the calibration process.
#'
#' @format Data frame with 7 columns
#' \describe{
#' \item{era}{The MIP era}
#' \item{year}{Year being described (1851--2300)}
#' \item{value}{Value of the variable being reported}
#' \item{variable}{Variable being reported ("tas" or "co2").  Temperatures are
#' anomalies relative to the first decade reported by the model.}
#' \item{model}{Name of the model}
#' \item{ensemble}{Identifier for the ensemble member}
#' \item{experiment}{Experiment being reported. One of "historical", "rcp26", "rcp45",
#' "rcp60", "rcp85", "esmHistorical", "esmrcp85"}
#' }
#' @family CMIP6
'cmip6_data'
