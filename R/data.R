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
