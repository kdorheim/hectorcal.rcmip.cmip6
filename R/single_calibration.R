#' Calibrate Hector to a single CMIP6 ESM.
#'
#' @param cmip6_runs A vector of the cmip6 scenario names to use to create Hector cores
#' @param esm_data A data frame of ESM data for a single model that contains the following columns, year, model, variable, experiment.
#' @param normalize A list of center and the scale values to use to noramlize the Hector and ESM output data.
#' @param initial_param A named vector of inital paramters to be optimized over.
#' @param cmip_range Default set to NULL and the make_minim_function will only look at the error between esm_data and Hector data.
#' But if cmip_range is set to a dataframe containing columns (variable, year, sig, lower, and upper) then the minimize function will also
#' minimize the value returned by the -log of the mesa function.
#' @param param_penalty Default is set to NULL but can be set to a function that will return a tibble of experiment / variable / value to penalize the
#' mean squared error for "unreal" paramter values.
#' @param maxit The max number of itterations for optim, default set to 500.
#' @param n_parallel The max number of cores to parallize the runs over,  unless sepcified will use the number of cores detected by \code{detectCores}.
#' @param showMessages Default set to FALSE, will supress Hector error messages.
#' @param intermediateOutput Default set to FALSE, but if set to TRUE will return the MSE for each variable / experiment / ensemble memeber instead over the over all MSE.
#' @return An object returned by \code{optim}
#' @export
single_calibration_cmip6 <- function(cmip6_runs, esm_data, normalize, initial_param, cmip_range = NULL,
                                  param_penalty = NULL, maxit = 500, n_parallel = NULL, showMessages = FALSE,
                                  intermediateOutput = FALSE){

  # Set up the Hector cores with the cmip6 scenarios.
  cores <- lapply(cmip6_runs, newCMIP6core)

  # Make the to minimize function.
  fn <- hectorcal::make_minimize_function(hector_cores = cores,
                                          esm_data = esm_data,
                                          normalize = normalize,
                                          param = initial_param,
                                          cmip_range = cmip_range,
                                          n = n_parallel,
                                          param_penalty = param_penalty,
                                          showMessages = showMessages,
                                          intermediateOutput = intermediateOutput)

  # Optimize the hector parameters.
  stats::optim(par = initial_param, fn = fn, control = list('maxit' = maxit))

}
