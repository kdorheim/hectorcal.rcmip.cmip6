# This script creates an ensemble of Hector runs that will be for the PCA and
# to generate the center and scale values used in the single ESM calibration
# protocols.
# Right now this is set up to run on my local machine but can be adapted
# to run on pic.

# 0. Set Up --------------------------------------------------------------------------------
Sys.time()

OUTPUT_DIR <- here::here('data-raw', 'B.Hector_enesmble_runs')
dir.create(OUTPUT_DIR)

# Load the required packages
library(hector)
library(hectorcal)
library(doParallel)
library(dplyr)
devtools::load_all('~/Documents/2019/As/hector/')


# Define the prior hyper-parameters, these should be the same as the hyper-parameters defined in
# the likelihood function. These will be used as default inputs into functions defined
# in section 1 to create the prior distirbtuion.
hyper_params <- list(ecsmu = 3,.0,
                     ecssig = 3.0,
                     aeromu = 1.0,
                     aerosig = 1.4,
                     volcanomu = 1.0,
                     volcanosig = 1.4,
                     kappamu = 2.3,
                     kappasig = 2.0,
                     betamu = 0.3,
                     betasig = 0.7,
                     q10mu = 2.0,
                     q10sig = 2.0,
                     c0mu = 285,
                     c0sig = 14.0)


# Set the seed to ensure reproducible results
set.seed(867-5309)

# This should be equal to the number of nodes selected in the...
nodes <- 1

# The number of Hector runs per rcp scenario
n_runs <- 2000

# Years to extract the Hector output for
years  <- 1850:2100


# 1. Define functions ---------------------------------------------------------------------
# Randomly generate carbon values based on the prior distribution. These are the
# parameters used in the emission driven runs. Will be used in the hector_sample function.
#
# Args
#   ecsmu: the climate sensitivity mean, default is set to the value in the hyper parameter defined in section 0
#   ecssig: the climate sensitivity  standard deviation
#   aeromu: the aerosol scalar mean
#   aerosig: the aerosol standard deviation
# Returns: a list of functions that will sample the climate sensitivity, aero scaler, and ocean diffusivity
# parameter spaces. Each function in the list will sample the prior distribution n times.
make_concen_dist <- function(ecsmu = hyper_params$ecsmu, ecssig = hyper_params$ecssig,
                             aeromu = hyper_params$aeromu, aerosig = hyper_params$aerosig,
                             volcanomu = hyper_params$volcanomu, volcanosig = hyper_params$volcanosig,
                             kappamu = hyper_params$kappamu, kappasig = hyper_params$kappasig){

  list <- list(function(n) {rlnorm(n = n, meanlog = log(ecsmu), sdlog = log(ecssig))},
               function(n) {rnorm(n = n, mean = aeromu, sd = aerosig)},
               function(n) {rnorm(n = n, mean = volcanomu, sd = volcanosig)},
               function(n) {rnorm(n = n, mean = kappamu, sd = kappasig)})
  # Name the functions returned in the list by hector parameter
  names(list) <- c(ECS(), AERO_SCALE(), VOLCANIC_SCALE(), DIFFUSIVITY())

  list
}

# Randomly generate carbon cycle and climate parameter values based on the prior distribution. These are the
# parameters used in the emission driven runs. Will be used in the hector_sample function.
#
# Args
#   ecsmu: the climate sensitivity mean, default is set to the value in the hyper parameter defined in section 0
#   ecssig: the climate sensitivity  standard deviation
#   aeromu: the aerosol scalar mean
#   aerosig: the aerosol standard deviation
#   c0mu: mean preindustrial co2
#   c0sig: sd preindustrial co2
#   q10mu: mean temperature effect on heterotrophic respiration
#   q10sig: sd effect on heterotrophic respiration
#   betamu: mean co2 fertilization
#   betasig: sd co2 fertilization
# Returns: a list of functions that will sample the climate sensitivity, aero scaler, ocean diffusivity
# beta, q10, and pre industrial CO2. Each function in the list will sample the prior distribution n times.
make_emiss_dist <- function(ecsmu = hyper_params$ecsmu, ecssig = hyper_params$ecssig,
                            aeromu = hyper_params$aeromu, aerosig = hyper_params$ecssig,
                            volcanomu = hyper_params$volcanomu, volcanosig = hyper_params$volcanosig,
                            kappamu = hyper_params$kappamu, kappasig = hyper_params$kappasig,
                            c0mu = hyper_params$c0mu,  c0sig = hyper_params$c0sig,
                            q10mu = hyper_params$q10mu, q10sig = hyper_params$q10sig,
                            betamu = hyper_params$betamu, betasig = hyper_params$betasig){


  concentration_dists <- make_concen_dist(ecsmu = ecsmu, ecssig = ecssig,
                                          aeromu = aeromu, aerosig = aerosig,
                                          volcanomu = volcanomu, volcanosig = volcanosig,
                                          kappamu = kappamu, kappasig = kappasig)

  emission_dists <- append(concentration_dists, list(function(n){ rnorm(n = n, mean = c0mu, sd = c0sig) },
                                                     function(n){ hectorcal::rtruncnorm(n = n, a = 0, b = Inf, mu = q10mu, sig = q10sig) },
                                                     function(n){ hectorcal::rtruncnorm(n = n, a = 0, b = Inf, mu = betamu, sig = betasig)}))
  # Name the functions returned in the list by hector parameter
  names(emission_dists) <- c(ECS(), AERO_SCALE(), VOLCANIC_SCALE(), DIFFUSIVITY(), PREINDUSTRIAL_CO2(), Q10_RH(), BETA())

  emission_dists
}

# Create a list n long with a hector core for RCMIP scenario
#
# Args
#   cmip_run: a vector of the names of the CMIP6 cores to generate.
# Returns: the a list of hector cores to parallelize over
setup_cores <- function(cmip_run) {
  unlist(lapply(cmip_run, newCMIP6core))
}


# Run Hector with the input paramters
# Args
#   param_vals: a data frame of the Hector paramters
#   cmip_run: the name of the CMIP6 Hector run
#   keeptimes: a vector of the years of data to store results for
#   vars: a vector of the Hector output names to save, the default is set to save temp and heat flux
sample_Hector <- function(param_vals, cmip_run, keeptimes, vars = c(GLOBAL_TEMP(), HEAT_FLUX())){

  assertthat::assert_that(length(cmip_run) == 1)
  core      <- setup_cores(cmip_run)
  core      <- core[[1]]


  apply(param_vals, MARGIN = 1,
        function(input){

          tryCatch({
            param        <- as.vector(input[1:4])
          names(param) <- names(input)[1:4]
          parameterize_core(core = core, params = param)
          reset(core)

          run(core = core, runtodate = max(keeptimes))
          fetchvars(core, keeptimes, vars)  %>%
            mutate(run = input[[5]])},
          error = function(e){data.frame(year = NA, value = NA, scenario = NA, run = input[[5]], stringsAsFactors = FALSE)})



        }) %>%
    bind_rows()
}




# 2. Runs ------------------------------------------------------------------------------------------------
# Make the output directory
output_dir <- OUTPUT_DIR
dir.create(output_dir)
## Variables to keep for different kinds of runs
vars <- c(GLOBAL_TEMP(), HEAT_FLUX(), ATMOSPHERIC_CO2())


# Make the concentration and the emission prior distribution functions.
concentration_dists <- make_concen_dist()


# Find the ini files and set up the ini vectors to used in the runs
all_runs    <- names(cmip6_Hector_inputs)
concen_runs <- all_runs[grepl(pattern = 'concentration.', all_runs)]
concen_runs <- concen_runs[grepl(pattern = 'ssp|historical', concen_runs )]


# Concentration Driven Runs -----------------------------------------------------------'

## Start by generating a data frame of the parameter vlaues.
param_vals        <- bind_cols(lapply(concentration_dists, function(f){f(2000)}))
param_vals$run_id <- 1:nrow(param_vals)

# Historical
sample_Hector(param_vals = param_vals, cmip_run = 'concentration.historical-cmip6', keeptimes = 1850:2014) %>%
saveRDS(file = file.path(output_dir, 'conc_hist.rds'))

# SSP119
 sample_Hector(param_vals = param_vals, cmip_run = 'concentration.ssp119', keeptimes = 1850:2100) %>%
   saveRDS(file = file.path(output_dir, 'conc_ssp119.rds'))

#  SSP126
 sample_Hector(param_vals = param_vals, cmip_run = 'concentration.ssp126', keeptimes = 1850:2100) %>%
   saveRDS(file = file.path(output_dir, 'conc_ssp126.rds'))

 # SSP245
 sample_Hector(param_vals = param_vals, cmip_run = 'concentration.ssp245', keeptimes = 1850:2100) %>%
   saveRDS(file = file.path(output_dir, 'conc_ssp245.rds'))

 # SSP370
 sample_Hector(param_vals = param_vals, cmip_run = 'concentration.ssp370', keeptimes = 1850:2100) %>%
   saveRDS(file = file.path(output_dir, 'conc_ssp370.rds'))

# SSP370 Low NTCF
 sample_Hector(param_vals = param_vals, cmip_run = 'concentration.ssp370-lowNTCF', keeptimes = 1850:2100) %>%
   saveRDS(file = file.path(output_dir, 'conc_ssp370-lowNTCF.rds'))

 # SSP434
 sample_Hector(param_vals = param_vals, cmip_run = 'concentration.ssp434', keeptimes = 1850:2100) %>%
   saveRDS(file = file.path(output_dir, 'conc_ssp434.rds'))

 # SSP460
 sample_Hector(param_vals = param_vals, cmip_run = 'concentration.ssp460', keeptimes = 1850:2100) %>%
   saveRDS(file = file.path(output_dir, 'conc_ssp460.rds'))

 # SSP534-over
 sample_Hector(param_vals = param_vals, cmip_run = 'concentration.ssp534-over', keeptimes = 1850:2100) %>%
   saveRDS(file = file.path(output_dir, 'conc_ssp535-over.rds'))

 # SSP585
 sample_Hector(param_vals = param_vals, cmip_run = 'concentration.ssp585', keeptimes = 1850:2100) %>%
   saveRDS(file = file.path(output_dir, 'conc_ssp585.rds'))


