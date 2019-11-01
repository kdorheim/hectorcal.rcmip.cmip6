## 1.process_CMIP6_hfds.R
## This script processes the CMIP6 files for the emulation porition of the rcmip project.
## Running time depends on the number of files being processed.

# 0. Set Up --------------------------------------------------------------------------------------
# Load the required R packages.
library(dplyr)
library(tidyr)
library(tibble)
library(ncdf4)

# Define directories.
PROJECT_DIR   <- '/pic/projects/GCAM/Dorheim/rcmip-cmip6calibration' # Where the GlobalC project lives on pic
INPUT_DIR     <- file.path(PROJECT_DIR, 'output')             # Where the csv files containing the information about the files to process live.
OUTPUT_DIR    <- file.path(INPUT_DIR)                         # Where to save the final csv file.
INTERMED_DIR  <- file.path(OUTPUT_DIR, 'intermediate-ncs'); dir.create(INTERMED_DIR) # Where to save all the intermediate ncdf files
CMIP6_DIR     <- file.path('/pic', 'projects', 'GCAM', 'CMIP6')


# Define the path to the CDO
CDO_EXE <- "/share/apps/netcdf/4.3.2/gcc/4.4.7/bin/cdo" # Define the cdo directory.

# Option to delete all of the intermediate nc and csv files.
CELAN_UP <- FALSE

# 1. Import Infomration To Process ------------------------------------------------------------
#fx_files   <- read.csv(list.files(INPUT_DIR, pattern = '0.fx_files_CMIP6_hfds.csv', full.names = TRUE), stringsAsFactors = FALSE)
#data_files <- read.csv(list.files(INPUT_DIR, pattern = '0.to_process_CMIP6_hfds.csv', full.names = TRUE), stringsAsFactors = FALSE)

list.files(CMIP6_DIR, 'cmip6_index.csv', full.names = TRUE) %>%
  read.csv(stringsAsFactors = FALSE) ->
  cmip6_index

cmip6_index %>%
  filter(variable == 'hfds') %>%
  filter(domain == 'Omon') %>%
  filter(grepl(pattern = 'ssp|historical', x = experiment))  %>%
  rename(files = file) %>%
  mutate(era = 'cmip6') ->
  data_files


cmip6_index %>%
  filter(variable == 'areacella') %>%
  filter(model %in% data_files$model & experiment %in% data_files$experiment & ensemble %in% data_files$ensemble) %>%
  spread(variable, file) %>%
  select(-time) ->
  fx_files






# 2. Calculate the Ocean Area Weight ---------------------------------------------------------
# Make a netcdf of ocean weights, where cells that are land based have a value of 0 so that when we take the global average those values do not count.
# This function will save the new netcdf as the out_nc.
# Args
#       cellArea_nc: the netcdf containing the cell area
#       landFrac_nc: the netcdf of the land fraction data
# Returns the path to ocean cell area weights netcdf.
# ocean_weights <- function(cellArea_nc, landFrac_nc){
#
#   out1_nc <- paste0(INTERMED_DIR, 'out1.nc')
#   out2_nc <- paste0(INTERMED_DIR, 'out2.nc')
#   out3_nc <- paste0(INTERMED_DIR, 'out3.nc')
#   # Although CMIP calls the land fraction data files land fraction they are actually
#   # percent land on a scale of 0 to 100 ocean area = cell area * (1 - 0.01 * landPercent)
#   system2(CDO_EXE, args = c("mulc,-0.01", landFrac_nc, out1_nc), stdout = TRUE, stderr = TRUE)
#   system2(CDO_EXE, args = c("addc,1", out1_nc, out2_nc), stdout = TRUE, stderr = TRUE)
#   system2(CDO_EXE, args = c("mul", cellArea_nc, out2_nc, out3_nc), stdout = TRUE, stderr = TRUE)
#
#   # Return the path to the weights
#   out3_nc
#
# }

# # Calculate the ocean area cell weights.
# oceanWeight_nc <- unlist(mapply(FUN = ocean_weights,
#                                 cellArea_nc = fx_files$areacella[1:3],
#                                 landFrac_nc = fx_files$sftlf))


# fx_files %>%
#   select(areacella = files, model, experiment, ensemble, grid) %>% head()
#   full_join(data_files) %>%
#   na.omit() ->
#   data_cellArea


# 3. Calculate Global Average  -------------------------------------------------------
# Ensure that we are only processing the land area weights for the model, experiment, and ensemble
# members we have access to.
# data_files %>%
#   cbind(areacella = oceanWeight_nc) ->
#   fx_data_to_process

# Calucalte the weighted global average of a netcdf.
# Args
#   data_CellArea: a dataframe containing the netcdf to process, CMIP6 meta data, and the cell area netcdf.
#   region_input: default is set to NULL, which will process the global weighted average. Otherwise it can be set to
#   a list of the size of the lat and lon boxes to process the weighted regional average of.
# Returns
#   messages - prints out messages about the netcdf and region that is being processed, this can make the slurm files messy but is helpful for debugging.
#   intermediate netcdfs - Because CMIP6 files cannot be processed with pipelined CDO, this function saves lots of intermediate netcdf files. They eventually need to be cleaned up.
#   intermediate csv files - In order to help with the debugging process and prevent process from being lost of the pic script terminates early this function saves all of the processed regional data as csv files.
cdo_regional_means <- function(data_CellArea, region_input = NULL){

  # For every data netcdf file listed in the data_LandArea input calculate
  # the regional average.
  apply(data_CellArea, 1, function(input){
   tryCatch({# Messages
    # Messages
    print('------------------------------------------------')
    print(input[['files']])

    # Define the base name to use for the intermediate files that are saved during this process.
    base_name <- paste(input[['era']], input[["variable"]], input[["domain"]],  input[["model"]],
                       input[["experiment"]], input[["ensemble"]], input[["time"]], sep  = '_')

    # Make the intermediate netcdfs
    mean          <- file.path(INTERMED_DIR, paste0(base_name, '_Mean.nc'))
    final_data    <- file.path(INTERMED_DIR, paste0(base_name, '_final.nc'))
    data_gridArea <- file.path(INTERMED_DIR, paste0(base_name, '_gridarea.nc'))


    # Define the area grid for the data netcdf to process to use to calculate the
    # weighted mean and convert to absolute time.
    #   system2(CDO_EXE, args = c(paste0("setgridarea,", input[['areacella']]), input[['files']], data_gridArea), stdout = TRUE, stderr = TRUE)
    system2(CDO_EXE, args = c('fldmean', input[['files']], mean), stdout = TRUE, stderr = TRUE)
    system2(CDO_EXE, args = c('-a', '-copy', mean, final_data), stdout = TRUE, stderr = TRUE)

    # Extract and format output.
    nc <- nc_open(final_data)

    data.frame(value = ncvar_get(nc, input[['variable']]),
               units = ncatt_get(nc, input[['variable']])$units,
               time = ncvar_get(nc, 'time')) %>%
      mutate(era = input[['era']],
             variable = input[["variable"]],
             domain = input[["domain"]],
             model = input[["model"]],
             experiment = input[["experiment"]],
             ensemble = input[["ensemble"]]) ->
      global_mean

    # Save a copy of the data as an intermediate csv file
    write.csv(global_mean, file = file.path(INTER_OUTPUT_CSV, paste0(base_name, '.csv')), row.names = TRUE)

    global_mean

  }, error = function(e){NULL})


  }) %>%
    bind_rows()

}


# 3B. Weighted Global Average -------------------------------------------------------
INTER_OUTPUT_CSV <- file.path(OUTPUT_DIR, 'intermediate-csv-global')
dir.create(INTER_OUTPUT_CSV)

cdo_regional_means(data_files) %>%
  write.csv(file = file.path(OUTPUT_DIR, 'cmip6_global_means_hfds.csv'), row.names = FALSE)

# 4. Intermediate Files --------------------------------------------------------------

## If for some reason there is a need to use the intermediatei files un comment this section.
if(CELAN_UP){

  file.remove(list.files(INTERMED_DIR, full.names = TRUE, recursive = TRUE))

} else{

  list.files(INTER_OUTPUT_CSV, pattern = '.csv', full.names = TRUE) %>%
    lapply(FUN = read.csv, stringsAsFactors = FALSE) %>%
    bind_rows %>%
    select(-X) ->
    data2

  data2 %>%
    write.csv(file = file.path(OUTPUT_DIR, 'cmip6_region_meansNEW.csv'), row.names = FALSE)


}
