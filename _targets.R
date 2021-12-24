library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.

source("R/functions.R")
source("_packages.R")
options(timeout=1200) # let things download for at least 20 minutes
options(download.file.method = "libcurl")



# End this file with a list of target objects.
list(
  tar_target(medford_from_state, GetDataFromState()),
  tar_target(medford_from_city, GetDataFromCity()),
  tar_target(hhs_capacity_ma, CreateHHSData()),
  tar_target(hhs_capacity_ma_focal, CreateHHSDataFocalCities(hhs_capacity_ma)),
  tar_target(hhs_capacity_ma_focal_latest_pretty, CreateHHSDataFocalCitiesPretty(hhs_capacity_ma_focal)),
  tar_target(tsa_throughput, GetTSAThroughput())
 # tar_target(bosflights, GetBOSFlights())
)
