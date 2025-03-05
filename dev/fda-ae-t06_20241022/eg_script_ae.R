# install.packages("siera")
# remotes::install_github("https://github.com/clymbclinical/siera.git")
library(siera)
# install.packages("safetyData")
# install.packages("splitstackshape")
library(splitstackshape)

json_path <- "dev/fda-ae-t06_20241022/fda-ae-t06-ars-updated.json"

# Path to a folder which will contain the meta-programmed R scripts (feel free to update
# to a more suitable path)
output_folder <-"dev/fda-ae-t06_20241022"

# this folder contains ADaM datasets to produce ARD (we will use temporary
# directory tempdir(), but feel free to download the ADaMs required and use the location they are stored in.
# This can be done with e.g. dirname(ARS_example("ADSL.csv"))
# ADaM_folder <- tempdir()
ADaM_folder <- "dev/examples"

# run the readARS function with these 3 parameters.  This creates R scripts (1 for each output in output_folder)
readARS(json_path, output_folder, ADaM_folder)



