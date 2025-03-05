# install.packages("siera")
# remotes::install_github("https://github.com/clymbclinical/siera.git")
library(siera)
# install.packages("safetyData")
# install.packages("splitstackshape")
library(splitstackshape)

# write.csv(safetyData::adam_advs, "dev/examples/ADVS.csv")
# write.csv(safetyData::adam_adsl, "dev/examples/ADSL.csv")
# write.csv(safetyData::adam_adae, "dev/examples/ADAE.csv")

# json_path <- ARS_example("ARS_V1_Common_Safety_Displays.json")
# json_path <- "dev/examples/Common Safety Displays.json"
json_path <- "dev/build/data_change_simple.json"

# Path to a folder which will contain the meta-programmed R scripts (feel free to update
# to a more suitable path)
output_folder <-"dev/examples"

# this folder contains ADaM datasets to produce ARD (we will use temporary
# directory tempdir(), but feel free to download the ADaMs required and use the location they are stored in.
# This can be done with e.g. dirname(ARS_example("ADSL.csv"))
# ADaM_folder <- tempdir()
ADaM_folder <- "dev/examples"

# run the readARS function with these 3 parameters.  This creates R scripts (1 for each output in output_folder)
readARS(json_path, output_folder, ADaM_folder)



