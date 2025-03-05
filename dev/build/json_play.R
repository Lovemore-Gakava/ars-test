install.packages("jsonlite")
library(jsonlite)
data_change_simple <- fromJSON("dev/build/data_change_simple_sub_added.json")
ARS_V1_SAF <- fromJSON("dev/examples/ARS_V1_Common_Safety_Displays.json")
data_change_simple$analysisSets
ARS_V1_SAF$analyses
