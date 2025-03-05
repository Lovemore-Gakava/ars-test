if (!require("reticulate")) install.packages("reticulate")

# This is setup only once ->
# reticulate::virtualenv_create("ars_env")

library(reticulate)
# Update.Renviron to have this path
# RETICULATE_PYTHON = ".virtualenvs/ars_env/bin/python"
use_virtualenv("ars_env")
