# One-time setup function
setup_environment <- function(ADaM_folder) {
  # Install required packages
  install_if_missing <- function(package_name) {
    if (!requireNamespace(package_name, quietly = TRUE)) {
      install.packages(package_name)
    }
  }

  install_if_missing("remotes")
  install_if_missing("siera")
  install_if_missing("safetyData")
  install_if_missing("splitstackshape")

  # Install siera from GitHub if not already installed
  if (!requireNamespace("siera", quietly = TRUE)) {
    remotes::install_github("https://github.com/clymbclinical/siera.git")
  }

  # Load required libraries
  library(siera)
  library(splitstackshape)
  library(safetyData)

  # Create ADaM folder if it doesn't exist
  dir.create(ADaM_folder, showWarnings = FALSE, recursive = TRUE)

  # Write example data files as RData (only if they don't exist)
  data_files <- c("ADVS", "ADSL", "ADAE")
  for (file in data_files) {
    output_path <- file.path(ADaM_folder, paste0(file, ".RData"))
    if (!file.exists(output_path)) {
      data <- get(file, envir = as.environment("package:safetyData"))
      save(list = file, file = output_path, envir = environment())
      cat(sprintf("Saved %s to %s\n", file, output_path))
    } else {
      cat(sprintf("%s already exists, skipping...\n", output_path))
    }
  }
}

# Main workflow function
run_siera_workflow <- function(json_path, output_folder, ADaM_folder) {
  # Ensure output and ADaM folders exist
  dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
  dir.create(ADaM_folder, showWarnings = FALSE, recursive = TRUE)

  # Run the readARS function
  readARS(json_path, output_folder, ADaM_folder)
}

# Main function
main <- function(json_path = NULL, output_folder = NULL, ADaM_folder = NULL, run_setup = TRUE) {
  # Check if required paths are provided
  if (is.null(json_path) || is.null(output_folder) || is.null(ADaM_folder)) {
    cat("Error: Required paths are not provided. Please specify json_path, output_folder, and ADaM_folder.\n")
    return()  # This will abort the function
  }

  # Run one-time setup if requested
  if (run_setup) {
    setup_environment(ADaM_folder)
  }

  # Run the main workflow
  run_siera_workflow(json_path, output_folder, ADaM_folder)
}

# main(json_path = "path/to/json",
#      output_folder = "path/to/output",
#      ADaM_folder = "path/to/ADaM",
#      run_setup = TRUE)

