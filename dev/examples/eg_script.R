# Function to install a package if it's not already installed
install_if_missing <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name)
  }
}

# One-time setup function
setup_environment <- function() {
  # Install required packages
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

  # Write example data files (only if they don't exist)
  data_files <- c("ADVS.csv", "ADSL.csv", "ADAE.csv")
  for (file in data_files) {
    output_path <- file.path("dev", "examples", file)
    if (!file.exists(output_path)) {
      data_name <- sub("\\.csv$", "", file)
      write.csv(get(data_name, envir = as.environment("package:safetyData")), output_path)
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

# Execute the workflow
main <- function(json_path = NULL, output_folder = NULL, ADaM_folder = NULL, run_setup = TRUE) {
  # Run one-time setup if requested
  if (run_setup) {
    setup_environment()
  }

  # Set default paths if not provided
  if (is.null(json_path)) {
    json_path <- "dev/examples/Common Safety Displays.json"
  }
  if (is.null(output_folder)) {
    output_folder <- "dev/examples"
  }
  if (is.null(ADaM_folder)) {
    ADaM_folder <- "dev/examples"
  }

  # Run the main workflow
  run_siera_workflow(json_path, output_folder, ADaM_folder)
}

# Example usage:
# Run with default parameters
main()

# Run with custom parameters
# main(json_path = "path/to/custom/json", output_folder = "custom/output", ADaM_folder = "custom/adam", run_setup = FALSE)
#

main(json_path = "dev/examples/Common Safety Displays.json", run_setup = FALSE)


main(json_path = "dev/build/data_change_simple.json", run_setup = FALSE)

debugonce(readARS)
readARS("dev/build/data_change_simple_sub_added.json", "dev/examples", "dev/examples")


debugonce(readARS)
readARS("dev/examples/Common Safety Displays.json", "dev/examples", "dev/examples")
