################################################################################
# R Package Requirements for EDC Risk Perception Analysis
#
# This script installs all required R packages for the analysis scripts.
# Run this file once before running the analysis.
#
# Tested with R version 4.3.1
################################################################################

# Package versions used in this analysis (for reproducibility)
use_exact_versions <- TRUE # Set use_exact_versions = FALSE to install latest versions

# Required packages with exact versions used in analysis
package_versions <- c(
    "car" = "3.1.3",
    "caret" = "6.0.94",
    "dplyr" = "1.1.4",
    "effectsize" = "0.8.9",
    "effsize" = "0.8.1",
    "emmeans" = "1.10.5",
    "ggplot2" = "3.5.2",
    "ggrepel" = "0.9.6",
    "kableExtra" = "1.4.0",
    "knitr" = "1.48",
    "plyr" = "1.8.9",
    "purrr" = "1.0.4",
    "reshape2" = "1.4.4",
    "rstatix" = "0.7.2",
    "stringr" = "1.5.1",
    "tidyr" = "1.3.1",
    "tidyverse" = "2.0.0"
)

# Function to install packages with specific versions
install_versioned_packages <- function(pkg_versions) {
    # Install remotes if not available (needed for version-specific installation)
    if (!requireNamespace("remotes", quietly = TRUE)) {
        cat("Installing 'remotes' package...\n")
        install.packages("remotes")
    }
  
    for (pkg in names(pkg_versions)) {
        version <- pkg_versions[pkg]
        
        # Check if package is already installed with correct version
        if (pkg %in% installed.packages()[,"Package"]) {
            current_version <- as.character(packageVersion(pkg))
            if (current_version == version) {
                cat(sprintf("%s %s already installed\n", pkg, version))
                next
            } else {
                cat(sprintf("%s: updating from %s to %s\n", pkg, current_version, version))
            }
        } else {
            cat(sprintf("Installing %s %s\n", pkg, version))
        }
        
        # Install specific version from CRAN archive
        tryCatch({
            remotes::install_version(pkg, version = version, upgrade = "never", quiet = TRUE)
            cat(sprintf("%s %s installed successfully\n", pkg, version))
        }, error = function(e) {
            cat(sprintf("Error installing %s %s: %s\n", pkg, version, e$message))
        })
    }
}

# Function to install latest versions
install_latest_packages <- function(pkg_versions) {
    packages <- names(pkg_versions)
    new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

    if(length(new_packages) > 0) {
        cat("Installing missing packages:", paste(new_packages, collapse = ", "), "\n")
        install.packages(new_packages, dependencies = TRUE)
    } else {
        cat("All required packages are already installed.\n")
    }
}

# Install packages
cat("Checking for required packages...\n")
if (use_exact_versions) {
    cat("\nMode: Installing exact versions for reproducibility\n")
    cat("(This may take a few minutes...)\n\n")
    install_versioned_packages(package_versions)
} else {
    cat("\nMode: Installing latest available versions\n\n")
    install_latest_packages(package_versions)
}

# Verify installation and print versions
cat("\n=== Installed Package Versions ===\n")
all_correct <- TRUE
for (pkg in names(package_versions)) {
    if (pkg %in% installed.packages()[,"Package"]) {
        version <- as.character(packageVersion(pkg))
        expected <- package_versions[pkg]

        if (use_exact_versions && version != expected) {
            cat(sprintf("%-15s %s (expected %s)\n", pkg, version, expected))
            all_correct <- FALSE
        } else {
            cat(sprintf("%-15s %s\n", pkg, version))
        }
    } else {
        cat(sprintf("%-15s NOT INSTALLED\n", pkg))
        all_correct <- FALSE
    }
}

if (use_exact_versions && all_correct) {
    cat("\nAll packages installed with correct versions!\n")
} else if (!use_exact_versions) {
    cat("\nAll packages installed (using latest versions)\n")
} else {
    cat("\nSome packages have version mismatches. You may need to reinstall.\n")
}

cat("\nSetup complete! You can now run the analysis scripts.\n")
