# Validation Script: Check Metric-Resolution Validity
#
# For each variable name in metadata, verify that metrics exist and contain
# valid (non-NA) values for the specified resolution(s).

library(dplyr)
library(tidyr)
library(stringr)

# Load data ----
load('data/metadata.rda')
load('data/neast_county_metrics.rda')
load('data/neast_state_metrics.rda')

cat("=== Metric-Resolution Validation ===\n\n")

# Initialize results ----
validation_results <- list()
issues_found <- 0

# Get unique metrics from metadata ----
metrics_to_check <- metadata %>%
  select(`Variable Name`, Metric, Resolution) %>%
  distinct()

cat("Checking", nrow(metrics_to_check), "unique metric-resolution combinations...\n\n")

# Validation function ----
validate_metric <- function(var_name, metric_name, resolution_str) {

  # Handle NA resolution
  if (is.na(resolution_str) || resolution_str == "") {
    return(list(
      list(
        metric = metric_name,
        variable = var_name,
        resolution = "UNKNOWN",
        issue = "Resolution not specified in metadata",
        severity = "ERROR"
      )
    ))
  }

  # Parse resolution (can be "County", "State", or "County, State")
  # Case-insensitive matching
  has_county <- str_detect(resolution_str, regex("county", ignore_case = TRUE))
  has_state <- str_detect(resolution_str, regex("state", ignore_case = TRUE))

  issues <- list()

  # Check County resolution ----
  if (has_county) {
    county_data <- neast_county_metrics %>%
      dplyr::filter(variable_name == var_name)

    if (nrow(county_data) == 0) {
      issues <- c(issues, list(
        list(
          metric = metric_name,
          variable = var_name,
          resolution = "County",
          issue = "No data found in neast_county_metrics",
          severity = "ERROR"
        )
      ))
    } else {
      # Check for valid (non-NA) values
      valid_values <- county_data %>%
        dplyr::filter(!is.na(value)) %>%
        nrow()

      if (valid_values == 0) {
        issues <- c(issues, list(
          list(
            metric = metric_name,
            variable = var_name,
            resolution = "County",
            issue = "All values are NA",
            severity = "ERROR"
          )
        ))
      } else if (valid_values < nrow(county_data) * 0.5) {
        # Warning if more than 50% are NA
        pct_na <- round((nrow(county_data) - valid_values) / nrow(county_data) * 100, 1)
        issues <- c(issues, list(
          list(
            metric = metric_name,
            variable = var_name,
            resolution = "County",
            issue = paste0(pct_na, "% of values are NA"),
            severity = "WARNING"
          )
        ))
      }
    }
  }

  # Check State resolution ----
  if (has_state) {
    state_data <- neast_state_metrics %>%
      dplyr::filter(variable_name == var_name)

    if (nrow(state_data) == 0) {
      issues <- c(issues, list(
        list(
          metric = metric_name,
          variable = var_name,
          resolution = "State",
          issue = "No data found in neast_state_metrics",
          severity = "ERROR"
        )
      ))
    } else {
      # Check for valid (non-NA) values
      valid_values <- state_data %>%
        dplyr::filter(!is.na(value)) %>%
        nrow()

      if (valid_values == 0) {
        issues <- c(issues, list(
          list(
            metric = metric_name,
            variable = var_name,
            resolution = "State",
            issue = "All values are NA",
            severity = "ERROR"
          )
        ))
      } else if (valid_values < nrow(state_data) * 0.5) {
        # Warning if more than 50% are NA
        pct_na <- round((nrow(state_data) - valid_values) / nrow(state_data) * 100, 1)
        issues <- c(issues, list(
          list(
            metric = metric_name,
            variable = var_name,
            resolution = "State",
            issue = paste0(pct_na, "% of values are NA"),
            severity = "WARNING"
          )
        ))
      }
    }
  }

  return(issues)
}

# Run validation ----
all_issues <- list()

for (i in 1:nrow(metrics_to_check)) {
  row <- metrics_to_check[i, ]
  issues <- validate_metric(
    row$`Variable Name`,
    row$Metric,
    row$Resolution
  )

  if (length(issues) > 0) {
    all_issues <- c(all_issues, issues)
  }
}

# Report results ----
if (length(all_issues) == 0) {
  cat("✓ All metrics passed validation!\n")
  cat("  No issues found.\n\n")
} else {
  # Separate errors and warnings
  errors <- Filter(function(x) x$severity == "ERROR", all_issues)
  warnings <- Filter(function(x) x$severity == "WARNING", all_issues)

  if (length(errors) > 0) {
    cat("✗ ERRORS FOUND (", length(errors), "):\n", sep = "")
    cat("=====================================\n")
    for (err in errors) {
      cat("  • Metric:", err$metric, "\n")
      cat("    Variable:", err$variable, "\n")
      cat("    Resolution:", err$resolution, "\n")
      cat("    Issue:", err$issue, "\n")
      cat("\n")
    }
  }

  if (length(warnings) > 0) {
    cat("⚠ WARNINGS (", length(warnings), "):\n", sep = "")
    cat("=====================================\n")
    for (warn in warnings) {
      cat("  • Metric:", warn$metric, "\n")
      cat("    Variable:", warn$variable, "\n")
      cat("    Resolution:", warn$resolution, "\n")
      cat("    Issue:", warn$issue, "\n")
      cat("\n")
    }
  }
}

# Summary ----
cat("=== Validation Summary ===\n")
cat("Total metrics checked:", nrow(metrics_to_check), "\n")
cat("Errors found:", length(Filter(function(x) x$severity == "ERROR", all_issues)), "\n")
cat("Warnings found:", length(Filter(function(x) x$severity == "WARNING", all_issues)), "\n\n")

# Extract metrics with no data for claimed resolution ----
no_data_errors <- Filter(function(x) {
  x$severity == "ERROR" &&
  grepl("No data found", x$issue) &&
  x$resolution != "UNKNOWN"
}, all_issues)

# Initialize variables
county_metrics <- character(0)
state_metrics <- character(0)

if (length(no_data_errors) > 0) {
  cat("=== METRICS TO FIX IN METADATA ===\n")
  cat("These metrics claim to have data at a resolution where none exists.\n")
  cat("Remove the resolution from metadata or add the data.\n\n")

  # Group by resolution
  county_metrics <- unique(sapply(Filter(function(x) x$resolution == "County", no_data_errors),
                                   function(x) x$metric))
  state_metrics <- unique(sapply(Filter(function(x) x$resolution == "State", no_data_errors),
                                  function(x) x$metric))

  if (length(county_metrics) > 0) {
    cat("## County Resolution - No Data (", length(county_metrics), " metrics):\n", sep = "")
    cat("Remove 'County' from Resolution column for these metrics:\n\n")

    # Output as R vector for easy copy-paste
    cat("metrics_to_fix_county <- c(\n")
    for (i in seq_along(county_metrics)) {
      metric <- county_metrics[i]
      if (is.na(metric)) {
        cat("  NA")
      } else {
        cat("  \"", metric, "\"", sep = "")
      }
      if (i < length(county_metrics)) {
        cat(",\n")
      } else {
        cat("\n")
      }
    }
    cat(")\n\n")
  }

  if (length(state_metrics) > 0) {
    cat("## State Resolution - No Data (", length(state_metrics), " metrics):\n", sep = "")
    cat("Remove 'State' from Resolution column for these metrics:\n\n")

    # Output as R vector for easy copy-paste
    cat("metrics_to_fix_state <- c(\n")
    for (i in seq_along(state_metrics)) {
      metric <- state_metrics[i]
      if (is.na(metric)) {
        cat("  NA")
      } else {
        cat("  \"", metric, "\"", sep = "")
      }
      if (i < length(state_metrics)) {
        cat(",\n")
      } else {
        cat("\n")
      }
    }
    cat(")\n\n")
  }

  # Also output corresponding variable names
  cat("## Variable Names (for reference):\n\n")

  if (length(county_metrics) > 0) {
    cat("County variables:\n")
    county_vars <- unique(sapply(Filter(function(x) x$resolution == "County", no_data_errors),
                                  function(x) x$variable))
    cat("county_vars <- c(\"", paste(county_vars, collapse = "\", \""), "\")\n\n", sep = "")
  }

  if (length(state_metrics) > 0) {
    cat("State variables:\n")
    state_vars <- unique(sapply(Filter(function(x) x$resolution == "State", no_data_errors),
                                 function(x) x$variable))
    cat("state_vars <- c(\"", paste(state_vars, collapse = "\", \""), "\")\n\n", sep = "")
  }
}

# Return results for programmatic use
invisible(list(
  total_checked = nrow(metrics_to_check),
  errors = Filter(function(x) x$severity == "ERROR", all_issues),
  warnings = Filter(function(x) x$severity == "WARNING", all_issues),
  all_issues = all_issues,
  metrics_to_fix_county = county_metrics,
  metrics_to_fix_state = state_metrics
))


# Save --------------------------------------------------------------------

# results <- source('dev/validate_metric_resolution.R')
# 
# county_to_fix <- results$value$metrics_to_fix_county
# county_to_fix
# state_to_fix <- results$value$metrics_to_fix_state
# state_to_fix
# 
# bad_metrics <- list(
#   county_to_fix,
#   state_to_fix
# )
# saveRDS(bad_metrics, 'dev/bad_metrics.rds')
