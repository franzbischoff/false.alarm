# region Master Pipeline - Regime Detection
# This is the master script that documents the complete pipeline
# Run scripts in order: 10 -> 20 -> 30 -> 40

# region Pipeline Overview
# PHASE 1: GENERATION (Memory-intensive Matrix Profile computation)
#   10_prepare_data.R - Load and prepare ECG data
#   20_generate_matrix_profiles.R - Compute Matrix Profiles (one window size at a time)
#
# PHASE 2: PREDICTION (Exhaustive grid search and export)
#   30_predict_grid_search.R - Generate predictions with full grid search
#   40_convert_to_csv.R - Export to Python-compatible CSV format
# endregion Pipeline Overview

suppressPackageStartupMessages({
  library(cli, quietly = TRUE, warn.conflicts = FALSE)
  library(here, quietly = TRUE, warn.conflicts = FALSE)
})

# region Configuration
default_dataname <- "malignantventricular"
cli_args <- commandArgs(trailingOnly = TRUE)
dataname <- if (length(cli_args) >= 1L && nzchar(cli_args[1L])) cli_args[1L] else default_dataname
rscript_bin <- if (nzchar(Sys.which("Rscript"))) {
  Sys.which("Rscript")
} else {
  file.path(R.home("bin"), "Rscript")
}
# endregion Configuration

# region Script Paths
scripts <- c(
  "10_prepare_data.R",
  "20_generate_matrix_profiles.R",
  "30_predict_grid_search.R",
  "40_convert_to_csv.R"
)

script_descriptions <- c(
  "Prepare ECG data (load, resample, normalize, extract ground truth)",
  "Generate Matrix Profiles for all window sizes (25-400)",
  "Exhaustive grid search (threshold × landmark × min_gap_samples)",
  "Convert results to Python-compatible CSV format"
)
# endregion Script Paths

# region Display Pipeline
cli::cli_h1("Regime Detection Pipeline")
cli::cli_inform(c("i" = "Dataset: {dataname}"))
cli::cli_inform(c("i" = "Scripts directory: scripts/regime_detection/"))
cli::cli_rule()

for (i in seq_along(scripts)) {
  cli::cli_inform(c("*" = "{i}. {scripts[i]}"))
  cli::cli_inform(c(" " = "   {script_descriptions[i]}"))
}

cli::cli_rule()
# endregion Display Pipeline

# region Run Scripts
run_script <- function(script_name, description) {
  script_path <- here("scripts", "regime_detection", script_name)

  if (!file.exists(script_path)) {
    cli::cli_abort(c(
      "x" = "Script not found: {script_path}",
      "i" = "Check the scripts/regime_detection directory"
    ))
  }

  cli::cli_h2("{script_name}")
  cli::cli_inform(c(" " = description))

  tic <- Sys.time()
  status <- system2(
    command = rscript_bin,
    args = c(script_path, dataname),
    stdout = "",
    stderr = ""
  )
  tac <- Sys.time()
  elapsed <- round(difftime(tac, tic, units = "mins"), 2)

  if (!identical(status, 0L)) {
    cli::cli_abort(c(
      "x" = "{script_name} failed with status {status}",
      "i" = "Inspect the log above and fix the issue before continuing"
    ))
  }

  cli::cli_alert_success("{script_name} completed in {elapsed} minutes")
  cli::cli_rule()
}

cli::cli_alert_info("Running all scripts sequentially...")
cli::cli_inform(c("i" = "Dataset: {dataname}"))
cli::cli_inform(c("i" = "Rscript binary: {rscript_bin}"))

total_tic <- Sys.time()
for (i in seq_along(scripts)) {
  run_script(scripts[i], script_descriptions[i])
}
total_tac <- Sys.time()
total_elapsed <- round(difftime(total_tac, total_tic, units = "hours"), 2)
cli::cli_alert_success("All scripts completed in {total_elapsed} hours!")
# endregion Run Scripts

# region Output Structure
cli::cli_rule()
cli::cli_h2("Output Structure")
cli::cli_inform(c("i" = "All outputs saved to: output/regime_detection/{dataname}/"))
cli::cli_text("")
cli::cli_text("generation/")
cli::cli_text("  ├─ tidy_dataset.rds (from script 10)")
cli::cli_text("  ├─ matrix_profiles_w25.rds (from script 20)")
cli::cli_text("  ├─ matrix_profiles_w50.rds")
cli::cli_text("  └─ ... (one file per window size, 25-400)")
cli::cli_text("")
cli::cli_text("prediction/")
cli::cli_text("  └─ predictions_grid.rds (from script 30)")
cli::cli_text("")
cli::cli_text("{dataname}_predictions.csv (from script 40)")
cli::cli_rule()
# endregion Output Structure

# region Grid Search Dimensions
cli::cli_h2("Grid Search Dimensions")
cli::cli_inform(c("i" = "Window sizes: 16 values (25 to 400, step 25)"))
cli::cli_inform(c("i" = "Regime thresholds: 18 values (0.05 to 0.9, step 0.05)"))
cli::cli_inform(c("i" = "Regime landmarks: 15 values (2s to 9s, step 0.5s)"))
cli::cli_inform(c("i" = "Min gap samples: 6 values (200, 500, 1000, 2000, 3000, 5000)"))
cli::cli_inform(c("!" = "Total combinations per record: 16 × 18 × 15 × 6 = 25,920"))
cli::cli_rule()
# endregion Grid Search Dimensions
# endregion Master Pipeline
