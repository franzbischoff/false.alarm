# region Master Pipeline - Regime Detection
# Comment: This is the master script that documents the complete pipeline
# Comment: Run scripts in order: 10 -> 20 -> 30 -> 40

# region Pipeline Overview
# Comment: PHASE 1: GENERATION (Memory-intensive Matrix Profile computation)
# Comment:   10_prepare_data.R - Load and prepare ECG data
# Comment:   20_generate_matrix_profiles.R - Compute Matrix Profiles (one window size at a time)
#
# Comment: PHASE 2: PREDICTION (Exhaustive grid search and export)
# Comment:   30_predict_grid_search.R - Generate predictions with full grid search
# Comment:   40_convert_to_csv.R - Export to Python-compatible CSV format
# endregion Pipeline Overview

library(cli)
library(here)

# region Configuration
dataname <- "afib_regimes"
run_all <- FALSE # Set to TRUE to run all scripts sequentially
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
if (run_all) {
  cli::cli_alert_info("Running all scripts sequentially...")
  total_tic <- Sys.time()

  for (i in seq_along(scripts)) {
    script_path <- here("scripts", "regime_detection", scripts[i])

    cli::cli_h2("Running {scripts[i]}")
    tic <- Sys.time()
    source(script_path, encoding = "UTF-8")
    tac <- Sys.time()
    elapsed <- round(difftime(tac, tic, units = "mins"), 2)
    cli::cli_alert_success("{scripts[i]} completed in {elapsed} minutes")
    cli::cli_rule()
  }

  total_tac <- Sys.time()
  total_elapsed <- round(difftime(total_tac, total_tic, units = "hours"), 2)
  cli::cli_alert_success("All scripts completed in {total_elapsed} hours!")
} else {
  cli::cli_alert_info("To run all scripts, set run_all = TRUE")
  cli::cli_inform(c("i" = "Or run individual scripts:"))
  for (i in seq_along(scripts)) {
    cli::cli_inform(c("*" = "source('{here('scripts', 'regime_detection', scripts[i])}')"))
  }
}
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
