#
# Parameter Analysis Plotting Functions
#
# Visualization and reporting for FLOSS hyperparameter importance analysis.
#
# This script is independent and:
# - Loads cached analysis results from disk
# - Recalculates model performance metrics
# - Generates interaction plots (2-way parameter dependencies)
# - Creates variable importance comparison (FIRM, Permutation, SHAP methods)
# - Produces SHAP dependence plots (parameter effects on predictions)
# - Provides summary statistics
#
# USAGE in Rmd:
# ```r
# source(here("scripts", "regime_detection", "evaluation", "parameter_plots.R"))
# ```
#

# =============================================================================
# CONFIGURATION (Manual Definitions)
# =============================================================================
# nolint start
DATASET <- "afib_regimes" # Options: malignantventricular, afib_regimes, vtachyarrhythmias
METRIC <- "f3_weighted" # Options: f1_classic, f1_weighted, f3_classic, f3_weighted,
#          recall_4s, recall_10s, precision_4s, precision_10s,
#          edd_median_s, fp_per_min,
#          nab_score_standard, nab_score_low_fp, nab_score_low_fn

# SHAP/Permutation iterations (must match those used in parameter_analysis.R)
NSIM_FIRM <- 100
NSIM_PERM <- 100
NSIM_SHAP <- 400

# Cache directory for fitted models
CACHE_DIR <- here::here("output", "parameter_analysis")
# nolint end

# =============================================================================
# SETUP
# =============================================================================

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(cli)

cli_h1("Parameter Analysis - Plotting & Summary")
cli_alert_info("Dataset: {.val {DATASET}}")
cli_alert_info("Metric: {.val {METRIC}}")

# Check required packages
required_pkgs <- c("shapviz", "dbarts", "generics", "workflows", "yardstick")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli_abort("Package {.pkg {pkg}} not installed. Install before continuing.")
  }
  library(pkg, character.only = TRUE)
}

# =============================================================================
# LOAD CACHED RESULTS
# =============================================================================

cli_h2("Loading Analysis Results from Cache")

# Set up cache file paths
cache_file <- file.path(CACHE_DIR, glue::glue("bart_fitted_{DATASET}_{METRIC}.rds"))
cache_interactions <- file.path(
  CACHE_DIR,
  glue::glue("interactions_{DATASET}_{METRIC}.rds")
)
cache_importance <- file.path(CACHE_DIR, glue::glue("importances_{DATASET}_{METRIC}.rds"))

# Check all required cache files exist
missing_files <- NULL
if (!file.exists(cache_file)) missing_files <- c(missing_files, cache_file)
if (!file.exists(cache_interactions)) missing_files <- c(missing_files, cache_interactions)
if (!file.exists(cache_importance)) missing_files <- c(missing_files, cache_importance)

if (length(missing_files) > 0) {
  cli_abort("Missing cache files: {.path {missing_files}}\nRun parameter_analysis.R first.")
}

# Load BART model and extract data
cli_alert_info("Loading BART model from {.path {cache_file}}...")
trained_model <- readRDS(cache_file)
train_data <- trained_model$training_data
testing_data <- trained_model$testing_data
predictors_names <- c("window_size", "regime_threshold", "regime_landmark", "min_gap_samples")

cli_alert_success("Loaded training data: {nrow(train_data)} rows")
cli_alert_success("Loaded testing data: {nrow(testing_data)} rows")

# Refit model to get BART engine and performance metrics
# cli_alert_info("Refitting BART model on training data...")
# set.seed(102)
# best_fit <- generics::fit(trained_model$model, train_data)
# bart_engine <- workflows::extract_fit_engine(best_fit)

# # Calculate performance metrics
# pred <- predict(bart_engine, testing_data)
# pred <- colMeans(pred)

# rmse_val <- yardstick::rmse_vec(testing_data$mean, pred)
# rsq_val <- yardstick::rsq_vec(testing_data$mean, pred)

# cli_alert_success("Model RMSE: {.val {round(rmse_val, 4)}}")
# cli_alert_success("Model R²: {.val {round(rsq_val, 4)}}")

# Load interactions
cli_alert_info("Loading interactions from {.path {cache_interactions}}...")
interactions <- readRDS(cache_interactions)
cli_alert_success("Loaded {nrow(interactions)} interaction pairs")

# Load importance results
cli_alert_info("Loading importance results from {.path {cache_importance}}...")
importance_results <- readRDS(cache_importance)
importance_firm <- importance_results$importance_firm
importance_perm <- importance_results$importance_perm
importance_shap <- importance_results$importance_shap
shap_fastshap_all_test <- importance_results$shap_fastshap_all_test
cli_alert_success("Loaded importance data for all methods")

# For tree_data used in summary, we can reconstruct it from the original CSV
csv_path <- here("output", "regime_detection", DATASET, "evaluation", "models_aggregated.csv")
if (!file.exists(csv_path)) {
  cli_abort("CSV file not found: {.path {csv_path}}")
}

cli_alert_info("Loading original evaluation data...")
raw_data <- readr::read_csv(csv_path, show_col_types = FALSE)

outcome_name <- METRIC
tree_data <- raw_data |>
  select(all_of(c(predictors_names, outcome_name))) |>
  rename(mean = all_of(outcome_name)) |>
  filter(!is.na(mean), is.finite(mean))

cli_alert_success("Loaded {nrow(tree_data)} model configurations")
cli_alert_success("All data loaded successfully!")

# =============================================================================
# VISUALIZATION
# =============================================================================

cli_h2("Generating Plots")

# Plot 1: Interactions
cli_alert_info("Creating interaction plot...")
interactions_plot <- ggplot2::ggplot(interactions, ggplot2::aes(
  x = reorder(Variables, Interaction),
  y = Interaction, fill = Variables
)) +
  ggplot2::geom_col(color = "grey35", linewidth = 0.2) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = glue::glue("Parameter Interactions ({DATASET})"),
    subtitle = glue::glue("Metric: {METRIC}"),
    y = "Interaction Strength",
    x = NULL
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none")

print(interactions_plot)

# Plot 2: Importance Comparison (3 methods side-by-side)
cli_alert_info("Creating importance comparison plot...")

importance_firm_plot <- ggplot2::ggplot(importance_firm, aes(
  x = reorder(Variable, Importance),
  y = Importance, fill = Variable
)) +
  ggplot2::geom_col(colour = "grey35", linewidth = 0.8, show.legend = FALSE) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "FIRM",
    subtitle = "ICE curves",
    x = NULL, y = NULL
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.margin = margin(5, 5, 5, 10))

importance_perm_plot <- ggplot2::ggplot(importance_perm, aes(
  x = reorder(Variable, Importance, FUN = median),
  y = Importance, fill = Variable
)) +
  ggplot2::geom_boxplot(colour = "grey35", linewidth = 0.5, show.legend = FALSE) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Permutation",
    subtitle = glue::glue("{NSIM_PERM} iterations"),
    x = NULL, y = NULL
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.margin = margin(5, 5, 5, 10))

importance_shap_plot <- ggplot2::ggplot(importance_shap, aes(
  x = reorder(Variable, Importance),
  y = Importance, fill = Variable
)) +
  ggplot2::geom_col(colour = "grey35", linewidth = 0.8, show.legend = FALSE) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "SHAP",
    subtitle = glue::glue("{NSIM_SHAP} iterations"),
    x = NULL, y = NULL
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.margin = margin(5, 5, 5, 10))

all_importance <- (importance_firm_plot + plot_layout(tag_level = "keep")) |
  (importance_perm_plot + plot_layout(tag_level = "keep")) |
  (importance_shap_plot + plot_layout(tag_level = "keep")) +
    plot_layout(guides = "collect")

all_importance <- all_importance + plot_annotation(
  title = glue::glue("Variable Importance ({DATASET} - {METRIC})"),
  tag_levels = "A",
  theme = ggplot2::theme_bw() + ggplot2::theme(plot.title = ggplot2::element_text(size = 16))
)

print(all_importance)

# Plot 3: SHAP Dependence Plots (2×2 grid for 4 parameters)
cli_alert_info("Creating SHAP dependence plots...")

d1 <- shapviz::shapviz(shap_fastshap_all_test,
  X = testing_data[, predictors_names],
  baseline = mean(testing_data$mean)
)

shap_plots <- list()
for (feat in predictors_names) {
  shap_plots[[feat]] <- shapviz::sv_dependence(d1, feat, color_var = "auto") +
    ggplot2::geom_smooth(method = "loess", colour = "#0000ff44", alpha = 0.2, se = FALSE) +
    ggplot2::labs(y = "SHAP value", title = feat) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")
}

all_shap <- wrap_plots(shap_plots, ncol = 2) +
  plot_annotation(
    title = glue::glue("SHAP Dependence Plots ({DATASET} - {METRIC})"),
    subtitle = "Showing how each parameter affects model predictions",
    theme = ggplot2::theme_bw()
  )

print(all_shap)

# =============================================================================
# SUMMARY
# =============================================================================

cli_h2("Analysis Summary")

cli_alert_success("Dataset: {.val {DATASET}}")
cli_alert_success("Metric: {.val {METRIC}}")
cli_alert_success("Configurations analyzed: {.val {nrow(tree_data)}}")

cli_h3("Top 3 Most Important Parameters (SHAP)")
top_params <- importance_shap |>
  arrange(desc(Importance)) |>
  slice_head(n = 3) |>
  pull(Variable)

for (i in seq_along(top_params)) {
  cli_alert_info("{i}. {.field {top_params[i]}}")
}

cli_h3("Strongest Interactions")
top_interactions <- interactions |>
  slice_head(n = 3)

for (i in seq_len(nrow(top_interactions))) {
  cli_alert_info("{i}. {.field {top_interactions$Variables[i]}} (strength: {round(top_interactions$Interaction[i], 3)})")
}

cli_h3("Cache Files")
cli_alert_info("Model: {.path {cache_file}}")
cli_alert_info("Interactions: {.path {cache_interactions}}")
cli_alert_info("Importances: {.path {cache_importance}}")

cli_alert_success("Plotting complete! Visualizations generated above.")
