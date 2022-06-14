create_output <- function(file = here::here("output/work_output.rds"), target = c("main", "regime_optimize")) {
  if (target == "main") {
    floss_objects_names_wc <- tar_manifest(matches("ds_stats_mps_floss.*(1250_0|0_1250)"), "name")$name
    floss_objects_names_nc <- tar_manifest(matches("ds_stats_mps_nc_floss.*(1250_0|0_1250)"), "name")$name
    mp_constraint <- tar_read_raw(floss_objects_names_wc[1])
    floss_constraint <- tar_read_raw(floss_objects_names_nc[1])

    saveRDS(list(mp_constraint = mp_constraint, floss_constraint = floss_constraint), file, compress = "xz")
  } else if (target == "regime_optimize") {
    fitted_models <- tar_read_raw("analysis_fitted")
    evaluated_models <- tar_read_raw("analysis_evaluation")
    final_evaluation <- tar_read_raw("testing_evaluation")

    saveRDS(list(fitted_models = fitted_models, evaluated_models = evaluated_models, final_evaluation = final_evaluation), file, compress = "xz")
  } else {
    stop("Unknown target.")
  }
}
