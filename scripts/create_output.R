create_output <- function(file = here::here("output/work_output.rds")) {
  floss_objects_names_wc <- tar_manifest(matches("ds_stats_mps_floss.*(1250_0|0_1250)"), "name")$name
  floss_objects_names_nc <- tar_manifest(matches("ds_stats_mps_nc_floss.*(1250_0|0_1250)"), "name")$name
  mp_constraint <- tar_read_raw(floss_objects_names_wc[1])
  floss_constraint <- tar_read_raw(floss_objects_names_nc[1])

  saveRDS(list(mp_constraint = mp_constraint, floss_constraint = floss_constraint), file)
}
