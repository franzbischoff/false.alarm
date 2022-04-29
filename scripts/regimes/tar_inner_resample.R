source(here::here("scripts", "helpers", "utils_targets.R"), encoding = "UTF-8")

# training(split)
# testing(split)
# analisys(inner_resamples$splits)
# assessment(inner_resamples$splits)

# parsnip::new_model_spec
# tune::tune_grid
#


tar_inner_resample <- function(values, ..., names = tidyselect::everything()) {
  targets <- unlist(list(...), recursive = TRUE) %|||% list()
  targets::tar_assert_target_list(targets)
  tarchetypes:::assert_values_list(values)
  names_quosure <- rlang::enquo(names)
  names <- tarchetypes:::eval_tidyselect(names_quosure, base::names(values))
  values <- tibble::as_tibble(values)
  values <- tar_map_process_values(values)
  values <- tar_map_extend_values(targets, values, names)
  out <- lapply(targets, tar_map_target, values = values)
  flat <- unlist(out, recursive = TRUE)
  if_any(
    unlist,
    tarchetypes:::set_names(flat, tarchetypes:::map_chr(flat, ~ .x$settings$name)),
    tarchetypes:::set_names(out, tarchetypes:::map_chr(targets, ~ .x$settings$name))
  )
}

tar_map_process_values <- function(values) {
  for (name in names(values)) {
    values[[name]] <- map(
      values[[name]],
      ~ parse(text = targets::tar_deparse_safe(.x))[[1]]
    )
  }
  values
}

tar_map_extend_values <- function(targets, values, names) {
  suffix <- tar_map_produce_suffix(values, names)
  for (target in targets) {
    name <- target$settings$name
    targets::tar_assert_not_in(
      name,
      names(values),
      paste("target", name, "cannot be in names(values).")
    )
    values[[name]] <- as_symbols(make.names(paste(name, suffix, sep = "_")))
  }
  values
}

tar_map_produce_suffix <- function(values, names) {
  data <- values[names] %||% tar_map_default_suffixes(values)
  data <- map(data, ~ as.character(unlist(.x)))
  out <- apply(as.data.frame(data), 1, paste, collapse = "_")
  out <- gsub("'", "", out)
  out <- gsub("\"", "", out)
  make.unique(out, sep = "_")
}

tar_map_default_suffixes <- function(values) {
  id <- apply(
    X = values,
    MARGIN = 1,
    FUN = digest::digest,
    algo = "xxhash32"
  )
  list(id = id)
}

tar_map_target <- function(target, values) {
  lapply(
    tarchetypes:::transpose(values),
    tar_map_iter,
    target = target,
    command = target$command$expr,
    pattern = target$settings$pattern
  )
}

tar_map_iter <- function(values, target, command, pattern) {
  settings <- target$settings
  name <- as.character(values[[settings$name]])
  command <- tarchetypes:::substitute_expr(command, values)
  pattern <- tarchetypes:::substitute_expr(pattern, values) %||% NULL
  targets::tar_target_raw(
    name = name,
    command = command,
    pattern = pattern,
    packages = target$command$packages,
    library = target$command$library,
    format = settings$format,
    repository = settings$repository,
    iteration = settings$iteration,
    error = settings$error,
    memory = settings$memory,
    garbage_collection = settings$garbage_collection,
    deployment = settings$deployment,
    priority = settings$priority,
    resources = settings$resources,
    storage = settings$storage,
    retrieval = settings$retrieval,
    cue = targets::tar_cue(
      mode = target$cue$mode,
      command = target$cue$command,
      depend = target$cue$depend,
      format = target$cue$format,
      repository = target$cue$repository,
      iteration = target$cue$iteration,
      file = target$cue$file
    )
  )
}
