
create_floss_training <- function() {
  "!DEBUG: List files."
  files_paths <- find_all_files()

  "!DEBUG Set sample table."
  training <- tibble::tribble(
    ~file, ~start, ~end,
    "a103l", 5000, 35000, # Asystole #False alarm
    "a104s", 21400, 30000, # Asystole #False alarm
    "a105l", 1, 39000, # Asystole #False alarm
    "a134s", 1, 40000, # Asystole #False alarm
    # "a163l", 1, 40000, # Asystole #False alarm
    # "a165l", 1, 40000, # Asystole #False alarm
    # "a171l", 1, 20000, # Asystole #False alarm
    # "a604s", 1, 64000, # Asystole #True alarm
    # "b339l", 1, 63000, # Bradycardia #False alarm
    # "b387l", 1, 75000, # Bradycardia #False alarm
    # "b617l", 1, 75000, # Bradycardia #False alarm # bad signal
    # "f260s", 12000, 36000, # Ventricular_Flutter_Fib #False alarm
    # "f321l", 34000, 56000, # Ventricular_Flutter_Fib #False alarm # bad signal good signal
    # "f352s", 32000, 68000, # Ventricular_Flutter_Fib #False alarm
    # "f563l", 30000, 60000, # Ventricular_Flutter_Fib #True alarm
    # "t117l", 25000, 40000, # Tachycardia #True alarm
    # "t195l", 27000, 37000, # Tachycardia #True alarm
    # "t622s", 14000, 71000, # Tachycardia #True alarm
    # "v111l", 25000, 70000, # Ventricular_Tachycardia #False alarm
    # "v148s", 1, 22000, # Ventricular_Tachycardia #False alarm
    # "v197l", 1, 50000, # Ventricular_Tachycardia #True alarm
    # "v833l", 1, 59000 # Ventricular_Tachycardia #False alarm

    # "v128s", 12000, 21000, # Ventricular_Tachycardia #False alarm
    # "b517l", 1, 75000, # Bradycardia #True alarm xxx
    # "f697l", 1, 75000, #Ventricular_Flutter_Fib #True alarm
    # "t409l", 1, 75000, #Tachycardia #False alarm bad signal
    # "v217l", 1, 75000 #Ventricular_Tachycardia #False alarm # complex signal # zero line makes cac zero
  )

  files <- NULL
  for (file in training$file) {
    idx <- grep(file, files_paths)
    if (idx > 0) {
      files <- c(files, files_paths[idx])
    }
  }

  targets_dataset <- purrr::flatten(sapply(files, function(x) read_ecg(x), simplify = FALSE))

  training_data <- list()

  "!DEBUG Building data list."
  j <- 1
  for (i in seq_along(targets_dataset)) {
    filename <- attr(targets_dataset[[i]], "info")$filename
    frequency <- attr(targets_dataset[[i]], "info")$frequency
    idxs <- dplyr::filter(training, file == filename)

    if (nrow(idxs) == 1) {
      training_data[[j]] <- targets_dataset[[i]]$II[seq.int(idxs$start, idxs$end)]
      attr(training_data[[j]], "info") <- list(frequency = frequency)
      j <- j + 1
    }
  }

  names(training_data) <- training$file

  return(training_data)
}
