#' Retrieve a list of files for ECG read
#'
#' This function reads
#'

find_all_files <- function(path = here::here("inst/extdata/physionet"),
                           data_type = c("alarm", "regimes"),
                           classes = c("all", "asystole", "bradycardia", "tachycardia", "fibv", "vtachy", "persistent_afib", "paroxistical_afib", "non_afib"),
                           limit_per_class = 1000,
                           long = FALSE) {
  "!DEBUG Starting process"

  checkmate::assert_directory_exists(path, access = "r")
  classes <- match.arg(classes, several.ok = TRUE)

  if (long) {
    files <- paste0(
      path, "/",
      list.files(here::here(path),
        pattern = "l\\.hea$"
      )
    )
  } else {
    files <- paste0(
      path, "/",
      list.files(here::here(path),
        pattern = "\\.hea$"
      )
    )
  }

  if (!("all" %in% classes)) {
    filtered <- NULL

    if (data_type == "alarm") {
      for (class in classes) {
        res <- switch(class,
          asystole = grep("a\\d*.\\.hea", files, value = TRUE),
          bradycardia = grep("b\\d*.\\.hea", files, value = TRUE),
          tachycardia = grep("t\\d*.\\.hea", files, value = TRUE),
          fibv = grep("f\\d*.\\.hea", files, value = TRUE),
          vtachy = grep("v\\d*.\\.hea", files, value = TRUE)
        )

        res <- head(res, limit_per_class)

        filtered <- c(filtered, res)
      }
    } else if (data_type == "regimes") {
      for (class in classes) {
        res <- switch(class,
          persistent_afib = grep("*.\\.per\\.hea", files, value = TRUE),
          paroxistical_afib = grep("*.\\.par\\.hea", files, value = TRUE),
          non_afib = grep("*.\\.non\\.hea", files, value = TRUE)
        )

        res <- head(res, limit_per_class)

        filtered <- c(filtered, res)
      }
    } else {
      stop("Invalid type: ", data_type)
    }
    files <- filtered
  }

  return(files)
}
