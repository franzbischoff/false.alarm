#' Retrieve a list of files for ECG read
#'
#' This function retrieves a list of all ".hea" files in a folder, using defined
#' constraints.
#'
#' @param path a string. The path of the desired folder.
#' @param data_type a string. The dataset type ("alarm" or "regimes")
#' @param classes character vector. Defines which classes will be imported. Default is "all"
#' @param limit_per_class integer. Limit the size of the dataset by number of the same class. Used
#'                        to speed up the pipeline before the final trial.
#' @param long a logical. Only for the "alarm" dataset. Means that only the long series will be retrieved.

find_all_files <- function(path = here::here("inst/extdata/physionet"),
                           data_type = c("alarm", "regimes"),
                           classes = c("all", "asystole", "bradycardia", "tachycardia", "fibv", "vtachy", "persistent_afib", "paroxysmal_afib", "non_afib"),
                           limit_per_class = 1000,
                           long = FALSE) {
  "!DEBUG Starting process"

  checkmate::assert_directory_exists(path, access = "r")
  data_type <- match.arg(data_type)
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
          paroxysmal_afib = grep("*.\\.par\\.hea", files, value = TRUE),
          non_afib = grep("*.\\.non\\.hea", files, value = TRUE)
        )

        res <- head(res, limit_per_class)

        filtered <- c(filtered, res)
      }
    } else {
      rlang::abort("Invalid type: ", data_type)
    }
    files <- filtered
  }

  return(files)
}
