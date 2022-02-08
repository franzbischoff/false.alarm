#!/usr/bin/Rscript --vanilla

source("renv/activate.R")

renv::restore()

tryCatch(
  {
    pkg_date <- utils::packageDate("false.alarm")
    files_r <- paste0(here::here("R"), "/", list.files(here::here("R"), pattern = "*.R"))
    files_cpp <- paste0(here::here("R"), "/", list.files(here::here("src"), pattern = "*.cpp"))
    files_h <- paste0(here::here("R"), "/", list.files(here::here("src"), pattern = "*.h"))
    files <- c(files_r, files_cpp, files_h)
    changed <- na.omit(files[as.Date(file.mtime(files)) > (pkg_date)])

    if (length(changed) > 0) {
      message("false.alarm is outdated, trying to automatically resolve this.")
      Rcpp::compileAttributes()
      renv::install(".")
    }
    rm("pkg_date", "files_r", "files_cpp", "files_h", "files", "changed")
  },
  error = function(e) {
    message("false.alarm is not installed, trying to automatically resolve this.")
    Rcpp::compileAttributes()
    renv::install(".")
  },
  finally = {
    message("done.")
  }
)

renv::isolate()
