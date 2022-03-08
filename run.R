# pull data from the internet
if (!dir.exists(here::here("inst/extdata/physionet"))) {
  dir.create(here::here("inst/extdata"), recursive = TRUE, showWarnings = FALSE)
  options(timeout = max(300, getOption("timeout")))
  download.file("https://zenodo.org/record/5794658/files/physionet.zip?download=1", method = "libcurl", destfile = here::here("inst/extdata/data.zip"))

  # if less than this, probably download error
  if (file.size(here::here("inst/extdata/data.zip")) < 300000000) {
    stop("Error downloading the dataset, the file seems too small. Please check what happened.")
  }

  unzip(here::here("inst/extdata/data.zip"), exdir = here::here("inst/extdata"), setTimes = TRUE, overwrite = FALSE)
  file.remove(here::here("inst/extdata/data.zip"))
}

if (dir.exists(here::here("inst/extdata"))) {
  if (isTRUE(as.logical(Sys.getenv("CI")))) {
    Rcpp::compileAttributes()
    renv::install(".")
    targets::tar_prune()
  } else {
    tryCatch(
      {
        pkg_date <- packageDate("false.alarm")
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

    targets::tar_watch(
      targets_only = TRUE, supervise = TRUE, seconds = 30, display = "graph", browse = TRUE, outdated = FALSE,
      label = c("time", "branches", "size"), port = 55444
    )
  }

  Sys.setenv(TAR_WARN = "false")

  # Uncomment to run targets sequentially on your local machine.
  # targets::tar_make()
  # Uncomment to run targets in parallel
  targets::tar_make_future(workers = 4L)
  # on local processes or a Sun Grid Engine cluster.
  # targets::tar_make_clustermq(workers = 2L)

  # Finally create the output files what will be used on this current Workflowr release:
  tryCatch(
    {
      message("Creating the outputs for Workflowr.")

      network <- targets::tar_visnetwork(TRUE, label = c("time", "size", "branches"))
      tips <- stringr::str_split_fixed(network$x$nodes$label, pattern = "\n", n = 2)[, 2]
      tips <- stringr::str_replace_all(tips, "\n", "<br />")
      network$x$nodes$title <- tips
      network$x$nodes$label <- network$x$nodes$name
      saveRDS(network, file = here::here("output/network.rds"))
      rm(network)
      rm(tips)

      source(here::here("scripts/common/create_output.R"), encoding = "UTF-8")
      create_output(file = here::here("output/work_output.rds"))
      rm(create_output)
    },
    error = function(e) {
      message("Could not create the outputs for Workflowr.")
    },
    finally = {
      message("done.")
    }
  )
} else {
  stop("Error installing dataset.")
}

# remove data before commit results
# if (dir.exists(here::here("inst/extdata"))) {
#   unlink(here::here("inst/extdata"), recursive = TRUE, force = TRUE)
# }
