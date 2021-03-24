# pull data from the internet
if (!dir.exists(here("inst/extdata"))) {
  download.file("https://bit.ly/39aaZl3", method = "libcurl", destfile = here("inst/data.zip"))
  unzip(here("inst/data.zip"), exdir = here("inst"), setTimes = TRUE, overwrite = FALSE)
  file.remove(here("inst/data.zip"))
}

if (dir.exists(here("inst/extdata"))) {
  # Uncomment to run targets sequentially on your local machine.
  targets::tar_make()
  # Uncomment to run targets in parallel
  # on local processes or a Sun Grid Engine cluster.
  # targets::tar_make_clustermq(workers = 2L)
} else {
  stop("Error installing dataset.")
}

# remove data before commit results
# if (dir.exists(here("inst/extdata"))) {
#   unlink(here("inst/extdata"), recursive = TRUE, force = TRUE)
# }
