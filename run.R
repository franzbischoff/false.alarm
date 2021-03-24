# pull data from the internet
if (!dir.exists(here("inst/extdata/physionet"))) {
  dir.create(here("inst/extdata"), recursive = TRUE, showWarnings = FALSE)
  download.file("https://zenodo.org/record/4634014/files/physionet.zip?download=1", method = "libcurl", destfile = here("inst/extdata/data.zip"))
  unzip(here("inst/extdata/data.zip"), exdir = here("inst/extdata"), setTimes = TRUE, overwrite = FALSE)
  file.remove(here("inst/extdata/data.zip"))
}

if (dir.exists(here("inst/extdata"))) {
  # Uncomment to run targets sequentially on your local machine.
  targets::tar_make()
  # Uncomment to run targets in parallel
  # on local processes or a Sun Grid Engine cluster.
  # targets::tar_make_clustermq(workers = 2L)

  network <- tar_visnetwork(TRUE, label = c("time", "size", "branches"))
  tips <- stringr::str_split_fixed(network$x$nodes$label, pattern = "\n", n = 2)[, 2]
  tips <- stringr::str_replace_all(tips, "\n", "<br />")
  network$x$nodes$title <- tips
  network$x$nodes$label <- network$x$nodes$name
  saveRDS(network, file = here("output/network.rds"))
  rm(network)
  rm(tips)

} else {
  stop("Error installing dataset.")
}

# remove data before commit results
# if (dir.exists(here("inst/extdata"))) {
#   unlink(here("inst/extdata"), recursive = TRUE, force = TRUE)
# }
