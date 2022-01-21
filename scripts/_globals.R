library(targets)
library(tarchetypes)
library(purrr)

#### Dev variables ----

dev_mode <- FALSE # !identical(Sys.getenv("DEBUGME"), "")
skip_graphics <- TRUE

if (dev_mode) {
  # I know I shall not use this
  devtools::load_all(".")
  # Sys.setenv(DEBUGME_OUTPUT_FILE = "debugme.log")
}

# examples: a109l_aVF_300_0_raw
#           a161l (asystole) 300_1250, false neg

# CONCEPT: Concept: in Euclidean space, values above sqrt(2*w) are negativelly correlated on Pearson's
# Contrast Profile: CPm = ( MPm(+−) − MPm(++))/ sqrt(2 * m)
# CONCEPT: how near to consider to update the MP
# CONCEPT: density of similarity changes a little bit from the size of constraint, but a lot due to the window size. but, the sum of the density from 50-100 doesnt change in any case


#### Physionet's dataset definitions ----
# Asystole: No QRS for at least 4 seconds
# Ventricular Flutter/Fibrillation: Fibrillatory, flutter, or oscillatory waveform for at least 4 .leap.seconds

# Extreme Bradycardia: Heart rate lower than 40 bpm for 5 consecutive beats
# Extreme Tachycardia: Heart rate higher than 140 bpm for 17 consecutive beats
# Ventricular Tachycardia: 5 or more ventricular beats with heart rate higher than 100 bpm

#### General: Config variables ----

options(tidyverse.quiet = TRUE)
options(target_ds_path = "inst/extdata/physionet")
options(crayon.enabled = TRUE)
cluster <- FALSE
backend <- "FUTURE"

#### Targets: Load Scripts ----

# Load all scripts
script_files <- list.files(here::here("scripts", "common"), pattern = "*.R")
sapply(here::here("scripts", "common", script_files), source, local = .GlobalEnv, encoding = "UTF-8")
rm(script_files)

#### Targets: Setup engine ----

if (isFALSE(cluster)) { ## Locally
  if (backend == "FUTURE") {
    library(future)
    library(future.callr)
    future::plan(callr)
  } else {
    options(
      clustermq.scheduler = "multiprocess",
      clustermq.ssh.host = NULL,
      clustermq.ssh.log = NULL
    )
    library(clustermq)
  }
} else {
  if (backend == "FUTURE") { ## cluster # tar_make_future(workers = 4)
    # *** If using future for multithreading / clustering ***
    library(future)
    library(future.batchtools)

    future::plan(
      strategy = future.batchtools::batchtools_custom,
      cluster.functions = batchtools::makeClusterFunctionsSSH(
        list(
          # batchtools::Worker$new("franz@192.168.1.237", ncpus = 4)
          batchtools::Worker$new("localhost", ncpus = 4)
        ),
        # fs.latency = 1000
      )
    )
  } else { # tar_make_clustermq(workers = 3)
    options(
      clustermq.scheduler = "ssh",
      clustermq.ssh.host = "franz@192.168.1.237", # use your user and host, obviously
      clustermq.ssh.log = "~/cmq_ssh.log" # log for easier debugging # nolint
    )
    library(clustermq)
  }
}

#### Targets: Define targets options ----

# use renv::install(".") to update the rcpp functions
tar_option_set(
  packages = c("dplyr", "false.alarm"),
  format = "rds",
  resources = tar_resources(
    #   #   #   # *** If using clustermq for multithreading / clustering ***
    # clustermq = tar_resources_clustermq(
    #   template = list(num_cores = 4)
    # ), # or n_jobs??
    #   #   #   # *** If using future for multithreading / clustering ***
    future = tar_resources_future(
      resources = list(n_cores = 4)
    )
  ),
  garbage_collection = TRUE,
  # workspace_on_error = TRUE,
  memory = "transient",
  # storage = "main",
  # envir = globalenv(),
  # iteration = "list",
  # debug = "ds_stats_mps_floss2_0_1250_0_200_fa978ffc",
  # cue = tar_cue(
  #   mode = "thorough",
  #   command = TRUE,
  #   depend = TRUE,
  #   format = TRUE,
  #   iteration = TRUE,
  #   file = FALSE
  # ),
  imports = "false.alarm" # TODO: remove when there is no change on package functions. Clears the graph.
)

#### Pipeline: variable definitions ----
# signal sample frequency, this is a constant
const_sample_freq <- 250
# known series headers;
# V: unspecified precordial lead
# MCL: modified precordial lead (unspecified) (read abstract of https://doi.org/10.1016/0735-1097(91)90762-X)
# ABP: arterial blood pressure (invasive, from one of the radial arteries)
# PLETH: uncalibrated raw output of fingertip plethysmograph
# RESP: uncalibrated respiration waveform, estimated from thoracic impedance
const_signals <- c("time", "I", "II", "III", "aVR", "aVL", "aVF", "V", "MCL", "PLETH", "ABP", "RESP")
const_classes <- c("asystole", "bradycardia", "tachycardia", "fibv", "vtachy")
# keep only the X filenames
var_head <- 10
# The subset that will be keep from the dataset (seq.int(240 * const_sample_freq + 1, 300 * const_sample_freq) means the last 60 seconds)
var_subset <- seq.int(240 * const_sample_freq + 1, 300 * const_sample_freq) # last 60 secs
# keep only the X files per class (asystole, bradycardia, etc)
var_limit_per_class <- 2

var_classes_include <- "asystole"
var_classes_exclude <- setdiff(const_classes, var_classes_include)

var_signals_include <- "II"
var_signals_exclude <- setdiff(const_signals, var_signals_include) # c("time", "V", "ABP", "PLETH", "RESP")

# window size of the filters used to remove spurious data
var_filter_w_size <- 100 # c(100, 200)

# Matrix Profile:
## batch size of the online matrix profile algorithm (doesn't affect the results, only jumps snapshots for brevity)
var_mp_batch <- 100
## size of the online MP buffer to keep memory footprint low (multiply by the freq to have it in seconds)
var_mp_history <- 20 * const_sample_freq # 20 secs
## Threshold used on computing the MP. This concept is described elsewhere. Values above 10 use another formula
var_mp_threshold <- c(0, 0.5) # c(0, 0.5, 0.6, 0.9, 50, 60, 90)
## the window size for the MP algorithm
var_window_size <- c(200, 250)
## Multiplier for the size of the Exclusion Zone (e.g., window_size * ez == exclusion_zone)
var_ez <- 0.5
## time constraint used on MP. Don't use both mp_time_constraint and floss_time_constraint (multiply by the freq to have it in seconds)
var_mp_time_constraint <- c(0, 5 * const_sample_freq, 10 * const_sample_freq) # , 17 * const_sample_freq)

# FLOSS:
## time constraint used on FLOSS. Don't use both mp_time_constraint and floss_time_constraint (multiply by the freq to have it in seconds)
var_floss_time_constraint <- c(0, 5 * const_sample_freq, 10 * const_sample_freq) # , 17 * const_sample_freq)

# Regime change detection:
## Index position where the regime change algorithm will focus the attention
var_regime_landmark <- 3 * const_sample_freq # 3 seconds from the end
## threshold below the regime change algorithm will trigger
var_regime_threshold <- c(0.3, 0.4, 0.5) # c(0.1, 0.2, 0.3, 0.4, 0.5)

# debug(process_ts_in_file)
# tar_make(names = ds_mp_filtered, callr_function = NULL)

# start debugme after loading all functions
if (dev_mode) {
  debugme::debugme()
}
