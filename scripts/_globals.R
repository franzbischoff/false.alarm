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

# CONCEPT: Concept: in Euclidean space, values above sqrt(2*w) are negatively correlated on Pearson's
# Contrast Profile: CPm = ( MPm(+−) − MPm(++))/ sqrt(2 * m)
# CONCEPT: how near to consider to update the MP
# CONCEPT: density of similarity changes a little bit from the size of constraint,
#    but a lot due to the window size. but, the sum of the density from 50-100 doesn't change in any case

# CONCEPT: https://www.wikiwand.com/en/Platt_scaling for classification


#### Physionet's dataset definitions ----
# Asystole: No QRS for at least 4 seconds
# Ventricular Flutter/Fibrillation: Fibrillatory, flutter, or oscillatory waveform for at least 4 .leap.seconds

# Extreme Bradycardia: Heart rate lower than 40 bpm for 5 consecutive beats
# Extreme Tachycardia: Heart rate higher than 140 bpm for 17 consecutive beats
# Ventricular Tachycardia: 5 or more ventricular beats with heart rate higher than 100 bpm

#### General: Config variables ----

options(tidyverse.quiet = TRUE)
options(dplyr.summarise.inform = FALSE)
options(target_ds_path = "inst/extdata/physionet")
options(crayon.enabled = TRUE)

#### Targets: Load Scripts ----

# Load all scripts
script_files <- list.files(here::here("scripts", "common"), pattern = "*.R")
sapply(here::here("scripts", "common", script_files), source, local = .GlobalEnv, encoding = "UTF-8")
rm(script_files)
source(here::here("scripts", "helpers", "glue_fmt.R"), local = .GlobalEnv, encoding = "UTF-8")

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
# if (dev_mode) {
#   debugme::debugme()
# }
