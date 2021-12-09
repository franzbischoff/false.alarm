# Sys.setenv(TAR_PROJECT = "classifier")

# Load global config
source(here("scripts", "_globals.R"))

# Overwrite some configs
# The subset that will be keep from the dataset (seq.int(290 * const_sample_freq + 1, 300 * const_sample_freq) means the last 10 seconds)
var_subset <- seq.int(290 * const_sample_freq + 1, 300 * const_sample_freq) # last 10 secs
var_include <- "II"
# "time", "I", "II", "III", "V", "aVR", "aVL", "aVF", "PLETH", "ABP", "RESP", "MCL"
var_exclude <- setdiff(const_headers, var_include)

#### Pipeline: Start ----

r_input <- tar_files_input(
  #### Pipeline: Read files from directory ----
  file_paths,
  find_all_files(types = "asystole") # types = "asystole", "bradycardia", "tachycardia", "vfib", "vtachy"
)

r_dataset <- tar_target(
  #### Pipeline: Import the last 10 seconds of all TRUE and FALSE alarms ----
  dataset,
  {
    temp <- read_and_prepare_ecgs(file_paths,
      subset = var_subset # here we want all alarms
      # true_alarm = TRUE,
      # limit_per_type = var_limit_per_type
    )
    reshape_dataset_by_truefalse(temp, var_include)
  }
)

# R includes k-means, and the "flexclust" package can do k-means++


# rolling_origin {rsample}

#### Pipeline: Join targets ----
list(r_input, r_dataset)

#### Pipeline: End ----
