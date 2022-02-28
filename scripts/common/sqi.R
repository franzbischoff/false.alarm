deriv <- function(data) {
  res <- NULL
  for (i in seq.int(2, length(data) - 1)) {
    d <- (data[i + 1] - data[i - 1]) / 2
    res <- c(res, d)
  }
  return(res)
}

sd_r <- function(data) {
  std(data)
}

rmssd_r <- function(data) {
  sqrt(exp(diff(data)^2))
}

# w 50?
# activity > 2
# complex > 8? if hist_diff ~0 , FP
# complex == 0, disconnected?
# turning points == 1 means disconnected?


# "Zero Crossing Rate "(ZCR): Counting of the number of times the signal change its amplitude from positive to negative values or vice versa.
# This value is then normalized dividing by the number of samples in the signal segment under study

zero_cross_rate <- function(data) {
  count <- 0
  for (i in seq.int(2, length(data))) {
    if ((data[i] * data[i - 1]) < 0) {
      count <- count + 1
    }
  }

  return(count / length(data))
}

# "Activity": Defined as the variance of the signal

activity <- function(data) {
  var(data)
}

# "Mobility": Squared root of the ratio of the variance of the first derivative of the signal to the
# variance of the original signal

mobility <- function(data) {
  act1der <- activity(diff(data))
  act <- activity(data)

  res <- sqrt(act1der / act)

  return(res)
}

# "Complexity": Ratio of the mobility of the first derivative of the signals to the mobility of the signal itself

complex <- function(data) {
  mob1der <- mobility(diff(data))
  mob <- mobility(data)

  res <- mob1der / mob

  return(res)
}


# Sum of Squared Differences
# sqrt(sum(diff(data)^2) / (w-2)) ==> sqrt(var(diff(data))) ==> sd(diff(data))
win_complex <- function(data, window) {
  profile_size <- length(data) - window + 1
  av <- vector(mode = "numeric", length = profile_size)

  for (j in 1:profile_size) {
    # av[j] <- tsmp:::complexity(data[j:(j + window - 1)])
    av[j] <- sum(diff(data[j:(j + window - 1)])^2)
  }

  av <- sqrt(av)

  # dilution: limit -> 1, max is 1, min approaches 1 for big dilution
  # av <- tsmp:::zero_one_norm(av)
  # av <- av + dilution
  # av <- av / (dilution + 1L)

  return(av)
}

# First-Difference histogram: The baseline is defined as the most common sample value during R-R
# periods. The sample value corresponding to the histogram peak [mode] was declared the baseline and the
# difference between consecutive baselines gives the baseline shift from beat to beat. Noise content
# is estimated from the first-difference histogram of R-R intervals. Noise contribution is one minus
# the frequency of occurrence of first differences with values around zero divided by the number of
# samples in the R-R interval [6].

hist_diff <- function(data, baseline = 0) {
  bin <- round(data, 2)
  ux <- unique(bin)
  mode <- ux[which.max(tabulate(match(bin, ux)))]
  return(abs(baseline - mode))
}

# 5 sec
turning_points <- function(data) {
  d <- abs(diff(sign(diff(data)))) == 2
  return(sum(d) / length(data))
}

# "Kurtosis": Measure of the Gaussianity of a distribution.
# As ECG signals are hyper-Gaussian, higher kurtosis values are associated with lower
# quality in the ECG. (DelRio2011)
# 10 sec

ecg_kurtosis <- function(data) {
  e1071::kurtosis(data)
}


# Difference between the original signal and the aligned averaged signal (Average): Noise is
# estimated as the difference between the original signal and the aligned averaged signal.
# Consequently the estimated SNR can be computed [2]
# -- the average of each repeating pattern to create a "template", then compute the residuals, then
#    low-pass ~15Hz, then add back to the average (there is a further step for adding the stretching)

# "Karhunen-Loeve transform" (KLT): KLT is a transformation that reduces a large set of variables down
# to a smaller set. The smaller set of variables separates the information of the different sources
# (ECG and Noise). In this way, noise can be estimated and the SNR calculated [3].

# "Turns counts" (TC): Counting of the number of local minimums with amplitude higher than a threshold. The threshold was defined as 0.1mV [3].
# THIS IS ROBUST TO NOISE, WE WANT TO MEASURE NOISE!

# "T-P interval average power divided by the QRS": T-P interval average power
# divided by the QRS average power. This is calculated for every beat [4].

# "Cumulative mismatch histogram": Mismatch values of consecutive QRS complex are stored as histograms
# for subsequent analysis generating a mismatch histogram. Cumulative histograms are then calculated.
# The signal quality is determined based on how fast the cumulative histogram curves rise. The signals
# with higher quality will rise faster than the signals with lower quality [5].

# Frequency content in six bandwidth and Out of range event (ORE): Energy of the signal in six
# frequency bandwidths (0.05-0.25, 0.25-10, 10-20, 20-48, 48- 52, and 52-100 Hz). ORE: counting of the
# number of times the signal go above or below a threshold. The threshold was defined as +-4 mV [7].

# "LMS adaptive filtering": LMS adaptive filtering was used to remove the ECG signal and therefore
# estimate the noise content. A template of the clean ECG signal was used as reference input signal to
# the adaptive filter. Then SNR can be estimated.

# "Temporal Dispersion": defined as:
