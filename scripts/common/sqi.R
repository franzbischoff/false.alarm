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

zero_cross <- function(data) {
  count <- 0
  for (i in seq.int(2, length(data))) {
    if ((data[i] * data[i - 1]) < 0) {
      count <- count + 1
    }
  }

  return(count)
}

activity <- function(data) {
  var(data)
}

mobility <- function(data) {
  act1der <- activity(diff(data))
  act <- activity(data)

  res <- sqrt(act1der / act)

  return(res)
}

complex <- function(data) {
  mob1der <- mobility(diff(data))
  mob <- mobility(data)

  res <- mob1der / mob

  return(res)
}

hist_diff <- function(data) {
  return(abs(-0.04870981 - median(data)))
}

# 5 sec
turning_points <- function(data) {
  d <- abs(diff(sign(diff(data)))) == 2
  return(sum(d))
}

# As ECG signals are hyper-Gaussian, higher kurtosis values are associated with lower
# quality in the ECG. (DelRio2011)

# 10 sec
ecg_kurtosis <- function(data) {
  e1071::kurtosis(data)
}

# "Karhunen-Loeve transform" (KLT): KLT is a transformation that reduces a large set of variables down
# to a smaller set. The smaller set of variables separates the information of the different sources
# (ECG and Noise). In this way, noise can be estimated and the SNR calculated [3].

# "Activity": Defined as the variance of the signal [3].

# "Mobility": Squared root of the ratio of the variance of the first derivative of the signal to the
# variance of the original signal [3].

# "Complexity": Ratio of the mobility of the first derivative of the signals to the mobility of the signal itself [3].

# "Turns counts" (TC): Counting of the number of local minimums with amplitude higher than a threshold. The threshold was defined as 0.1mV [3].
# THIS IS ROBUST TO NOISE, WE WANT TO MEASURE NOISE!

# "Zero Crossing Rate "(ZCR): Counting of the number of times the signal change its amplitude from positive to negative values or vice versa.
# This value is then normalized dividing by the number of samples in the signal segment under study
# [3].

# "T-P interval average power divided by the QRS": TP=turning points? T-P interval average power divided by the QRS average power. This is calculated for every beat [4].

# "Cumulative mismatch histogram": Mismatch values of consecutives QRS complex are stored as histograms for subsequent analysis generating a mismatch

# First-Difference histogram

# Frequency content in six bandwidth and Out of range event (ORE)

# "LMS adaptive filtering": LMS adaptive filtering was used to remove the ECG signal and therefore
# estimate the noise content. A template of the clean ECG signal was used as reference input signal to
# the adaptive filter. Then SNR can be estimated.

# "Kurtosis": Measure of the Gaussianity of a distribution [8]. As ECG signals are hyper-Gaussian,
# higher kurtosis values are associated with lower quality in the ECG.

# "Temporal Dispersion": defined as:
