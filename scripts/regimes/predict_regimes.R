predict_regimes <- function(floss_list, window_size, time_constraint, regime_threshold,
                            regime_landmark,
                            ez = 0.5, history = 5000,
                            sample_freq = 250, batch = 100) {
  regimes <- extract_regimes(floss_list,
    params = list(
      window_size = window_size,
      ez = ez,
      regime_threshold = regime_threshold,
      regime_landmark = floor(regime_landmark * sample_freq), # 3 sec from the end
      batch = batch,
      history = history,
      mp_time_constraint = time_constraint,
      floss_time_constraint = 0
    ),
    infos = list(foo = "bar")
  )

  if (isFALSE(regimes)) {
    return(1)
  } else {
    return(regimes$idxs)
  }
}
