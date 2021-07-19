
win_complex <- function(data, window, dilution) {
  profile_size <- length(data) - window + 1
  av <- vector(mode = "numeric", length = profile_size)

  for (j in 1:profile_size) {
    av[j] <- tsmp:::complexity(data[j:(j + window - 1)])
  }

  # av <- tsmp:::zero_one_norm(av)
  # av <- av + dilution
  # av <- av / (dilution + 1L)

  return(av)
}
