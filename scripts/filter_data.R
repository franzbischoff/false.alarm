filter_mp <- function(ecg_data, params) {
  mp <- attr(ecg_data, params$attribute)
  filter <- attr(ecg_data, "filters")
  filter <- filter[[params$filter]]

  mp$matrix_profile[filter] <- Inf
  mp$profile_index[filter] <- which(filter)

  attr(ecg_data, params$attribute) <- mp

  return(ecg_data)
}

filter_data <- function(ecg_data, params) {
  window <- params$window

  norm_data <- matrixprofiler::znorm(ecg_data)
  mean_std <- matrixprofiler::movmean_std(norm_data, window)
  mean_of_sd <- mean(mean_std$sd)

  filters <- list()

  # # wander
  # filters$wander <- (mean_of_sd < mean_std$sd)
  # # mean more than sd
  # filters$mean_more <- (abs(mean_std$avg) >= mean_std$sd)
  # complex
  filters$complex <- win_complex(norm_data, window, 0)
  filters$complex_lim <- filters$complex > params$cplx_lim
  # # abs-mean
  # filters$absmean <- abs(mean_std$avg)
  # # std
  # filters$std <- mean_std$sd
  # # std
  # filters$sig <- mean_std$sig
  # # sum
  # filters$sum <- mean_std$sum
  # # sqrsum
  # filters$sqrsum <- mean_std$sqrsum
  # kurtosis
  filters$kurtosis <- zoo::rollapply(norm_data, window, e1071::kurtosis, align = "left")
  # skewness
  filters$skewness <- zoo::rollapply(norm_data, window, e1071::skewness, align = "left")

  attr(ecg_data, params$attribute) <- filters

  return(ecg_data)
}

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


plot_filters <- function(ecg_data) {
  checkmate::qassert(ecg_data, "L")

  file <- names(ecg_data)
  checkmate::qassert(file, "S1")
  names <- names(ecg_data[[file]])
  names <- setdiff(names, c("time", "PLETH", "ABP"))

  def_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(def_par))

  graphics::par(mfrow = c(1, 1))

  split <- (60000:74801)

  for (n in names) {
    filters <- attr(ecg_data[[file]][[n]], "filters")
    plot(scale(ecg_data[[file]][[n]][split], scale = FALSE),
      type = "l",
      main = paste(file, n),
      ylim = c(-3, 20)
    )
    lines(filters$complex[split], col = "red")
    lines(abs(filters$kurtosis[split]), col = "green")
    lines(abs(filters$skewness[split]), col = "blue")
    # points(filters$wander, col = "red", pch = "x")
    # points(filters$mean_more, col = "blue", pch = "+")
    abline(v = 72500 - split[1], col = "black")
  }
}

plot_ecg <- function(data) {
  plots <- list()
  filters <- attr(data, "filters")
  names <- names(filters)
  info <- attr(data, "info")
  for (i in seq_len(length(filters) + 1)) {
    if (i == 1) {
      plots[[i]] <- plotly::plot_ly(
        x = as.POSIXct(seq_len(length(data)), origin = "1970-01-01", tz = "UTC"),
        y = data,
        mode = "lines", type = "scatter",
        name = "data"
      )
    } else {
      plots[[i]] <- plotly::plot_ly(
        x = as.POSIXct(seq_len(length(filters[[i - 1]])), origin = "1970-01-01", tz = "UTC"),
        y = filters[[i - 1]],
        mode = "lines", type = "scatter",
        name = names[i - 1]
      )
    }
    plots[[i]] <- plotly::layout(plots[[i]],
      margin = list(t = 80, b = 50, l = 50, r = 50, pad = 0),
      title = paste0(
        "Monitor multiplot from file '", info$filename, "'\n",
        info$alarm, " = ", info$true
      ),
      xaxis = list(
        title = "time",
        type = "date",
        ticklen = 5,
        tickformat = "%M:%S.%2f",
        rangeselector = list(
          visible = TRUE,
          buttons = list(
            list(
              count = 4,
              label = "4 secs",
              step = "second",
              stepmode = "backward"
            ),
            list(
              count = 10,
              label = "10 secs",
              step = "second",
              stepmode = "backward"
            ),
            list(
              count = 30,
              label = "30 secs",
              step = "second",
              stepmode = "backward"
            ),
            list(
              count = 60,
              label = "60 secs",
              step = "second",
              stepmode = "backward"
            ),
            list(step = "all")
          )
        ),
        rangeslider = TRUE
      )
      # yaxis = list(
      #   autorange = "visible" # not working yet
      #   fixedrange = FALSE
      # )
    )
  }

  fig <- plotly::subplot(plots, nrows = length(names) + 1, shareX = TRUE)
  print(fig)
  return(invisible(fig))
}
