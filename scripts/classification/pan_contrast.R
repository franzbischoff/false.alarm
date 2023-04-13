pan <- function(split, shapelet_sizes, progress = FALSE) {
  checkmate::qassert(split, "L4")
  checkmate::qassert(shapelet_sizes, "N+")

  "!DEBUG Compute Pan CP"

  idxs <- (split$data$alarm == "true")

  true_alarms <- unlist(split$data$values[idxs])
  true_alarms <- as.numeric(true_alarms[!is.na(true_alarms)])


  false_alarms <- unlist(split$data$values[!idxs])
  false_alarms <- as.numeric(false_alarms[!is.na(false_alarms)])

  result <- list()

  for (i in shapelet_sizes) {
    true_alarms_val <- validate_data(true_alarms, i %/% 2)
    false_alarms_val <- validate_data(false_alarms, i %/% 2)
    res <- contrast(false_alarms_val, true_alarms_val, i, progress = progress)
    result[[as.character(i)]] <- res
  }

  return(result)
}

get_pan_contrast <- function(contrast_profiles, position = 1) {
  checkmate::qassert(contrast_profiles, "L+")

  "!DEBUG Get First Pan CP"
  w_sizes <- names(contrast_profiles)
  n_sizes <- as.numeric(w_sizes)
  cp_len <- nrow(contrast_profiles[[1]]$cps)

  pan_cp <- matrix(NA, ncol = length(w_sizes), nrow = cp_len)

  for (i in seq_len(length(w_sizes))) {
    # adding NA to the left makes a better pan plot, since we are working with streams, we look backwards
    repad <- c(rep(NA, n_sizes[i]), contrast_profiles[[i]]$cps[1:(cp_len - n_sizes[i]), position])

    # pan_cp[, i] <- contrast_profiles[[i]]$cps[, 1]
    pan_cp[, i] <- repad
    # print(paste("Plotting Pan Contrast Profile for shapelet size", i, "name", names(contrast_profiles[[i]])))
    # res <- pan_contrast[[as.character(i)]]
    # plot_contrast(res, i, num_shapelets)
  }

  colnames(pan_cp) <- w_sizes
  return(pan_cp)
}

get_pan_platos <- function(contrast_profiles, position = 1) {
  checkmate::qassert(contrast_profiles, "L+")

  "!DEBUG Get First Pan CP"
  w_sizes <- names(contrast_profiles)
  n_sizes <- as.numeric(w_sizes)
  max_size <- max(n_sizes)

  pan_platos <- matrix(NA, ncol = length(w_sizes), nrow = max_size)

  for (i in seq_len(length(w_sizes))) {
    padded <- c(contrast_profiles[[i]]$platos[, position], rep(NA, max_size - n_sizes[i]))
    pan_platos[, i] <- padded
  }

  colnames(pan_platos) <- w_sizes
  return(pan_platos)
}

plot_pan_contrast <- function(pan_cp, plot_type = c("heatmap", "surface"), mode = c("all", "max")) {
  checkmate::qassert(pan_cp, "m")
  plot_type <- match.arg(plot_type)
  mode <- match.arg(mode)

  "!DEBUG Plot Pan CP"

  w_sizes <- colnames(pan_cp)

  steps <- list()

  min_val <- 0
  max_val <- round(max(pan_cp, na.rm = TRUE), 2)

  z <- t(pan_cp)

  fig <- NULL

  if (mode == "max") {
    xx <- apply(z, 1, function(x) {
      which.max(x)
    })
    mm <- as.numeric(apply(z, 1, function(x) {
      max(x, na.rm = TRUE)
    }))
    min_plato <- round(min(mm, na.rm = TRUE), 2)
    yy <- as.numeric(w_sizes)

    values <- seq(min_val, max_val, by = 0.01)
    for (i in seq_along(values)) {
      step <- list(
        label = values[i], method = "restyle", args = list("marker.cmin", values[i])
      )
      steps[[i]] <- step
    }

    fig <- plotly::plot_ly(
      x = xx,
      y = yy,
      text = round(mm, 4),
      type = "scatter",
      mode = "markers",
      hoverinfo = "x+y+text",
      marker = list(
        colorscale = "Blackbody",
        reversescale = TRUE,
        showscale = TRUE,
        size = 15,
        color = mm
      )
    ) %>% plotly::layout(
      xaxis = list(
        title = "Time",
        range = c(0, ncol(z))
      ),
      yaxis = list(
        title = "Shapelet Size"
      ),
      sliders = list(
        list(steps = steps, pad = list(t = 30), active = floor(min_plato / 0.01))
      ),
      updatemenus = list(
        list(
          type = "dropdown",
          buttons = list(
            list(
              method = "restyle",
              args = list("marker.colorscale", "Blackbody"),
              label = "Blackbody"
            ),
            list(
              method = "restyle",
              args = list("marker.colorscale", "Jet"),
              label = "Jet"
            ),
            list(
              method = "restyle",
              args = list("marker.colorscale", "Viridis"),
              label = "Viridis"
            ),
            list(
              method = "restyle",
              args = list("marker.colorscale", "Bluered"),
              label = "Bluered"
            ),
            list(
              method = "restyle",
              args = list("marker.colorscale", "Blues"),
              label = "Blues"
            ),
            list(
              method = "restyle",
              args = list("marker.colorscale", "Cividis"),
              label = "Cividis"
            ),
            list(
              method = "restyle",
              args = list("marker.colorscale", "Earth"),
              label = "Earth"
            ),
            list(
              method = "restyle",
              args = list("marker.colorscale", "Electric"),
              label = "Electric"
            ),
            list(
              method = "restyle",
              args = list("marker.colorscale", "Greens"),
              label = "Greens"
            ),
            list(
              method = "restyle",
              args = list("marker.colorscale", "Greys"),
              label = "Greys"
            ),
            list(
              method = "restyle",
              args = list("marker.colorscale", "Hot"),
              label = "Hot"
            ),
            list(
              method = "restyle",
              args = list("marker.colorscale", "Picnic"),
              label = "Picnic"
            ),
            list(
              method = "restyle",
              args = list("marker.colorscale", "Portland"),
              label = "Portland"
            ),
            list(
              method = "restyle",
              args = list("marker.colorscale", "Rainbow"),
              label = "Rainbow"
            ),
            list(
              method = "restyle",
              args = list("marker.colorscale", "RdBu"),
              label = "RdBu"
            ),
            list(
              method = "restyle",
              args = list("marker.colorscale", "Reds"),
              label = "Reds"
            ),
            list(
              method = "restyle",
              args = list("marker.colorscale", "YlGnBu"),
              label = "YlGnBu"
            ),
            list(
              method = "restyle",
              args = list("marker.colorscale", "YlOrRd"),
              label = "YlOrRd"
            )
          )
        ),
        list(
          type = "buttons",
          y = 0.5,
          active = 1,
          buttons = list(
            list(
              method = "restyle",
              args = list("marker.reversescale", FALSE),
              label = "Normal scale"
            ),
            list(
              method = "restyle",
              args = list("marker.reversescale", TRUE),
              label = "Reverse scale"
            )
          )
        )
      )
    )
  } else {
    if (plot_type == "heatmap") {
      values <- seq(min_val, max_val, by = 0.01)
      for (i in seq_along(values)) {
        step <- list(
          label = values[i], method = "restyle", args = list("zmin", values[i])
        )
        steps[[i]] <- step
      }

      fig <- plotly::plot_ly(
        y = w_sizes,
        colorscale = "Jet",
        type = "heatmap", ygap = 1, zsmooth = "best",
        z = ~z
      )
    } else {
      values <- seq(min_val, max_val, by = 0.01)
      for (i in seq_along(values)) {
        step <- list(
          label = values[i], method = "restyle", args = list("cmin", values[i])
        )
        steps[[i]] <- step
      }

      fig <- plotly::plot_ly(
        y = w_sizes,
        colorscale = "Jet",
        type = "surface",
        z = ~z
      ) %>% plotly::layout(scene = list(
        camera = list(
          eye = list(x = -0.5, y = -1, z = 0.5),
          projection = list(type = "orthographic")
        ),
        aspectratio = list(x = 1, y = 0.5, z = 0.5)
      ))
    }
    fig <- fig %>%
      plotly::layout(
        sliders = list(
          list(steps = steps, pad = list(t = 30))
        ),
        updatemenus = list(
          list(
            type = "dropdown",
            buttons = list(
              list(
                method = "restyle",
                args = list("colorscale", "Jet"),
                label = "Jet"
              ),
              list(
                method = "restyle",
                args = list("colorscale", "Viridis"),
                label = "Viridis"
              ),
              list(
                method = "restyle",
                args = list("colorscale", "Blackbody"),
                label = "Blackbody"
              ),
              list(
                method = "restyle",
                args = list("colorscale", "Bluered"),
                label = "Bluered"
              ),
              list(
                method = "restyle",
                args = list("colorscale", "Blues"),
                label = "Blues"
              ),
              list(
                method = "restyle",
                args = list("colorscale", "Cividis"),
                label = "Cividis"
              ),
              list(
                method = "restyle",
                args = list("colorscale", "Earth"),
                label = "Earth"
              ),
              list(
                method = "restyle",
                args = list("colorscale", "Electric"),
                label = "Electric"
              ),
              list(
                method = "restyle",
                args = list("colorscale", "Greens"),
                label = "Greens"
              ),
              list(
                method = "restyle",
                args = list("colorscale", "Greys"),
                label = "Greys"
              ),
              list(
                method = "restyle",
                args = list("colorscale", "Hot"),
                label = "Hot"
              ),
              list(
                method = "restyle",
                args = list("colorscale", "Picnic"),
                label = "Picnic"
              ),
              list(
                method = "restyle",
                args = list("colorscale", "Portland"),
                label = "Portland"
              ),
              list(
                method = "restyle",
                args = list("colorscale", "Rainbow"),
                label = "Rainbow"
              ),
              list(
                method = "restyle",
                args = list("colorscale", "RdBu"),
                label = "RdBu"
              ),
              list(
                method = "restyle",
                args = list("colorscale", "Reds"),
                label = "Reds"
              ),
              list(
                method = "restyle",
                args = list("colorscale", "YlGnBu"),
                label = "YlGnBu"
              ),
              list(
                method = "restyle",
                args = list("colorscale", "YlOrRd"),
                label = "YlOrRd"
              )
            )
          ),
          list(
            type = "buttons",
            y = 0.5,
            buttons = list(
              list(
                method = "restyle",
                args = list("reversescale", FALSE),
                label = "Normal scale"
              ),
              list(
                method = "restyle",
                args = list("reversescale", TRUE),
                label = "Reverse scale"
              )
            )
          )
        )
      )
  }


  fig
}

# Output:
#   plato: The subsequence that most distinguishes
#          positiveTS from negativeTS
#   plato_indices: The starting index of each of the K Platos
#   plato_primary_contrast: Contrast value of each plato in the K=1 CP
#   plato_nary_contrast: Contrast value of each plato after appending
#     the previous Plato to negativeTS. May be helpful in identifying
#     diminishing returns and retundant behaviors.
# TODO: check find_k_shapelets()
contrastprofile_topk <- function(split, shapelet_sizes, num_shapelets, progress = FALSE) {
  checkmate::qassert(split, "L4")
  checkmate::qassert(shapelet_sizes, "N+")

  "!DEBUG Compute Pan CP"

  idxs <- (split$data$alarm == "true")

  # time series containing at least two instances of a desired behavior
  true_alarms <- unlist(split$data$values[idxs])
  true_alarms <- as.numeric(true_alarms[!is.na(true_alarms)])

  # time series containing zero instances of a desired behavior
  false_alarms <- unlist(split$data$values[!idxs])
  false_alarms <- as.numeric(false_alarms[!is.na(false_alarms)])

  result <- list()

  for (i in seq_along(shapelet_sizes)) {
    true_alarms_val <- validate_data(true_alarms, shapelet_sizes[i] %/% 2)
    false_alarms_val <- validate_data(false_alarms, shapelet_sizes[i] %/% 2)

    self_mp <- mpx(
      data = true_alarms_val,
      window_size = shapelet_sizes[i],
      exclusion_zone = 0.5,
      distance = "euclidean",
      progress = progress,
      idxs = FALSE
    )$matrix_profile

    if (!all(is.finite(self_mp))) {
      cli::cli_warn("self_mp contains non finite values.")
    }

    # clip values above sqrt(2 * shapelet_sizes) as they are anti-correlated
    clip <- sqrt(2 * shapelet_sizes[i])
    self_mp[self_mp > clip] <- clip

    # pad to make future comparisons between matrix profiles
    pad <- length(true_alarms_val) - length(self_mp) + 1
    self_mp_padded <- c(self_mp, rep(NA, pad))

    platos <- matrix(NA, nrow = shapelet_sizes[i], ncol = num_shapelets)
    plato_indices <- rep(NA, num_shapelets)
    plato_primary_contrast <- rep(NA, num_shapelets)
    plato_nary_contrast <- rep(NA, num_shapelets)
    cps <- matrix(NA, nrow = length(self_mp_padded), ncol = num_shapelets)
    primary_cp <- NULL # rep(NA, length(self_mp_padded))

    join_mp_history <- rep(clip, length(self_mp_padded))
    past_plato <- false_alarms_val

    for (ki in seq_len(num_shapelets)) {
      cli::cli_h1("Computing plato {ki} of {num_shapelets}.")
      #  Matrix profile AB-join between true_alarms_val and false_alarms_val
      join_mp <- mpx(
        data = true_alarms_val,
        window_size = shapelet_sizes[i],
        query = past_plato,
        exclusion_zone = 0.5,
        distance = "euclidean",
        progress = progress,
        idxs = FALSE
      )$matrix_profile

      #  Euclidean distance values above sqrt(2*m) are equivalent to
      #    anti-correlated values
      if (!all(is.finite(join_mp))) {
        cli::cli_warn("join_mp contains non finite values.")
      }

      join_mp[join_mp > clip] <- clip

      #  pad with NA to make future comparisons between matrix profiles
      pad <- length(true_alarms_val) - length(join_mp) + 1
      join_mp <- c(join_mp, rep(NA, pad))
      na_idxs <- is.na(join_mp)

      self_mp_padded[na_idxs] <- NA
      join_mp_history <- pmin(join_mp_history, join_mp)
      #  Contrast Profile
      cp <- join_mp_history - self_mp_padded

      #  Normalize values to the range [0,1]
      cp <- cp / clip

      # discard negative values. These occur when a subsequence in T+ has a
      # closer nearest neighbor in T-. It's not a behavior of interest here
      cp <- pmax(0, cp)

      cps[, ki] <- cp

      if (ki == 1) {
        primary_cp <- cp
      }

      #  plato is the subsequence in true_alarms_val corresponding to index with
      #    largest contrast profile value
      plato_index <- which.max(cp)
      max_contrast_value <- cp[plato_index]
      plato <- true_alarms_val[plato_index:(plato_index + shapelet_sizes[i] - 1)]
      plato_indices[ki] <- plato_index
      # Reflects the contrast without appending previous platos to negativeTS
      plato_primary_contrast[ki] <- primary_cp[plato_index]
      # This may be helpful in identifying diminishing returns/redundant platos
      # if primary > nary, probably is a redundant plato
      plato_nary_contrast[ki] <- max_contrast_value
      platos[, ki] <- plato
      #  setup for next iteration
      exclusion_length <- shapelet_sizes[i]
      start_index <- max(1, (plato_index - exclusion_length))
      end_index <- min(length(true_alarms_val), ceiling(plato_index + shapelet_sizes[i] - 1 + exclusion_length))
      past_plato <- true_alarms_val[start_index:end_index]
    }

    result[[as.character(shapelet_sizes[i])]] <- list(
      cps = cps, platos = platos, plato_indices = plato_indices,
      plato_primary_contrast = plato_primary_contrast, plato_nary_contrast = plato_nary_contrast
    )
  }

  return(result)
}

kneed <- function(contrasts) {
  # contrasts <- c(0.31767218, 0.29146924, 0.17797986, 0.15451647, 0.09342605, 0.08841086, 0.08017338, 0.07088699, 0.07014500, 0.06976499)
  numk <- length(contrasts)
  first <- contrasts[1]
  last <- contrasts[numk]
  numer <- 1 / numk + contrasts[1]
  den <- 1 / numk + contrasts[1] + contrasts[numk]
  new_contrasts <- ((1 / numk + first) - contrasts) / (1 / numk + first - last)

  xlab <- seq_len(numk) / numk

  diag <- abs(xlab - new_contrasts) / sqrt(2)
  vert <- new_contrasts - xlab
  elb <- vert - diag

  return(elb)
}


pan_contrast <- function(data_pos_neg, signal = "II", shapelet_sizes) {
  checkmate::qassert(data_pos_neg, "L1")
  checkmate::qassert(signal, "S1")
  checkmate::qassert(shapelet_sizes, "N+")

  "!DEBUG Compute Pan CP"

  cli::cli_h1("Processing signal {signal}")

  # which classes are present in the dataset?
  classes <- unique(names(data_pos_neg[[signal]]))

  class_result <- list()

  # do the thing for each class
  for (cl in classes) {
    cli::cli_h2("Starting class {cl}")
    profiles <- list()
    for (i in seq_along(shapelet_sizes)) {
      cli::cli_h2("Window size {shapelet_sizes[i]}")

      checkmate::assert_true(data_pos_neg[[signal]][[cl]][[i]]$shapelet_size == shapelet_sizes[i])

      reference <- data_pos_neg[[signal]][[cl]][[i]]$neg_stream
      anomalous <- data_pos_neg[[signal]][[cl]][[i]]$pos_stream

      self_mp <- mpx(
        data = anomalous,
        window_size = shapelet_sizes[i],
        exclusion_zone = 0.5,
        distance = "euclidean",
        progress = FALSE,
        idxs = FALSE
      )$matrix_profile

      if (!all(is.finite(self_mp))) {
        cli::cli_warn("self_mp contains non finite values.")
      }

      # clip values above sqrt(2 * shapelet_sizes) as they are anti-correlated
      clip <- sqrt(2 * shapelet_sizes[i])
      self_mp[self_mp > clip] <- clip

      join_mp <- mpx(
        data = anomalous,
        window_size = shapelet_sizes[i],
        query = reference,
        exclusion_zone = 0.5,
        distance = "euclidean",
        progress = FALSE,
        idxs = FALSE
      )$matrix_profile

      if (!all(is.finite(join_mp))) {
        cli::cli_warn("join_mp contains non finite values.")
      }

      join_mp[join_mp > clip] <- clip

      contrast <- join_mp - self_mp # TruFalse - True

      # normalize between 0 and 1
      contrast <- contrast / clip
      contrast[contrast < 0] <- 0
      profiles[[i]] <- contrast
    }
    class_result[[cl]] <- profiles
  }

  result <- list()
  result[[signal]] <- class_result

  return(result)
}
