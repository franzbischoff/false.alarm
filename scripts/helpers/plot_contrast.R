plot_max_contrast <- function(contrast_profiles) {
  checkmate::qassert(contrast_profiles, "L")

  profile_size <- nrow(contrast_profiles[[1]]$cps)
  num_windows <- length(contrast_profiles)
  idxs <- vector("numeric", num_windows)
  values <- vector("numeric", num_windows)
  windows <- vector("numeric", num_windows)

  for (i in seq_len(num_windows)) {
    idxs[i] <- contrast_profiles[[i]]$plato_indices[1]
    values[i] <- contrast_profiles[[i]]$plato_primary_contrast[1]
    windows[i] <- nrow(contrast_profiles[[i]]$platos)
  }

  graphics::plot(
    x = idxs, y = windows, xlim = c(1, profile_size), type = "p",
    col = scales::alpha("red", values), pch = 19,
    xlab = "index", ylab = "Subsequence length", main = "Max Contrast by subsequence length"
  )
}

# plot the contrasts of the topk platos (window vs k)
plot_topk_contrasts <- function(contrast_profiles, type = c("contour", "heatmap"), colorscale = "Viridis") {
  checkmate::qassert(contrast_profiles, "L+")
  type <- match.arg(type)

  w_sizes <- names(contrast_profiles)
  k <- length(contrast_profiles[[1]]$plato_nary_contrast)

  pan_topk <- matrix(NA, ncol = k, nrow = length(w_sizes))

  for (i in seq_len(length(w_sizes))) {
    pan_topk[i, ] <- contrast_profiles[[i]]$plato_nary_contrast
  }

  rownames(pan_topk) <- w_sizes

  if (type == "heatmap") {
    return(plotly::plot_ly(
      y = w_sizes,
      z = pan_topk,
      type = "heatmap",
      colorscale = colorscale
    ))
  } else {
    plotly::plot_ly(
      y = w_sizes, type = "contour", z = pan_topk,
      contours = list(showlabels = TRUE),
      colorscale = colorscale
    )
  }
}



# solutions = result of find_solutions
plot_best_candidates <- function(solutions, fold = 1, n = 1, max_size = NULL) {
  checkmate::qassert(solutions, c("L+", "D"))

  if (!is.data.frame(solutions)) {
    solutions <- list_dfr(solutions)
  }

  if (fold > nrow(solutions)) {
    fold <- nrow(solutions)
    cli::cli_warn("Fold is greater than the number of folds. Using the last fold.")
  }

  if (n > nrow(solutions$data[[fold]])) {
    n <- nrow(solutions$data[[fold]])
    cli::cli_warn("N is greater than the number of solutions {n}. Using the last solution.")
  }

  coverage <- solutions$coverage[[fold]]
  num_segments <- length(solutions$data[[fold]]$cov_idxs[[n]])
  redundancy <- solutions$redundancy[[fold]]
  solutions <- solutions$data[[fold]]

  solutions <- solutions |> dplyr::arrange(as.numeric(window))

  max_len <- max(as.numeric(solutions$window))
  num_platos <- nrow(solutions)
  ks <- solutions$k

  platos <- matrix(NA, ncol = num_platos, nrow = max_len)

  for (i in seq_len(num_platos)) {
    pl <- solutions$plato[[i]]
    pl[is.infinite(pl)] <- NA
    platos[seq_along(pl), i] <- pl
  }


  colnames(platos) <- as.character(seq_len(num_platos))
  platos <- tibble::as_tibble(platos)

  platos <- platos |>
    dplyr::mutate(n = dplyr::row_number()) |>
    tidyr::pivot_longer(seq_len(num_platos))


  gg <- platos |> ggplot2::ggplot(ggplot2::aes(x = n, y = value)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~name,
      ncol = 1, labeller = ggplot2::as_labeller(
        function(x) {
          y <- as.numeric(x)
          lbl <- glue::glue("k = {ks[y]}")
          return(lbl)
        }
      ),
      strip.position = "left"
    )

  gg <- gg + ggplot2::theme_bw(base_family = "Roboto") +
    ggplot2::ggtitle(glue::glue("Fold {fold} - Coverage {coverage} of {num_segments} - Redundancy {redundancy}")) +
    ggplot2::xlab("length") +
    ggplot2::xlim(0, ifelse(is.null(max_size), 400, max_size)) +
    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    )

  return(invisible(gg))
}


# Ok, don't check the line numbers. This is a mess but it works.
# pan_cp is the result of get_topk_pan_contrast()
plot_pan_contrast <- function(pan_cp, plot_type = c("heatmap", "surface"), mode = c("all", "max", "max_idx")) {
  checkmate::qassert(pan_cp, "m")
  plot_type <- match.arg(plot_type)
  mode <- match.arg(mode)

  "!DEBUG Plot Pan CP"

  w_sizes <- colnames(pan_cp)
  c_sizes <- as.numeric(w_sizes)

  steps <- list()

  min_val <- 0
  max_val <- round(max(pan_cp, na.rm = TRUE), 2)

  z <- t(pan_cp)

  max_contrast <- round(max(z, na.rm = TRUE), 5)
  max_by_idx <- apply(z, 2, function(x) {
    x[is.na(x)] <- 0
    round(max(x, na.rm = TRUE), 5)
  })
  max_idx <- which(max_by_idx == max_contrast)
  profile <- z[, max_idx]
  max_window <- c_sizes[which.max(profile)]

  fig <- NULL

  if (mode == "max") {
    xx <- apply(z, 1, function(x) {
      which.max(x)
    })
    mm <- as.numeric(apply(z, 1, function(x) {
      max(x, na.rm = TRUE)
    }))
    min_plato <- round(min(mm, na.rm = TRUE), 2)

    values <- seq(min_val, max_val, by = 0.01)
    for (i in seq_along(values)) {
      step <- list(
        label = values[i], method = "restyle", args = list("marker.cmin", values[i])
      )
      steps[[i]] <- step
    }

    fig <- plotly::plot_ly(
      x = xx,
      y = c_sizes,
      text = round(mm, 4),
      type = "scatter",
      mode = "markers",
      hoverinfo = "x+y+text",
      marker = list(
        colorscale = "Blackbody",
        colorbar = list(title = "Contrast"),
        reversescale = TRUE,
        showscale = TRUE,
        size = 15,
        color = mm
      )
    ) |> plotly::layout(
      xaxis = list(
        title = "Time",
        range = c(0, ncol(z))
      ),
      yaxis = list(
        title = "Window Size"
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
  } else if (mode == "max_idx") {
    data <- tibble::tibble(x = c_sizes, y = profile)
    gg <- ggplot2::ggplot(data) +
      ggpattern::geom_area_pattern(ggplot2::aes(x = x, y = y), pattern = "gradient", pattern_fill = "blue", pattern_fill2 = "yellow") +
      ggplot2::annotate("segment", x = max_window, y = max_contrast, xend = max_window, yend = 0, color = "red") +
      ggplot2::annotate("text",
        x = max_window, y = 0, label = glue::glue("Window size {max_window} at index {max_idx}"),
        color = "white", angle = 90, vjust = -0.5, hjust = -0.2, size = 7
      ) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle("Best window size") +
      ggplot2::ylab("Contrast") +
      ggplot2::xlab("Window Size")
    return(gg)
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
        y = c_sizes,
        colorscale = "Jet",
        type = "heatmap", ygap = 1, zsmooth = "best",
        z = ~z,
        colorbar = list(title = "Contrast")
      ) |> plotly::layout(
        annotations = list(
          list(
            x = max_idx,
            y = max_window,
            xshift = 2,
            yshift = -2,
            arrowcolor = "#000000",
            arrowsize = 3,
            standoff = 10,
            arrowhead = 6,
            text = "<b>Max</b>",
            font = list(color = "#000000", size = 14),
            showarrow = TRUE
          ),
          list(
            x = max_idx,
            y = max_window,
            arrowcolor = "#E5E5E5",
            arrowsize = 3,
            standoff = 10,
            arrowhead = 6,
            text = "<b>Max</b>",
            font = list(color = "#E5E5E5", size = 14),
            showarrow = TRUE
          )
        ),
        xaxis = list(
          title = "Time"
        ),
        yaxis = list(
          title = "Window Size"
        )
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
        y = c_sizes,
        colorscale = "Jet",
        type = "surface",
        z = ~z
      ) |> plotly::layout(
        scene = list(
          annotations = list(
            list(
              x = max_idx,
              y = max_window,
              z = max_contrast,
              xshift = 2,
              yshift = -2,
              arrowcolor = "#000000",
              arrowsize = 3,
              standoff = 10,
              arrowhead = 6,
              text = "<b>Max</b>",
              font = list(color = "#000000", size = 14),
              showarrow = TRUE
            ),
            list(
              x = max_idx,
              y = max_window,
              z = max_contrast,
              arrowcolor = "#E5E5E5",
              arrowsize = 3,
              standoff = 10,
              arrowhead = 6,
              text = "<b>Max</b>",
              font = list(color = "#E5E5E5", size = 14),
              showarrow = TRUE
            )
          ),
          xaxis = list(
            title = "Time"
          ),
          yaxis = list(
            title = "Window Size"
          ),
          zaxis = list(title = "Contrast"),
          camera = list(
            eye = list(x = -0.5, y = -1, z = 0.5),
            projection = list(type = "orthographic")
          ),
          aspectratio = list(x = 1, y = 0.5, z = 0.5)
        )
      )
    }
    fig <- fig |>
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
