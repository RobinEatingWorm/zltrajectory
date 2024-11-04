#' Plot Distance Time Series with Diagnostic Features
#'
#' @description This function produces a plot of distances traveled between
#'   consecutive time points that is intended to aid in determining reasonable
#'   values for the `min_distance` and `min_time` parameters in `find_bouts()`.
#'
#' @details The `time_indices` and `hlines` arguments are intended to be used to
#'   display the output of `find_bouts()` and the value used for the
#'   `min_distance` argument of `find_bouts()`.
#'
#'   If too few peaks are found because the horizontal line shown by `hlines` is
#'   too high and appears to be over visible peaks, consider decreasing
#'   `min_distance`. Conversely, if too many peaks are found because the
#'   horizontal line is too low and captures peaks when the zebrafish is not
#'   moving significantly, consider increasing `min_distance`.
#'
#'   If several peaks are not detected because peaks appear to be too spread
#'   out, consider decreasing `min_time`. Conversely, if too many peaks are
#'   detected even if not all of the peaks seem to correspond to a full swim
#'   bout, consider increasing `min_time`.
#'
#' @param trajectory a trajectory data frame. See `create_trajectory()` for more
#'   details.
#' @param main a main title for the plot.
#' @param xlab a label for the x-axis.
#' @param ylab a label for the y-axis.
#' @param time_indices row indices within the trajectory data frame to mark on
#'   the plot. This argument takes either a vector or list of vectors as input.
#'   The output of `find_bouts()` can be passed into this argument, which will
#'   display bouts in green and peaks in red by default.
#' @param hlines y-values of horizontal lines to be added to the plot. This is
#'   intended to display the `min_distance` argument from `find_bouts()` so that
#'   users can see how bouts and peaks were separated.
#' @param time_min the minimum time to plot.
#' @param time_max the maximum time to plot.
#' @param ylim the y-limits of the plot. This can be useful to zoom closer to a
#'   horizontal line specified by `hlines`.
#' @param col_time_indices colors of each vector of indices in the
#'   `time_indices` argument.
#' @param col_hlines colors of each horizontal line in the `hlines` argument.
#'
#' @examples
#' trajectory <- create_trajectory(cadmium_zebrafish,
#'                                 x = "X.center",
#'                                 y = "Y.center",
#'                                 t = "Trial.time")
#' plot_diagnostics(trajectory,
#'                  xlab = "Time (s)", ylab = "Distance (mm)",
#'                  hlines = 2, time_min = 1240, time_max = 1260)
#' bouts_and_peaks_list <- find_bouts(trajectory,
#'                                    min_distance = 2,
#'                                    min_time = 0.6,
#'                                    return_peaks = TRUE)
#' plot_diagnostics(trajectory,
#'                  xlab = "Time (s)", ylab = "Distance (mm)",
#'                  time_indices = bouts_and_peaks_list, hlines = 2,
#'                  time_min = 1240, time_max = 1260)
#'
#' @export
#' @importFrom graphics abline points segments
plot_diagnostics <- function(trajectory,
                             main = "Distance Traveled Since Previous Time",
                             xlab = "Time",
                             ylab = "Distance",
                             time_indices = NULL,
                             hlines = NULL,
                             time_min = min(trajectory$t),
                             time_max = max(trajectory$t),
                             ylim = NULL,
                             col_time_indices = c("green", "red"),
                             col_hlines = c("blue")) {
  # Validate arguments
  validate_trajectory(trajectory)
  if (!is.null(time_indices)) {
    if (is.numeric(time_indices)) {
      validate_arg(time_indices, "time_indices", "numeric", length_one = FALSE)
    } else {
      for (i in seq_len(length(time_indices))) {
        validate_arg(time_indices[[i]],
                     paste0("time_indices[[", i, "]]"),
                     "numeric",
                     length_one = FALSE)
      }
    }
  }
  validate_arg(time_min, "time_min", "numeric", positive = FALSE)
  validate_arg(time_max, "time_max", "numeric", positive = FALSE)

  # Calculate distances traveled between consecutive points
  p1 <- as.matrix(trajectory[1:(nrow(trajectory) - 1), c("x", "y")])
  p2 <- as.matrix(trajectory[2:nrow(trajectory), c("x", "y")])
  distances <- unname(linear_algebra_distances(p1, p2))

  # Shift up by 1 index to show distances from previous index to current index
  shifted_distances <- c(NA, distances)

  # Find indices of minimum and maximum times to plot
  time_min_index <- min(which(trajectory$t >= time_min))
  time_min_index <- ifelse(time_min_index == 1, 2, time_min_index)
  time_max_index <- max(which(trajectory$t <= time_max))

  # Plot distances traveled from previous index to current index vs time
  plot(trajectory$t[time_min_index:time_max_index],
       shifted_distances[time_min_index:time_max_index],
       main = main, ylim = ylim, xlab = xlab, ylab = ylab, cex = 0.5, pch = 16)
  graphics::segments(trajectory$t[time_min_index:(time_max_index - 1)],
                     shifted_distances[time_min_index:(time_max_index - 1)],
                     trajectory$t[(time_min_index + 1):time_max_index],
                     shifted_distances[(time_min_index + 1):time_max_index])

  # Plot time indices of interest
  if (is.list(time_indices)) {
    for (i in seq_len(length(time_indices))) {
      valid_time_indices <- time_indices[[i]] >= time_min_index &
                            time_indices[[i]] <= time_max_index
      time_indices[[i]] <- time_indices[[i]][valid_time_indices]
      graphics::points(trajectory$t[time_indices[[i]]],
                       shifted_distances[time_indices[[i]]],
                       cex = 1.25, col = col_time_indices[i], pch = 4)
    }
  } else if (is.numeric(time_indices)) {
    valid_time_indices <- time_indices >= time_min_index &
                          time_indices <= time_max_index
    time_indices <- time_indices[valid_time_indices]
    graphics::points(trajectory$t[time_indices],
                     shifted_distances[time_indices],
                     cex = 1.25, col = col_time_indices[1], pch = 4)
  }


  # Add horizontal lines of interest
  if (!is.null(hlines)) {
    graphics::abline(h = hlines, col = col_hlines, lty = "dotted")
  }

  # Plot is finished
  return(invisible(NULL))
}


#' Plot the Distribution of a Vector
#'
#' @description This is a simple function for plotting the distribution of a
#'   vector. Outputs from `calculate_angles()`,
#'   `calculate_bouts_per_time_unit()`, and `calculate_distances()` can be
#'   visualized with this function, although users may need to create their own
#'   plots if more advanced capabilities are required.
#'
#' @param observations a vector of numeric observations.
#' @param type the type of plot. Supported types are `"histogram"`, `"kde"`, and
#'   `"ecdf"`.
#' @param main a main title for the plot.
#' @param xlab a label for the x-axis.
#' @param ylab a label for the y-axis.
#' @param breaks breaks in a histogram. If the plot is not a histogram, this
#'   argument does nothing. See the `breaks` argument in `graphics::hist()` for
#'   more details.
#' @param xlim the x-limits of the plot. This may be useful for histograms.
#'
#' @examples
#' trajectory <- create_trajectory(cadmium_zebrafish,
#'                                 x = "X.center",
#'                                 y = "Y.center",
#'                                 t = "Trial.time")
#' angles <- calculate_angles(trajectory)
#' plot_distribution(angles, "histogram",
#'                   main = "Distribution of Angles",
#'                   xlab = "Angle (degrees)",
#'                   ylab = "Density",
#'                   breaks = 36,
#'                   xlim = c(-180, 180))
#'
#' @export
#' @importFrom graphics hist
#' @importFrom stats density ecdf
plot_distribution <- function(observations,
                              type,
                              main = NULL,
                              xlab = NULL,
                              ylab = NULL,
                              breaks = "Sturges",
                              xlim = range(observations)) {
  # Validate arguments
  validate_arg(observations,
               "observations",
               "numeric",
               length_one = FALSE,
               positive = FALSE)

  # Create plot of distribution specified by type
  if (type == "histogram") {
    graphics::hist(observations, breaks = breaks, freq = FALSE,
                   main = main, xlab = xlab, ylab = ylab, xlim = xlim)
  } else if (type == "kde") {
    plot(stats::density(observations),
         main = main, xlab = xlab, ylab = ylab, xlim = xlim)
  } else if (type == "ecdf") {
    plot(stats::ecdf(observations),
         main = main, xlab = xlab, ylab = ylab, xlim = xlim)
  } else {
    stop("invalid type of plot")
  }

  # Plot is finished
  return(invisible(NULL))
}


#' Plot the Raw Trajectory
#'
#' @description This function plots the x- and y-positions of a fish within a
#'   specified time range.
#'
#' @param trajectory a trajectory data frame. See `create_trajectory()` for more
#'   details.
#' @param main a main title for the plot.
#' @param xlab a label for the x-axis.
#' @param ylab a label for the y-axis.
#' @param time_min the minimum time to plot.
#' @param time_max the maximum time to plot.
#' @param col colors of each point in the plot. By default the rainbow color
#'   palette is used, with red indicating the position at the earliest time and
#'   magenta indicating the position at the latest time.
#'
#' @examples
#' trajectory <- create_trajectory(cadmium_zebrafish,
#'                                 x = "X.center",
#'                                 y = "Y.center",
#'                                 t = "Trial.time")
#' plot_trajectory(trajectory,
#'                 xlab = "x-position (mm)", ylab = "y-position (mm)",
#'                 time_min = 1200, time_max = 1260)
#'
#' @export
#' @importFrom graphics segments
#' @importFrom grDevices rainbow
plot_trajectory <- function(trajectory,
                            main = "Trajectory",
                            xlab = "x-position",
                            ylab = "y-position",
                            time_min = min(trajectory$t),
                            time_max = max(trajectory$t),
                            col = NULL) {
  # Validate arguments
  validate_trajectory(trajectory)
  validate_arg(time_min, "time_min", "numeric", positive = FALSE)
  validate_arg(time_max, "time_max", "numeric", positive = FALSE)

  # Find indices of minimum and maximum times to plot
  time_min_index <- min(which(trajectory$t >= time_min))
  time_max_index <- max(which(trajectory$t <= time_max))

  # Set default color
  if (is.null(col)) {
    col <- grDevices::rainbow(time_max_index - time_min_index + 1, end = 5 / 6)
  }

  # Plot trajectory
  plot(trajectory$x[time_min_index:time_max_index],
       trajectory$y[time_min_index:time_max_index],
       main = main, xlab = xlab, ylab = ylab, cex = 0.5, col = col, pch = 16)
  graphics::segments(trajectory$x[time_min_index:(time_max_index - 1)],
                     trajectory$y[time_min_index:(time_max_index - 1)],
                     trajectory$x[(time_min_index + 1):time_max_index],
                     trajectory$y[(time_min_index + 1):time_max_index],
                     col = col)

  # Plot is finished
  return(invisible(NULL))
}
