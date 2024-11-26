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
#' @param hlines numeric values indicating where horizontal lines will be
#'   plotted on the y-axis. This is intended to display the `min_distance`
#'   argument from `find_bouts()` so that users can see how bouts and peaks were
#'   separated.
#' @param time_min the minimum time to plot.
#' @param time_max the maximum time to plot.
#' @param ylim the y-limits of the plot. This can be useful to zoom closer to a
#'   horizontal line specified by `hlines`.
#' @param col_time_indices colors of each vector of indices in the
#'   `time_indices` argument.
#' @param col_hlines colors of each horizontal line in the `hlines` argument.
#' @param legend_default whether to display a default legend. This argument is
#'   `TRUE` by default and assumes that the output of `find_bouts()` has been
#'   passed into the `time_indices` argument. Otherwise, this argument should be
#'   set to `FALSE` to avoid the creation of an erroneous legend.
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
#' @importFrom graphics abline legend points segments
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
                             col_hlines = c("blue"),
                             legend_default = TRUE) {
  # Validate arguments
  validate_trajectory(trajectory)
  validate_diagnostic_time_indices(time_indices, col_time_indices)
  validate_time_limits(time_min, time_max)

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

  # Add a default legend if requested
  if (legend_default) {
    if (is.list(time_indices)) {
      graphics::legend("topright", legend = c("Bouts", "Peaks"),
                       col = col_time_indices,
                       lty = 0, lwd = 2, pch = 4, pt.cex = 1.25)
    } else if (is.numeric(time_indices)) {
      graphics::legend("topright", legend = "Bouts",
                       col = col_time_indices[1],
                       lty = 0, lwd = 2, pch = 4, pt.cex = 1.25)
    }
  }

  # Plot time indices of interest
  if (is.list(time_indices)) {
    col_time_indices <- rep(col_time_indices,
                            times = sapply(time_indices, length))
    time_indices <- unlist(time_indices, use.names = FALSE)
  } else if (is.numeric(time_indices)) {
    col_time_indices <- rep(col_time_indices[1], times = length(time_indices))
  }
  valid_time_indices <- time_indices >= time_min_index &
                        time_indices <= time_max_index
  time_indices <- time_indices[valid_time_indices]
  col_time_indices <- col_time_indices[valid_time_indices]
  graphics::points(trajectory$t[time_indices],
                   shifted_distances[time_indices],
                   cex = 1.25, col = col_time_indices, lwd = 2, pch = 4)

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
#' bouts <- find_bouts(trajectory, min_distance = 2, min_time = 0.6)
#' angles <- calculate_angles(trajectory, bouts = bouts)
#' plot_distribution(angles, "histogram",
#'                   main = "Distribution of Angles",
#'                   xlab = "Angle (degrees)",
#'                   ylab = "Density",
#'                   breaks = 36,
#'                   xlim = c(-180, 180))
#'
#' @export
#' @importFrom graphics box hist
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
    graphics::box(bty = "o")
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
#' @param col colors of each point in the plot. By default, this function uses
#'   the rainbow color palette, which contains the colors red, yellow, green,
#'   cyan, blue, and magenta in that order. Hues in between each consecutive
#'   pair of colors are also included. The position colored in red occurs at the
#'   earliest time, the position colored in yellow occurs at a time after the
#'   time of the position colored in red, the position colored in green occurs
#'   at a time after the time of the position colored in yellow, and so on, with
#'   the position colored in magenta occurring at the latest time.
#'
#' @param time_colorbar whether to display a colorbar showing the mapping
#'   between times and colors of points in the trajectory.
#' @param time_colorbar_lab a label for the colorbar.
#'
#' @examples
#' trajectory <- create_trajectory(cadmium_zebrafish,
#'                                 x = "X.center",
#'                                 y = "Y.center",
#'                                 t = "Trial.time")
#' plot_trajectory(trajectory,
#'                 xlab = "x-position (mm)", ylab = "y-position (mm)",
#'                 time_min = 1200, time_max = 1260,
#'                 time_colorbar_lab = "Time\n(s)")
#'
#' @export
#' @importFrom graphics image layout par segments
#' @importFrom grDevices rainbow
plot_trajectory <- function(trajectory,
                            main = "Trajectory",
                            xlab = "x-position",
                            ylab = "y-position",
                            time_min = min(trajectory$t),
                            time_max = max(trajectory$t),
                            col = NULL,
                            time_colorbar = TRUE,
                            time_colorbar_lab = "Time") {
  # Validate arguments
  validate_trajectory(trajectory)
  validate_time_limits(time_min, time_max)

  # Specify the plot arrangement if a colorbar for time will be added
  if (time_colorbar) {
    graphics::layout(matrix(c(1, 2), ncol = 2), widths = c(0.875, 0.125))
  }

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

  # Add a colorbar for time
  if (time_colorbar) {
    graphics::par(mar = c(5, 1, 4, 2) + 0.1)

    # Interpolate times and find the color for each interpolated time
    times <- trajectory$t[time_min_index:time_max_index]
    times_interpolated <- seq(trajectory$t[time_min_index],
                              trajectory$t[time_max_index],
                              length.out = 1000)
    time_differences <- matrix(rep(times_interpolated, times = length(times)),
                               nrow = length(times_interpolated),
                               ncol = length(times)) -
                        matrix(rep(times, times = length(times_interpolated)),
                               nrow = length(times_interpolated),
                               ncol = length(times),
                               byrow = TRUE)
    time_differences[which(time_differences < 0)] <- NA
    col_indices <- apply(time_differences, 1, which.min)

    # Plot the colorbar
    graphics::image(0, times_interpolated, matrix(col_indices, nrow = 1),
                    col = col, xlab = time_colorbar_lab, useRaster = TRUE,
                    xaxt = "n")
  }

  # Plot is finished
  return(invisible(NULL))
}
