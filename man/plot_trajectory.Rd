% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot_trajectory}
\alias{plot_trajectory}
\title{Plot the Raw Trajectory}
\usage{
plot_trajectory(
  trajectory,
  main = "Trajectory",
  xlab = "x-position",
  ylab = "y-position",
  time_min = min(trajectory$t),
  time_max = max(trajectory$t),
  col = NULL,
  time_colorbar = TRUE,
  time_colorbar_lab = "Time"
)
}
\arguments{
\item{trajectory}{a trajectory data frame. See `create_trajectory()` for more
details.}

\item{main}{a main title for the plot.}

\item{xlab}{a label for the x-axis.}

\item{ylab}{a label for the y-axis.}

\item{time_min}{the minimum time to plot.}

\item{time_max}{the maximum time to plot.}

\item{col}{colors of each point in the plot. By default, this function uses
the rainbow color palette, which contains the colors red, yellow, green,
cyan, blue, and magenta in that order. Hues in between each consecutive
pair of colors are also included. The position colored in red occurs at the
earliest time, the position colored in yellow occurs at a time after the
time of the position colored in red, the position colored in green occurs
at a time after the time of the position colored in yellow, and so on, with
the position colored in magenta occurring at the latest time.}

\item{time_colorbar}{whether to display a colorbar showing the mapping
between times and colors of points in the trajectory.}

\item{time_colorbar_lab}{a label for the colorbar.}
}
\description{
This function plots the x- and y-positions of a fish within a
  specified time range.
}
\examples{
trajectory <- create_trajectory(cadmium_zebrafish,
                                x = "X.center",
                                y = "Y.center",
                                t = "Trial.time")
plot_trajectory(trajectory,
                xlab = "x-position (mm)", ylab = "y-position (mm)",
                time_min = 1200, time_max = 1260,
                time_colorbar_lab = "Time\n(s)")

}
