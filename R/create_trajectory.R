#' Create a Trajectory Data Frame
#'
#' @description This function produces a valid data frame able to be used by
#'   other plotting and analysis functions in this package that contains the
#'   trajectory of a zebrafish. The trajectory data frame has three columns:
#'   x-position, y-position, and time.
#'
#' @param data a data frame containing the x- and y-positions of a fish.
#' @param x the name of the column containing the x-positions of the fish.
#' @param y the name of the column containing the y-positions of the fish.
#' @param t if a time column is present in the data, this parameter is the name
#'   of the column containing the times of all observations.
#' @param rate an alternative way of specifying time. This parameter generates a
#'   sequence of numbers starting from 0 and changing by `rate` for each
#'   observation.
#'
#' @return a data frame containing the trajectory of a zebrafish that can be
#'   used by analysis functions within this package.
#'
#' @examples
#' trajectory <- create_trajectory(cadmium_zebrafish,
#'                                 x = "X.center",
#'                                 y = "Y.center",
#'                                 t = "Trial.time")
#'
#' @export
create_trajectory <- function(data, x, y, t = NULL, rate = NULL) {
  # Validate arguments
  validate_data(data, x, y, t = t, rate = rate)

  # Create time vector
  if (!is.null(t)) {
    t <- as.numeric(data[[t]])
  } else {
    t <- seq(0, rate * (nrow(data) - 1), by = rate)
  }

  # Create trajectory data frame and remove rows with missing values
  trajectory <- data.frame(x = as.numeric(data[[x]]),
                           y = as.numeric(data[[y]]),
                           t = t)
  trajectory <- trajectory[-which(apply(is.na(trajectory), 1, any)), ]
  row.names(trajectory) <- NULL
  return(trajectory)
}
