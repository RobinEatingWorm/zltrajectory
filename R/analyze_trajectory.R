#' Calculate Trajectory Turn Angles
#'
#' @description This function calculates turn angles formed by every consecutive
#'   group of three observations within the trajectory data frame.
#'
#' @param trajectory a trajectory data frame. See `create_trajectory()` for more
#'   details.
#' @param bouts a vector of indices corresponding to beginnings of bouts. If
#'   this argument is provided, this function only uses observations at bout
#'   starts to calculate turn angles. Hence, turn angles are calculated using
#'   every consecutive group of three bouts. See `find_bouts()` for more
#'   details.
#'
#' @return a vector of turn angles. Each turn angle has a sign indicating its
#'   direction: left turns are positive while right turns are negative. In the
#'   edge case where a zebrafish somehow turns exactly 180 degrees, the sign is
#'   positive regardless of whether a left or right turn was performed.
#'
#' @examples
#' trajectory <- create_trajectory(cadmium_zebrafish,
#'                                 x = "X.center",
#'                                 y = "Y.center",
#'                                 t = "Trial.time")
#' bouts <- find_bouts(trajectory, min_distance = 2, min_time = 0.6)
#' angles <- calculate_angles(trajectory, bouts = bouts)
#'
#' @export
calculate_angles <- function(trajectory, bouts = NULL) {
  # Validate arguments
  validate_trajectory(trajectory, bouts = bouts)

  # Only use positions at bouts if specified
  if (!is.null(bouts)) {
    trajectory <- trajectory[bouts, ]
  }

  # Points on initial sides, vertices, and points on terminal sides of angles
  p1 <- as.matrix(trajectory[1:(nrow(trajectory) - 2), c("x", "y")])
  p2 <- as.matrix(trajectory[2:(nrow(trajectory) - 1), c("x", "y")])
  p3 <- as.matrix(trajectory[3:nrow(trajectory), c("x", "y")])

  # Calculate turn angles using supplementary angle formula
  angles <- unname(linear_algebra_angles(p2 - p1, p3 - p2))
  return(angles[!is.na(angles)])
}


#' Calculate Trajectory Bouts per a Specified Unit of Time
#'
#' @description This function groups bouts using a time unit and calculates the
#'   number of bouts within each unit.
#'
#' @param trajectory a trajectory data frame. See `create_trajectory()` for more
#'   details.
#' @param bouts a vector of indices corresponding to beginnings of bouts. See
#'   `find_bouts()` for more details.
#' @param unit the unit of time to group bouts by. This unit must be the same as
#'   the unit used for times within the trajectory data frame. For example, if
#'   trajectory times were recorded in seconds, setting this argument to 60
#'   would calculate the number of bouts per minute.
#'
#' @return a vector of bouts per time unit.
#'
#'   Note that the last element of this vector may be incomplete. For example,
#'   if time were recorded in seconds and the `unit` argument was set to 60,
#'   then the first element would indicate the number of bouts occurring at time
#'   greater than or equal to 0 seconds and less than 60 seconds, the second
#'   element would indicate the number of bouts occurring at time greater than
#'   or equal to 60 seconds and less than 120 seconds, and so on.
#'
#'   If only 90 seconds were recorded in the trajectory data frame, the return
#'   value would have two elements. The first element would indicate the number
#'   of bouts occurring at time greater than or equal to 0 seconds and less than
#'   60 seconds, and the second element would indicate the number of bouts
#'   occurring at time greater than or equal to 60 seconds and less than or
#'   equal to 90 seconds, despite how 60 seconds was specified as the time unit.
#'
#'   If exactly 60 seconds were recorded in the trajectory data frame, the
#'   return value would have two elements. The first element would indicate the
#'   number of bouts occurring at time greater than or equal to 0 seconds and
#'   less than 60 seconds, and the second element would indicate the number of
#'   bouts occurring at the exact 60-second mark (which logically should not be
#'   greater than 1).
#'
#' @examples
#' trajectory <- create_trajectory(cadmium_zebrafish,
#'                                 x = "X.center",
#'                                 y = "Y.center",
#'                                 t = "Trial.time")
#' bouts <- find_bouts(trajectory, min_distance = 2, min_time = 0.6)
#' bouts_per_minute <- calculate_bouts_per_time_unit(trajectory,
#'                                                   bouts,
#'                                                   unit = 60)
#'
#' @export
calculate_bouts_per_time_unit <- function(trajectory, bouts, unit = 1) {
  # Validate arguments
  validate_trajectory(trajectory, bouts = bouts)
  validate_arg(unit, "unit", "numeric")

  # Matrix specifying which indices (rows) belong to which time units (columns)
  units_bouts <- matrix(0,
                        nrow = nrow(trajectory),
                        ncol = (max(trajectory$t) %/% unit) + 1)
  offsets <- nrow(trajectory) * (trajectory$t[bouts] %/% unit)
  units_bouts[bouts + offsets] <- 1

  # Find the total number of bouts within each time unit
  return(colSums(units_bouts))
}


#' Calculate Trajectory Distances
#'
#' @description This function calculates distances traveled between every
#'   consecutive pair of observations within the trajectory data frame.
#'
#' @param trajectory a trajectory data frame. See `create_trajectory()` for more
#'   details.
#' @param bouts a vector of indices corresponding to beginnings of bouts. If
#'   this argument is provided, this function only uses observations at bout
#'   starts to calculate distances. Hence, distances traveled are calculated
#'   between every consecutive pair of bouts. See `find_bouts()` for more
#'   details.
#'
#' @return a vector of distances traveled.
#'
#' @examples
#' trajectory <- create_trajectory(cadmium_zebrafish,
#'                                 x = "X.center",
#'                                 y = "Y.center",
#'                                 t = "Trial.time")
#' bouts <- find_bouts(trajectory, min_distance = 2, min_time = 0.6)
#' distances <- calculate_distances(trajectory, bouts = bouts)
#'
#' @export
calculate_distances <- function(trajectory, bouts = NULL) {
  # Validate arguments
  validate_trajectory(trajectory, bouts = bouts)

  # Only use positions at bouts if specified
  if (!is.null(bouts)) {
    trajectory <- trajectory[bouts, ]
  }

  # Calculate distances between consecutive points
  p1 <- as.matrix(trajectory[1:(nrow(trajectory) - 1), c("x", "y")])
  p2 <- as.matrix(trajectory[2:nrow(trajectory), c("x", "y")])
  distances <- unname(linear_algebra_distances(p1, p2))
  return(distances)
}
