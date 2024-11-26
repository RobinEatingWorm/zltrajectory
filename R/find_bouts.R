#' Find Bout Indices in a Trajectory Data Frame
#'
#' @description This function calculates row indices in the trajectory data
#'   frame that correspond to the beginnings of swim bouts. During a swim bout,
#'   a zebrafish larva travels with a sudden burst of motion, rather than
#'   continuous movement.
#'
#'   The algorithm for calculating swim bouts is as follows. First, peaks (local
#'   maxima) in the time series of distances traveled between consecutive time
#'   points in the trajectory data frame are found. Next, potential row indices
#'   corresponding to the beginnings of swim bouts are identified. Finally, each
#'   bout index is paired with a peak index occurring afterwards. If any bout or
#'   peak indices remain, they are discarded.
#'
#' @param trajectory a trajectory data frame. See `create_trajectory()` for more
#'   details.
#' @param min_distance the minimum distance the zebrafish must travel between
#'   two consecutive points for a peak to be detected. Ideally, this should
#'   clearly separate the burst motion of a zebrafish (where it travels greater
#'   distances between consecutive time points) from when it is not actively
#'   moving.
#' @param min_time the minimum time required between peaks. This must have the
#'   same units as the times within the trajectory data frame.
#' @param return_peaks whether or not to also return peak indices.
#'
#' @return a vector of indices corresponding to the beginnings of swim bouts. If
#'   return_peaks is `TRUE`, this function returns a list with the names
#'   `"bouts"`, containing swim bout indices, and `"peaks"`, containing indices
#'   of the corresponding peaks used to identify swim bouts.
#'
#' @examples
#' trajectory <- create_trajectory(cadmium_zebrafish,
#'                                 x = "X.center",
#'                                 y = "Y.center",
#'                                 t = "Trial.time")
#' bouts_and_peaks_list <- find_bouts(trajectory,
#'                                    min_distance = 2,
#'                                    min_time = 0.6,
#'                                    return_peaks = TRUE)
#' bouts <- bouts_and_peaks_list$bouts
#' peaks <- bouts_and_peaks_list$peaks
#'
#' @export
#' @importFrom pracma findpeaks
find_bouts <- function(trajectory,
                       min_distance,
                       min_time,
                       return_peaks = FALSE) {
  # Validate arguments
  validate_trajectory(trajectory)
  validate_arg(min_distance, "min_distance", "numeric")
  validate_arg(min_time, "min_time", "numeric")
  validate_arg(return_peaks, "return_peaks", "logical")

  # Calculate distances between consecutive points
  p1 <- as.matrix(trajectory[1:(nrow(trajectory) - 1), c("x", "y")])
  p2 <- as.matrix(trajectory[2:nrow(trajectory), c("x", "y")])
  distances <- unname(linear_algebra_distances(p1, p2))

  # Convert trajectory times and minimum time threshold to positive integers
  scale_times_to_integers_list <- scale_times_to_integers(trajectory, min_time)
  integer_times <- scale_times_to_integers_list$integer_times
  min_time <- scale_times_to_integers_list$min_time

  # Remove the first time so that each time corresponds to a traveled distance
  integer_times <- integer_times[2:length(integer_times)]

  # Create mappings using integer times
  map_times_distances_list <- map_times_distances(distances, integer_times)
  expanded_distances <- map_times_distances_list$expanded_distances
  integer_times_map <- map_times_distances_list$integer_times_map

  # Find peak and trough indices
  peaks <- pracma::findpeaks(expanded_distances,
                             minpeakheight = min_distance,
                             minpeakdistance = min_time)[, 2]
  peaks <- integer_times_map[peaks]
  troughs <- which(distances[1:(length(distances) - 1)] < min_distance &
                   distances[2:length(distances)] >= min_distance)

  # Find bouts and their corresponding peaks
  bouts_and_peaks_list <- remove_repeated_extrema(distances, peaks, troughs)
  if (return_peaks) {
    return(bouts_and_peaks_list)
  }
  return(bouts_and_peaks_list$bouts)
}


#' Create Mappings Between Times and Distances
#'
#' @description With times as integers, this function creates mappings using
#'   integer times as indices (keys).
#'
#' @param distances distances traveled between consecutive points. These should
#'   have been calculated with `linear_algebra_distances()`.
#' @param integer_times times as integers. These should have been calculated
#'   with `scale_times_to_integers()`
#'
#' @return a list of two vectors that use integer times as indices (keys).
#'   `expanded_distances` maps integer times to distances. `integer_times_map`
#'   maps integer times to the original distance indices.
map_times_distances <- function(distances, integer_times) {
  # Use integer times as indices for distances
  expanded_distances <- rep(0, max(integer_times, length(distances)))
  expanded_distances[integer_times] <- distances

  # Mapping from integer times to original distance indices
  integer_times_map <- rep(NA, max(integer_times, length(distances)))
  integer_times_map[integer_times] <- seq_along(distances)

  # Return distances and mapping
  return(list(expanded_distances = expanded_distances,
              integer_times_map = integer_times_map))
}


#' Find the Maximum Amount of Decimal Places
#'
#' @description Given a numeric vector, find the maximum amount of decimal
#'   places among all elements.
#'
#' @param numbers a vector of numbers.
#'
#' @return the maximum amount of decimal places found in any number.
max_decimal_places <- function(numbers) {
  # Separate integer and decimal parts of each number
  parts <- strsplit(as.character(numbers), "\\.")
  parts <- sapply(parts, function(char) {
    if (length(char) == 1) {
      char <- c(char, "")
    }
    return(char)
  })

  # Find the maximum number of decimal places among all number
  return(max(nchar(parts[2, ])))
}


#' Extract Bouts and Corresponding Peaks
#'
#' @description This function uses a vector of peak indices and a vector of
#'   trough indices (indices that potentially correspond to the beginnings of
#'   bouts) and performs two actions. First, it removes peaks that occur after
#'   other peaks without a trough in between. Second, it removes troughs that
#'   occur before other troughs without a peak in between. The remaining indices
#'   are those indicating beginnings of bouts and the peaks used to identify
#'   them.
#'
#' @param distances distances traveled between consecutive points. These should
#'   have been calculated with `linear_algebra_distances()`.
#' @param peaks indices of peaks.
#' @param troughs indices of troughs (potential beginnings of bouts)
#'
#' @return a list containing any remaining troughs and peaks after repeats are
#'   removed. Troughs are stored under the name `bouts` since they indicate when
#'   bouts begin. Peaks are stored under the name `peaks`.
remove_repeated_extrema <- function(distances, peaks, troughs) {
  # Combine peak and trough indices into a vector of extrema
  status <- rep(NA, length(distances))
  status[troughs] <- 0
  status[peaks] <- 1
  extrema <- which(!is.na(status))

  # Remove trailing peaks
  repeat_trailing_extrema <- which(diff(status[extrema]) == 0) + 1
  status[intersect(which(status == 1), extrema[repeat_trailing_extrema])] <- NA

  # Remove leading troughs
  repeat_leading_extrema <- which(diff(status[extrema]) == 0)
  status[intersect(which(status == 0), extrema[repeat_leading_extrema])] <- NA

  # Return bouts and remaining peaks
  bouts <- which(status == 0) + 1
  peaks <- which(status == 1) + 1
  return(list(bouts = bouts, peaks = peaks))
}


#' Convert Times to Positive Integers
#'
#' @description This function is used within `find_bouts()` to convert
#'   trajectory times and the `min_time` argument to positive integers by
#'   multiplying them with the smallest power of 10 that removes all decimal
#'   places in each time. If 0 is included in the trajectory times, each
#'   trajectory time is additionally increased by 1. Ultimately, by scaling
#'   trajectory times to positive integers, they can be used as vector indices.
#'
#' @param trajectory a trajectory data frame.
#' @param min_time the `min_time` argument from `find_bouts()`.
#'
#' @return a list containing trajectory times with the name `integer_times` and
#'   `min_time`, both modified to be positive integers.
scale_times_to_integers <- function(trajectory, min_time) {
  # Get the maximum number of decimal places in any specified time
  decimal_places <- max_decimal_places(c(trajectory$t, min_time))

  # Scale up all times to positive integers
  integer_times <- trajectory$t * (10 ^ decimal_places)
  if (min(integer_times) == 0) {
    integer_times <- integer_times + 1
  }
  min_time <- min_time * (10 ^ decimal_places)
  return(list(integer_times = integer_times, min_time = min_time))
}
