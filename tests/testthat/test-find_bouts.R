test_that("find max decimal places", {
  # List of numbers
  numbers <- c(5, 2.5, -3.753, 1.5802, -256.4, 510.481, -0.18543627, 163.74923)

  # Check that the maximum number of decimal places is correct
  decimal_places <- max_decimal_places(numbers)
  expect_equal(decimal_places, 8)
})

test_that("max decimal places with no decimals", {
  # List of numbers
  numbers <- c(5, 25, -3753, 15802, -2564, 510481, -18543627, 16374923)

  # Check that the maximum number of decimal places is correct
  decimal_places <- max_decimal_places(numbers)
  expect_equal(decimal_places, 0)
})

test_that("integer times without 0", {
  # Trajectory and minimum time threshold
  trajectory <- data.frame(x = rep(0, times = 8),
                           y = rep(0, times = 8),
                           t = c(0.02, 0.21, 0.253, 0.5, 1.7, 2, 36, 75.8))
  min_time <- 1

  # Check that times are scaled to positive integers
  scale_times_to_integers_list <- scale_times_to_integers(trajectory, min_time)
  integer_times <- scale_times_to_integers_list$integer_times
  min_time <- scale_times_to_integers_list$min_time
  expect_equal(integer_times, c(20, 210, 253, 500, 1700, 2000, 36000, 75800))
  expect_equal(min_time, 1000)
})

test_that("integer times with 0", {
  # Trajectory and minimum time threshold
  trajectory <- data.frame(x = rep(0, times = 8),
                           y = rep(0, times = 8),
                           t = c(0, 0.21, 0.253, 0.5, 1.7, 2, 36, 75.8))
  min_time <- 1

  # Check that times are scaled to positive integers
  scale_times_to_integers_list <- scale_times_to_integers(trajectory, min_time)
  integer_times <- scale_times_to_integers_list$integer_times
  min_time <- scale_times_to_integers_list$min_time
  expect_equal(integer_times, c(1, 211, 254, 501, 1701, 2001, 36001, 75801))
  expect_equal(min_time, 1000)
})

test_that("integer time and distance mappings", {
  # Distances and integer times
  distances <- c(0.2, 4, 5.67, 0.01)
  integer_times <- c(1, 2, 4, 7)

  # Check that distances and integer times are mapped correctly
  map_times_distances_list <- map_times_distances(distances, integer_times)
  expanded_distances <- map_times_distances_list$expanded_distances
  integer_times_map <- map_times_distances_list$integer_times_map
  expect_equal(expanded_distances, c(0.2, 4, 0, 5.67, 0, 0, 0.01))
  expect_equal(integer_times_map, c(1, 2, NA, 3, NA, NA, 4))
})

test_that("remove repeated extrema", {
  # Distances, peaks, and troughs
  distances <- 1:20
  peaks <- c(6, 9, 13, 14, 16, 19)
  troughs <- c(1, 5, 8, 17, 18)

  # Check that bouts and filtered peaks are correct
  bouts_and_peaks_list <- remove_repeated_extrema(distances, peaks, troughs)
  bouts <- bouts_and_peaks_list$bouts
  peaks <- bouts_and_peaks_list$peaks
  expect_equal(bouts, c(6, 9, 19))
  expect_equal(peaks, c(7, 10, 20))
})

test_that("bouts and peaks found are valid", {
  # Trajectory using cadmium zebrafish data (multiple warnings due to NAs)
  expect_warning(expect_warning(
    trajectory <- create_trajectory(cadmium_zebrafish,
                                    x = "X.center",
                                    y = "Y.center",
                                    t = "Trial.time")
  ))

  # Trajectory distances shifted up one index
  p1 <- as.matrix(trajectory[1:(nrow(trajectory) - 1), c("x", "y")])
  p2 <- as.matrix(trajectory[2:nrow(trajectory), c("x", "y")])
  distances <- unname(linear_algebra_distances(p1, p2))
  shifted_distances <- c(NA, distances)

  # Check that bouts and peaks found make sense
  bouts_and_peaks_list <- find_bouts(trajectory,
                                     min_distance = 2,
                                     min_time = 0.6,
                                     return_peaks = TRUE)
  bouts <- bouts_and_peaks_list$bouts
  peaks <- bouts_and_peaks_list$peaks
  expect_true(all(shifted_distances[bouts] < 2))
  expect_true(all(shifted_distances[peaks] >= 2))
  expect_equal(length(bouts), length(peaks))
})
