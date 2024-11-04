test_that("angle calculations without bouts", {
  # Create trajectory
  trajectory <- data.frame(x = c(0, 0, 1, 2, 0, -1, 0),
                           y = c(0, 1, 1, 2, 2, 0, 2),
                           t = 1:7)

  # Find angles of trajectory
  angles <- calculate_angles(trajectory)
  expect_equal(angles, c(-90, 45, 135, atan(2) * (180 / pi), 180))
})

test_that("angle calculations with bouts", {
  # Create trajectory and bouts
  trajectory <- data.frame(x = c(0, 0, 1, 2, 0, -1, 0),
                           y = c(0, 1, 1, 2, 2, 0, 2),
                           t = 1:7)
  bouts <- c(2, 4, 6, 7)

  # Find angles of trajectory
  angles <- calculate_angles(trajectory, bouts = bouts)
  expect_equal(angles, c((acos(8 / sqrt(65)) * (180 / pi)) - 180,
                         (acos(7 / sqrt(65)) * (180 / pi)) - 180))
})

test_that("bouts per time unit", {
  # Create trajectory and bouts
  trajectory <- data.frame(x = rep(0, times = 1000),
                           y = rep(0, times = 1000),
                           t = 0:999)
  bouts <- c(1, 2, 100, 101, 156, 199, 200, 300, 301, 799, 800, 801, 1000)

  # Find bouts per time unit
  bptu <- calculate_bouts_per_time_unit(trajectory, bouts, unit = 100)
  expect_equal(bptu, c(3, 4, 1, 1, 0, 0, 0, 2, 1, 1))
})

test_that("bouts per time unit with incomplete last unit", {
  # Create trajectory and bouts
  trajectory <- data.frame(x = rep(0, times = 1003),
                           y = rep(0, times = 1003),
                           t = 0:1002)
  bouts <- c(1, 2, 100, 101, 156, 199, 200, 300, 301, 799, 800, 801, 1000, 1001)

  # Find bouts per time unit
  bptu <- calculate_bouts_per_time_unit(trajectory, bouts, unit = 100)
  expect_equal(bptu, c(3, 4, 1, 1, 0, 0, 0, 2, 1, 1, 1))
})

test_that("bouts per time unit with unevenly spaced times", {
  # Create trajectory and bouts
  trajectory <- data.frame(x = rep(0, times = 10),
                           y = rep(0, times = 10),
                           t = c(0, 0.5, 0.78, 2.3, 5.99, 6, 6.01, 11, 20, 32))
  bouts <- c(1, 2, 3, 4, 5, 6, 7, 9, 10)

  # Find bouts per time unit
  bptu <- calculate_bouts_per_time_unit(trajectory, bouts, unit = 2)
  expect_equal(bptu, c(3, 1, 1, 2, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1))
})

test_that("distance calculations without bouts", {
  # Create trajectory
  trajectory <- data.frame(x = c(0, 0, 1, 2, 0, -1, 0),
                           y = c(0, 1, 1, 2, 2, 0, 2),
                           t = 1:7)

  # Find distances of trajectory
  distances <- calculate_distances(trajectory)
  expect_equal(distances, c(1, 1, sqrt(2), 2, sqrt(5), sqrt(5)))
})

test_that("distance calculations with bouts", {
  # Create trajectory
  trajectory <- data.frame(x = c(0, 0, 1, 2, 0, -1, 0),
                           y = c(0, 1, 1, 2, 2, 0, 2),
                           t = 1:7)
  bouts <- c(2, 4, 6, 7)

  # Find distances of trajectory
  distances <- calculate_distances(trajectory, bouts = bouts)
  expect_equal(distances, c(sqrt(5), sqrt(13), sqrt(5)))
})
