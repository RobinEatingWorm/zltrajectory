test_that("trajectory creation with time", {
  # Create data
  data <- data.frame(x_position = c(1, 2, 3, NA, 6),
                     y_position = c("1.5", "a", NA, "4", "-2"),
                     time = c(0, 4, 5.7, 10.4, 12))

  # NAs should be introduced by coercion
  expect_warning(trajectory <- create_trajectory(data,
                                                 x = "x_position",
                                                 y = "y_position",
                                                 t = "time"))

  # Check that trajectory data frame is as expected
  expect_equal(trajectory,
               data.frame(x = c(1, 6), y = c(1.5, -2.0), t = c(0, 12)))
})

test_that("trajectory creation with rate", {
  # Create data
  data <- data.frame(x_position = c(1, 2, 3, NA, 6),
                     y_position = c("1.5", "a", NA, "4", "-2"),
                     time = c(0, 4, 5.7, 10.4, 12))

  # NAs should be introduced by coercion
  expect_warning(trajectory <- create_trajectory(data,
                                                 x = "x_position",
                                                 y = "y_position",
                                                 rate = 2.5))

  # Check that trajectory data frame is as expected
  expect_equal(trajectory,
               data.frame(x = c(1, 6), y = c(1.5, -2.0), t = c(0, 10)))
})

test_that("trajectory creation fails with both time and rate", {
  # Create data
  data <- data.frame(x_position = c(1, 2, 3, NA, 6),
                     y_position = c("1.5", "a", NA, "4", "-2"),
                     time = c(0, 4, 5.7, 10.4, 12))

  # Should not work with both time and rate arguments specified
  expect_error(trajectory <- create_trajectory(data,
                                               x = "x_position",
                                               y = "y_position",
                                               t = "time",
                                               rate = 2.5))
})
