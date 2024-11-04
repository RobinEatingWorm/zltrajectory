test_that("angle calculations", {
  # Create angles in multiples of 45 degrees
  v1 <- matrix(rep(c(1, 0), times = 8), nrow = 8, ncol = 2, byrow = TRUE)
  v2 <- matrix(c(1, 0, 1, 1, 0, 1, -1, 1, -1, 0, -1, -1, 0, -1, 1, -1),
               nrow = 8,
               ncol = 2,
               byrow = TRUE)

  # Check that angles are calculated correctly
  angles <- linear_algebra_angles(v1, v2)
  expect_equal(angles, c(0, 45, 90, 135, 180, -135, -90, -45))
})

test_that("angle for no movement is NaN",{
  # Create degenerate angle
  v1 <- matrix(c(0, 0), nrow = 1, ncol = 2)
  v2 <- matrix(c(0, 0), nrow = 1, ncol = 2)

  # Check that angle is NaN
  angles <- linear_algebra_angles(v1, v2)
  expect_equal(angles, NaN)
})

test_that("distance calculations", {
  # Create various distances
  p1 <- matrix(rep(0, times = 8), nrow = 4, ncol = 2)
  p2 <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1), nrow = 4, ncol = 2, byrow = TRUE)

  # Check that distances are calculated correctly
  distances <- linear_algebra_distances(p1, p2)
  expect_equal(distances, c(0, 1, sqrt(2), 1))
})
