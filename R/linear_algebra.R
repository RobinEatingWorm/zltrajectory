#' Calculate Directed Angles
#'
#' @description This function uses linear algebra and vectorized operations to
#'   quickly calculate many directed angles formed by pairs of vectors. Vectors
#'   are assumed to have their tails at the origin of a 2D coordinate plane.
#'
#' @param v1 a matrix with n rows and 2 columns specifying n 2D vectors that
#'   each are an initial side of a directed angle.
#' @param v2 a matrix with n rows and 2 columns specifying n 2D vectors that
#'   each are a terminal side of a directed angle.
#'
#' @return A vector containing the magnitude and direction of each directed
#'   angle. A positive sign indicates a counterclockwise angle, while a negative
#'   sign indicates a clockwise angle.
linear_algebra_angles <- function(v1, v2) {
  # Validate arguments
  validate_matrices(v1, v2)

  # Calculate unit vectors
  u1 <- v1 / apply(v1, 1, norm, type = "2")
  u2 <- v2 / apply(v2, 1, norm, type = "2")

  # Calculate angles (in degrees) using geometric definition of dot product
  angles <- acos(rowSums(u1 * u2)) * (180 / pi)

  # Calculate angle directions using sign of last coordinate in cross product
  directions <- sign((u1[, 1] * u2[, 2]) - (u1[, 2] * u2[, 1]))

  # Special case where angles are exactly 180 degrees
  directions[directions == 0] = 1

  # Return directed angles
  return(angles * directions)
}


#' Calculate Distances
#'
#' @description This function uses linear algebra and vectorized operations to
#'   quickly calculate distances between many pairs of points on a 2D coordinate
#'   plane.
#'
#' @param p1 a matrix with n rows and 2 columms specifying n points in 2D space.
#' @param p2 a matrix with n rows and 2 columns specifying n points in 2D space.
#'
#' @return The distances between each corresponding pair of points.
linear_algebra_distances <- function(p1, p2) {
  # Validate arguments
  validate_matrices(p1, p2)

  # Distance formula
  return(apply(p2 - p1, 1, norm, type = "2"))
}
