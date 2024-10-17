angle <- function(v1, v2) {
  # Check arguments
  if (!is.matrix(v1) || !is.matrix(v2)) {
    stop("arguments must be matrices")
  }
  if (!is.numeric(v1) || !is.numeric(v2)) {
    stop("matrices must be numeric")
  }
  if (any(is.na(v1)) || any(is.na(v2))) {
    stop("matrices cannot have missing values")
  }
  if (dim(v1)[2] != 2 || dim(v2)[2] != 2) {
    stop("matrices must have two columns")
  }
  if (any(dim(v1) != dim(v2))) {
    stop("matrices must have the same dimensions")
  }

  # Calculate unit vectors
  u1 <- v1 / apply(v1, 1, norm, type = "2")
  u2 <- v2 / apply(v2, 1, norm, type = "2")

  # Calculate angles (in degrees) using geometric definition of dot product
  theta <- acos(rowSums(u1 * u2)) * (180 / pi)

  # Calculate angle directions using sign of last coordinate in cross product
  direction <- sign((u1[, 1] * u2[, 2]) - (u1[, 2] * u2[, 1]))

  # Return directed angles
  return(theta * direction)
}


distance <- function(p1, p2) {
  # Check arguments
  if (!is.matrix(p1) || !is.matrix(p2)) {
    stop("arguments must be matrices")
  }
  if (!is.numeric(p1) || !is.numeric(p2)) {
    stop("matrices must be numeric")
  }
  if (any(is.na(p1)) || any(is.na(p2))) {
    stop("matrices cannot have missing values")
  }
  if (dim(p1)[2] != 2 || dim(p2)[2] != 2) {
    stop("matrices must have two columns")
  }
  if (any(dim(p1) != dim(p2))) {
    stop("matrices must have the same dimensions")
  }

  # Distance formula
  return(apply(p2 - p1, 1, norm, type = "2"))
}
