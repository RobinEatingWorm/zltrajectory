#' Validate a Numeric or Logical Vector
#'
#' @description This function performs validation checks on a single numerical
#'   or logical vector used as an argument in another function. There is no
#'   return value.
#'
#' @param arg an argument to be checked.
#' @param name the name of the argument as a string.
#' @param type the type of the argument as a string. Supported types are either
#'   `"numeric"` or `"logical"`.
#' @param length_one whether or not the argument must have length one.
#' @param positive if the argument is numeric, whether or not it must be
#'   positive.
validate_arg <- function(arg, name, type, length_one = TRUE, positive = TRUE) {
  # Run general checks
  if (length_one == TRUE && length(arg) != 1) {
    stop(paste0("argument \"", name, "\" must have length 1"))
  }

  # Run checks on numeric
  if (type == "numeric" && !is.numeric(arg)) {
    stop(paste0("argument \"", name, "\" must be numeric"))
  }
  if (type == "numeric" && any(is.na(arg))) {
    stop(paste0("argument \"", name, "\" cannot have missing values"))
  }
  if (type == "numeric" && positive == TRUE && any(arg < 0)) {
    stop(paste0("argument \"", name, "\" must be positive"))
  }

  # Run checks on logical
  if (type == "logical" && !is.logical(arg)) {
    stop(paste0("argument \"", name, "\" must be logical"))
  }

  # Nothing to return if all checks passed
  return(invisible(NULL))
}


#' Validate Data Used to Create a Trajectory
#'
#' @description This function performs validation checks on the arguments used
#'   in `create_trajectory()`. There is no return value.
#'
#' @param data the `data` argument from `create_trajectory()`.
#' @param x the `x` argument from `create_trajectory()`.
#' @param y the `y` argument from `create_trajectory()`.
#' @param t the `t` argument from `create_trajectory()`.
#' @param rate the `rate` argument from `create_trajectory()`.
validate_data <- function(data, x, y, t = NULL, rate = NULL) {
  # Run checks on data frame
  if (!is.data.frame(data)) {
    stop("argument \"data\" must be a data frame")
  }
  if (!(x %in% names(data))) {
    stop("argument \"x\" must be a name of the data frame")
  }
  if (!(y %in% names(data))) {
    stop("argument \"y\" must be a name of the data frame")
  }

  # Run checks on time and rate arguments
  if (!xor(is.null(t), is.null(rate))) {
    stop("specify \'t\' or \'rate\' but not both")
  }
  if (!is.null(t) && !(t %in% names(data))) {
    stop("argument \"t\" must be a name of the data frame")
  }
  if (!is.null(rate) && !is.numeric(rate)) {
    stop("argument \"rate\" must be numeric")
  }
  if (!is.null(rate) && rate <= 0) {
    stop("argument \"rate\" must be a positive number")
  }

  # Nothing to return if all checks passed
  return(invisible(NULL))
}


#' Validate Time Indices for Diagnostic Plots
#'
#' @description This function performs validation checks on the `time_indices`
#'   and `col_time_indices` arguments in `plot_diagnostics()`. There is no
#'   return value.
#'
#' @param time_indices the `time_indices` argument from `plot_diagnostics()`.
#' @param col_time_indices the `col_time_indices` argument from
#'   `plot_diagonstics()`.
validate_diagnostic_time_indices <- function(time_indices, col_time_indices) {
  # Run checks on time indices and colors
  if (!is.null(time_indices) && !is.numeric(time_indices) &&
      !is.list(time_indices)) {
    stop("argument \"time_indices\" must be numeric or list if provided")
  }
  if (!is.null(time_indices) && !is.vector(col_time_indices)) {
    stop("argument \"col_time_indices\" must be vector")
  }
  if (is.numeric(time_indices)) {
    validate_arg(time_indices, "time_indices", "numeric", length_one = FALSE)
  } else if (is.list(time_indices)) {
    for (i in seq_along(time_indices)) {
      validate_arg(time_indices[[i]],
                   paste0("time_indices[[", i, "]]"),
                   "numeric",
                   length_one = FALSE)
    }
    if (length(col_time_indices) != length(time_indices)) {
      stop(paste0("the lengths of \"col_time_indices\" ",
                  "and \"time indices\" must be the same"))
    }
  }

  # Nothing to return if all checks passed
  return(invisible(NULL))
}


#' Validate Matrices for Linear Algebra Functions
#'
#' @description This function performs validation checks on matrices used in
#'   `linear_algebra_angles()` and `linear_algebra_distances()`. There is no
#'   return value.
#'
#' @param m1 a matrix.
#' @param m2 a matrix.
validate_matrices <- function(m1, m2) {
  # Run checks on matrices
  if (!is.matrix(m1) || !is.matrix(m2)) {
    stop("arguments must be matrices")
  }
  if (!is.numeric(m1) || !is.numeric(m2)) {
    stop("matrices must be numeric")
  }
  if (any(is.na(m1)) || any(is.na(m2))) {
    stop("matrices cannot have missing values")
  }
  if (ncol(m1) != 2 || ncol(m2) != 2) {
    stop("matrices must have two columns")
  }
  if (any(dim(m1) != dim(m2))) {
    stop("matrices must have the same dimensions")
  }

  # Nothing to return if all checks passed
  return(invisible(NULL))
}


#' Validate Time Limits on Plots
#'
#' @description This function performs validation checks on the minimum and
#'   maximum times to show in plots that display time on the x-axis. There is no
#'   return value.
#'
#' @param time_min the minimum time to plot.
#' @param time_max the maximum time to plot.
validate_time_limits <- function(time_min, time_max) {
  # Run checks on times
  validate_arg(time_min, "time_min", "numeric", positive = FALSE)
  validate_arg(time_max, "time_max", "numeric", positive = FALSE)
  if (time_min >= time_max) {
    stop("time_min must be less than time_max")
  }

  # Nothing to return if all checks passed
  return(invisible(NULL))
}


#' Validate a Trajectory Data Frame
#'
#' @description This function validates a trajectory data frame created from
#'   `create_trajectory()`. Additionally, this function can also validate bout
#'   indices obtained from `find_bouts()`. There is no return value.
#'
#' @param trajectory a trajectory data frame.
#' @param bouts a vector of indices corresponding to the beginnings of bouts.
validate_trajectory <- function(trajectory, bouts = NULL) {
  # Run checks on data frame containing trajectory
  if (!is.data.frame(trajectory)) {
    stop("argument \"trajectory\" must be a data frame")
  }
  if (!all(c("x", "y", "t") %in% names(trajectory))) {
    stop("trajectory must have names \'x\', \'y\', and \'t\'")
  }
  if (!all(sapply(trajectory, is.numeric))) {
    stop("\'x\', \'y\', and \'t\' must be numeric")
  }
  if (any(is.na(trajectory))) {
    stop("trajectory cannot have missing values")
  }
  if (any(diff(trajectory$t) <= 0)) {
    stop("times must be increasing")
  }

  # Run checks on associated bouts
  if (!is.null(bouts) && !is.numeric(bouts)) {
    stop("argument \"bouts\" must be numeric")
  }
  if (!is.null(bouts) && any(is.na(bouts))) {
    stop("argument \"bouts\" cannot have missing values")
  }
  if (!is.null(bouts) && !all(bouts %in% seq_len(nrow(trajectory)))) {
    stop("invalid bout start indices found")
  }

  # Nothing to return if all checks passed
  return(invisible(NULL))
}
