% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_args.R
\name{validate_arg}
\alias{validate_arg}
\title{Validate a Numeric or Logical Vector}
\usage{
validate_arg(arg, name, type, length_one = TRUE, positive = TRUE)
}
\arguments{
\item{arg}{an argument to be checked.}

\item{name}{the name of the argument as a string.}

\item{type}{the type of the argument as a string. Supported types are either
`"numeric"` or `"logical"`.}

\item{length_one}{whether or not the argument must have length one.}

\item{positive}{if the argument is numeric, whether or not it must be
positive.}
}
\description{
This function performs validation checks on a single numerical
  or logical vector used as an argument in another function. There is no
  return value.
}
