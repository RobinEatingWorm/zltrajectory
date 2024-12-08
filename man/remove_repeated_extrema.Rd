% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_bouts.R
\name{remove_repeated_extrema}
\alias{remove_repeated_extrema}
\title{Extract Bouts and Corresponding Peaks}
\usage{
remove_repeated_extrema(distances, peaks, troughs)
}
\arguments{
\item{distances}{distances traveled between consecutive points. These should
have been calculated with `linear_algebra_distances()`.}

\item{peaks}{indices of peaks.}

\item{troughs}{indices of troughs (potential beginnings of bouts)}
}
\value{
a list containing any remaining troughs and peaks after repeats are
  removed. Troughs are stored under the name `bouts` since they indicate when
  bouts begin. Peaks are stored under the name `peaks`.
}
\description{
This function uses a vector of peak indices and a vector of
  trough indices (indices that potentially correspond to the beginnings of
  bouts) and performs two actions. First, it removes peaks that occur after
  other peaks without a trough in between. Second, it removes troughs that
  occur before other troughs without a peak in between. The remaining indices
  are those indicating beginnings of bouts and the peaks used to identify
  them.
}
