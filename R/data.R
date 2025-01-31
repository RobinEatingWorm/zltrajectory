#' Trajectory of Zebrafish Exposed to Cadmium Chloride
#'
#' @description Trajectory data of one zebrafish larva from an experiment
#'   conducted to assess behavioral responses to environmental contaminants.
#'   This zebrafish was exposed to 1.25 mg/L cadmium chloride over a 94-minute
#'   trial consisting of an initial 10-minute acclimatization period in the
#'   dark, followed by six cycles of 10 minutes in the light and 4 minutes in
#'   the dark.
#'
#'   The original dataset was stored in a text file and consisted of metadata at
#'   the beginning of the file, followed by a table of measurements recorded
#'   during the trial. The version presented in this package has adapted the
#'   original by removing the metadata and only saving three columns in the
#'   table required for this package.
#'
#' @format A data frame with 169214 observations on 3 variables.
#'   \describe{
#'     \item{Trial.time}{The time since the beginning of the trial that an
#'     observation was collected (s)}
#'     \item{X.center}{The x-position of the center of the zebrafish (mm)}
#'     \item{Y.center}{The y-position of the center of the zebrafish (mm)}
#'   }
#'
#' @examples
#' cadmium_zebrafish
#'
#' @source Nuesser, L. (2016). Designating a segregation parameter for
#'   contamination detection in behavioral assays using zebrafish larvae;
#'   supporting information (Version 4). figshare.
#'   https://doi.org/10.6084/m9.figshare.3102133.v4
#'
#' @references Nüßer, L. K., Skulovich, O., Hartmann, S., Seiler, T.-B.,
#'   Cofalla, C., Schuettrumpf, H., Hollert, H., Salomons, E., & Ostfeld, A.
#'   (2016). A sensitive biomarker for the detection of aquatic contamination
#'   based on behavioral assays using zebrafish larvae. \emph{Ecotoxicology and
#'   Environmental Safety}, \emph{133}, 271–280.
#'   https://doi.org/10.1016/j.ecoenv.2016.07.033
"cadmium_zebrafish"
