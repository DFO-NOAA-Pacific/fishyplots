#' lw_vb_fit_doc: Documentation of lw_vb_predictions.rda
#'
#' Contains log-log length-weight regression estimates and Von Bertalanffy growth curve estimates for the top species of each science center, separated by sex. 
#'
#' @format A data frame with 130 rows and 9 variables:
#' \describe{
#'   \item{center}{science center of data collection}
#'   \item{common}{common species name}
#'   \item{scientific}{scientific species name}
#'   \item{sex}{sex, M or F}
#'   \item{a}{Scaling coefficient, intercept of length-weight regression }
#'   \item{b}{exponent, slope of length-weight regression }
#'   \item{k}{growth coefficient, rate of growth}
#'   \item{Linf}{Asymptotic length of growth curve}
#'   \item{t0}{intercept of curve, age at length 0 }
#' }
#'