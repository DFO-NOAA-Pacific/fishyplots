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
"lw_vb_predictions"

#' Alternate data storage for von Bertalanffy predictions
#'
#' Contains von Bertalanffy growth curve estimates for each fish age bracket 
#'
#' @format A data frame with 3061 rows and 9 variables:
#' \describe{
#'   \item{age_years}{Fish age in years}
#'   \item{fit}{Growth estimate (length cm)}
#'   \item{sex}{Fish sex, male or female}
#'   \item{linf}{Asymptotic length of growth curve}
#'   \item{k}{growth coefficient, rate of growth}
#'   \item{t0}{exponent, slope of length-weight regression }
#'   \item{center}{Region (NWFSC, AFSC, PBS)}
#'   \item{common}{Species common name}
#'   \item{scientific}{Species scientific name}
#' }
#'
"vb_predictions"