#' lw_vb_fit_doc: Documentation of lw and vb predictions
#'
#' Contains log-log length-weight regression estimates for the top species of each survey group, separated by sex. 
#'
#' @format A data frame with 10122 rows and 9 variables:
#' \describe{
#'   \item{fit_length}{fish lengths in cm }
#'   \item{fit_weight}{predicted weight in kg at length in cm}
#'   \item{sex}{sex, M or F}
#'   \item{a}{Scaling coefficient, intercept of length-weight regression }
#'   \item{b}{exponent, slope of length-weight regression }
#'   \item{region}{science center of data collection}
#'   \item{common_name}{common species name}
#'   \item{survey}{survey within region}
#'   \item{scientific_name}{scientific species name}
#' }
"lw_predictions"

#' Alternate data storage for von Bertalanffy predictions
#'
#' Contains von Bertalanffy growth curve estimates for each fish age bracket 
#'
#' @format A data frame with 3854 rows and 9 variables:
#' \describe{
#'   \item{age_years}{Fish age in years}
#'   \item{fit}{Growth estimate (length cm)}
#'   \item{sex}{Fish sex, male or female}
#'   \item{linf}{Asymptotic length of growth curve}
#'   \item{k}{growth coefficient, rate of growth}
#'   \item{t0}{exponent, slope of length-weight regression }
#'   \item{survey}{subregion (NWFSC, PBS, AK GULF, AK BSAI)}
#'   \item{common_name}{Species common name}
#'   \item{scientific_name}{Species scientific name}
#' }
#'
"vb_predictions"