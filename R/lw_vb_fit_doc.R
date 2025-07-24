#' lw_vb_fit_doc: Documentation of lw and vb predictions
#'
#' Contains log-log length-weight regression estimates for the top species of each survey group, separated by sex. 
#'
#' @format A data frame with 164 rows and 7 variables:
#' \describe{
#'   \item{region}{science center of data collection}
#'   \item{survey}{survey within region}
#'   \item{common}{common species name}
#'   \item{scientific}{scientific species name}
#'   \item{sex}{sex, M or F}
#'   \item{a}{Scaling coefficient, intercept of length-weight regression }
#'   \item{b}{exponent, slope of length-weight regression }
#' }
#'
"lw_predictions"

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