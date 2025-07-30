#' Modeled prediction data for NWFSC region
#'
#' Contains prediction data from spatial models on NWFSC surveys for select species.
#'
#' @format A data frame with 2007632 rows and 9 variables:
#' \describe{
#'   \item{lon}{Original longitude coordinate}
#'   \item{lat}{Original latitude coordinate}
#'   \item{X}{New UTM coordinate}
#'   \item{Y}{New UTM coordinate}
#'   \item{prediction}{Model estimate}
#'   \item{species}{Species specification}
#'   \item{sanity}{Check if the model/prediction has any issues}
#'   \item{region}{Region NWFSC}
#'   \item{crs}{UTM crs used in coordinate transformation}
#' }
#'
"predictions_nwfsc"

#' Modeled prediction data for AFSC region
#'
#' Contains prediction data from spatial models on AFSC surveys for select species.
#'
#' @format A data frame with 2724672 rows and 10 variables:
#' \describe{
#'   \item{lon}{Original longitude coordinate}
#'   \item{lat}{Original latitude coordinate}
#'   \item{X}{New UTM coordinate}
#'   \item{Y}{New UTM coordinate}
#'   \item{prediction}{Model estimate}
#'   \item{species}{Species specification}
#'   \item{sanity}{Check if the model/prediction has any issues}
#'   \item{region}{Region AFSC}
#'   \item{crs}{UTM crs used in coordinate transformation}
#'   \item{survey}{Subregion}
#' }
#'
"predictions_afsc"

#' Modeled prediction data for PBS region
#'
#' Contains prediction data from spatial models on NWFSC surveys for select species.
#'
#' @format A data frame with 321480 rows and 9 variables:
#' \describe{
#'   \item{lon}{Original longitude coordinate}
#'   \item{lat}{Original latitude coordinate}
#'   \item{X}{New UTM coordinate}
#'   \item{Y}{New UTM coordinate}
#'   \item{prediction}{Model estimate}
#'   \item{species}{Species specification}
#'   \item{sanity}{Check if the model/prediction has any issues}
#'   \item{region}{Region PBS}
#'   \item{crs}{UTM crs used in coordinate transformation}
#' }
#'
"predictions_pbs"