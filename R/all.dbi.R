#all.dbi.R

#' Dataset of design based biomass indices for all science centers
#'
#' Joined data from design based biomass estimates calculated separately by each science center
#'
#' @format ## `all.dbi`
#' A data frame with 3986 rows and 8 columns:
#' \describe{
#'   \item{region}{Research center that gathered data}
#'   \item{survey}{survey where data was collected}
#'   \item{common_name}{Common name of specimen collected }
#'   \item{scientific_name}{Scientific name of specimen collected}
#'   \item{year}{Year specimen was collected}
#'   \item{est}{Estimated design-based biomass}
#'   \item{lwr}{Lower confidence interval of biomass estimation}
#'   \item{upr}{Upper confidence interval of biomass estimation}
#'   ...
#' }
#'
"all.dbi"
