#' nwfsc_bio: Biological data for NWFSC survey
#'
#' Contains biological data from NWFSC surveys for available top species defined in data-raw/nwfsc_top.rds.
#'
#' @format A data frame with 1491160 rows and 11 variables:
#' \describe{
#'   \item{region}{region of data collection}
#'   \item{survey}{subregion}
#'   \item{year}{year specimen was collected}
#'   \item{common_name}{common name of specien}
#'   \item{scientific_name}{scientific name of specimen}
#'   \item{sex}{specimen sex, M, F, or U (unknown)}
#'   \item{length_cm}{length of specimen, in cm}
#'   \item{weight_kg}{weight of specimen, in kg}
#'   \item{age_years}{age of specimen, in years}
#'   \item{otosag_id}{Unique to nwfsc dataset, defines number of available age structures}
#'   \item{depth_m}{Depth in meters}
#' }
#'
"nwfsc_bio"

#' afsc_bio: Biological data for AFSC survey
#'
#' Contains biological data from AFSC surveys for available top species defined in data-raw/afsc_top.rds.
#'
#' @format A data frame with 485258 rows and 10 variables:
#' \describe{
#'   \item{region}{region of data collection}
#'   \item{survey}{subregion}
#'   \item{year}{year specimen was collected}
#'   \item{common_name}{common name of specien}
#'   \item{scientific_name}{scientific name of specimen}
#'   \item{sex}{specimen sex, M, F, or U (unknown)}
#'   \item{length_cm}{length of specimen, in cm}
#'   \item{weight_kg}{weight of specimen, in kg}
#'   \item{age_years}{age of specimen, in years}
#'   \item{depth_m}{Depth in meters}
#' }
#'
"afsc_bio"

#' pbs_bio: Biological data for PBS survey
#'
#' Contains biological data from PBS surveys for available top species defined in data-raw/pbs_top.rds.
#'
#' @format A data frame with 906520 rows and 10 variables:
#' \describe{
#'   \item{region}{region of data collection}
#'   \item{survey}{subregion}
#'   \item{year}{year specimen was collected}
#'   \item{common_name}{common name of specien}
#'   \item{scientific_name}{scientific name of specimen}
#'   \item{sex}{specimen sex, M, F, or U (unknown)}
#'   \item{length_cm}{length of specimen, in cm}
#'   \item{weight_kg}{weight of specimen, in kg}
#'   \item{age_years}{age of specimen, in years}
#'   \item{depth_m}{Depth in meters}
#' }
#'
"pbs_bio"

#' ak_survey_lengths: length count data from AFSC surveys
#'
#' Contains biological length counts from AFSC surveys for available top species defined in data-raw/afsc_top.rds, separated by survey group.
#' Compiled from large external dataset. Filtering can be found in data-raw/filter_ak_lengths.R
#'
#' @format A data frame with 1158 rows and 5 variables:
#' \describe{
#'   \item{year}{year of data collection}
#'   \item{survey}{subregion group within AFSC: AK GULF (Gulf of Alaska) or AK BSAI (Bering Sea and Aleutian Islands)}
#'   \item{common_name}{common name of specien}
#'   \item{scientific_name}{scientific name of specimen}
#'   \item{length.count}{number of lengths taken for specimen in survey and year}
#' }
#'
"ak_survey_lengths"

#' all_catch: Catch data (occurrences) for all surveys/regions
#'
#' Contains catch data from all surveys/regions for available top species. Catch data pulled from packages nwfscSurvey and surveyjoin
#'
#' @format A data frame with 4052 rows and 8 variables:
#' \describe{
#'   \item{region}{region of data collection}
#'   \item{survey}{subregion}
#'   \item{common_name}{common name of specimen}
#'   \item{scientific_name}{scientific name of specimen}
#'   \item{year}{year tows were conducted}
#'   \item{n_tows}{total number of tows conducted}
#'   \item{n_pos}{number of positive tows}
#'   \item{proportion_pos}{proportion of total tows that were positive}
#'   
#'   
#' }
#'
"all_catch"
