#Internals

### Example: 
# species_list <- c(1,2,3)
# x <- c( 4,5,6)
# 
# # all objects added to same usethis save
# usethis::use_data(species_list = species_list, x = x, internal = TRUE)


#' Convert numeric sex codes for standardization
#'
#' Converts 1 -> M, 2 -> F, 0 or 3 -> U, any other to NA
#' Internal function.
#'
#' @param x vector of sex codes
#' @return standardized sex characters
#' @noRd

# function to convert numeric sex codes to M, F and U (Unknown)
convert_sex <- function(x) { 
  x <- as.character(x)
  x <- dplyr::case_when(
    x == 1 ~ "M",
    x == 2 ~ "F",
    x %in% c("0", "3") ~ "U",
    TRUE ~ NA_character_  # set unknown or unexpected values as NA
  )
}

#' group surveys to differentiate AK Gulf and AK BSAI surveys for later data visualization. 
#'
#' groups all PBS surveys under "PBS". groups Gulf of Alaska into "AK GULF". groups Bering Sea and Aleutian Island surveys into "AK BSAI"
#' Internal function.
#'
#' @param data bio data
#' @return data with renamed survey column
#' @noRd
group_survey <- function(data){
    data$survey <- dplyr::case_when(
      grepl("NWFSC", data$survey, ignore.case = TRUE) ~ "NWFSC",
      grepl("U.S. West Coast", data$survey, ignore.case = TRUE) ~ "NWFSC",
      grepl("SYN", data$survey, ignore.case = TRUE) ~ "PBS",
      grepl("Gulf", data$survey, ignore.case = TRUE) ~ "AK GULF",
      TRUE ~ "AK BSAI"
    )
    return(data)
  }
  
#' cleans and standardizes common names of top fishes
#' makes all common names lowercase and renames species with multiple common names
#'
#' @param data bio data
#' @return bio data with cleaned common names
#' @noRd
clean_fishnames <- function(data) {
  data <- data %>% 
    mutate(
      common_name = stringr::str_to_lower(.data$common_name),
      scientific_name = stringr::str_to_lower(.data$scientific_name),
      common_name = dplyr::case_when(
        grepl("merluccius productus", scientific_name) ~ "pacific hake",
        grepl("squalus suckleyi", scientific_name) ~ "pacific spiny dogfish",
        TRUE ~ common_name # keep original if no match
        ))
  return(data)
}
