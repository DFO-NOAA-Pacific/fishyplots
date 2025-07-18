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


