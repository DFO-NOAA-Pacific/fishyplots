#' Function to plot length frequency of fish species
#'
#' @param data a data frame made using pull_bio() from a fisheries survey, contains biological info
#' @return a ggplot object
#' @importFrom dplyr filter 
#' @importFrom tidyr complete
#' @importFrom ggplot2 ggplot aes geom_histogram facet_wrap theme_bw labs
#' @importFrom scales full_seq 
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' }
length_frequency <- function(data) {
  data_clean <- data |>
    filter(!is.na(Length_cm), !is.na(Age_years)) |>
    complete(Year = full_seq(Year, 1))
  
  data_clean |>
    ggplot(aes(x= Length_cm)) +
    geom_histogram() +
    facet_wrap(~Year, nrow = 3) +
    theme_bw() +
    labs(title = "Length Frequency", x = "Length (cm)", y = "Count")
}