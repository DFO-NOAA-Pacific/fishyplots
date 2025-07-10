#' Function to plot length frequency of fish species
#'
#' @param data a data frame made using pull_bio() from a fisheries survey, contains biological info
#' @param time_series TRUE or FALSE 
#' @return a ggplot object
#' @importFrom dplyr filter group_by
#' @importFrom tidyr complete full_seq
#' @importFrom ggplot2 ggplot aes geom_histogram facet_wrap theme_bw labs geom_boxplot
#' @export
#'
#' @examples
#' \dontrun{
#' data <- pull_bio(survey = "NWFSC.Combo", common_name = "Pacific hake")
#' length_frequency(data)
#' 
#' data <- pull_bio(survey = "NWFSC.Combo", common_name = "Dover sole")
#' length_frequency(data, time_series = TRUE)
#' }
length_frequency <- function(data, time_series = FALSE) {
  if (time_series == FALSE) {
    data_clean <- data |>
      filter(!is.na(Length_cm)) |>
      complete(Year = full_seq(Year, 1))
    
    graph <- data_clean |>
      ggplot(aes(x = Length_cm)) +
      geom_histogram(binwidth = 4) +
      facet_wrap(~Year, nrow = 3, drop = FALSE) +
      theme_bw() +
      labs(title = "Length Frequency", x = "Length (cm)", y = "Count")
    return(graph)
  }
  if (time_series == TRUE) {
    data_clean <- data |>
      filter(!is.na(Length_cm))
    
    graph <- data_clean |>
      group_by(Year) |>
      ggplot(aes(x = Year, y = Length_cm, group = Year)) +
      geom_boxplot() +
      labs(x = "", y = "Length (cm)", title = "Length Frequency") +
      theme_bw()
    return(graph)
  }
}
