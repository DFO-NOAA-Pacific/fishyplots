#' Function to plot length frequency of fish species
#'
#' @param data a data frame made using pull_bio() from a fisheries survey, contains biological info
#' @param time_series TRUE or FALSE 
#' @return a ggplot object
#' @importFrom dplyr filter group_by summarize
#' @importFrom tidyr complete full_seq
#' @importFrom stats quantile
#' @importFrom ggplot2 ggplot aes geom_histogram facet_wrap theme_bw labs geom_errorbar geom_point theme
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
      theme(panel.grid = element_blank()) +
      labs(title = "Length Frequency", x = "Length (cm)", y = "Count")
    return(graph)
  }
  if (time_series == TRUE) {
    data_clean <- data |>
      filter(!is.na(Length_cm))
    
    summary_stats <- data_clean |>
      group_by(Year) |>
      summarize(median = quantile(Length_cm, 0.5),
                mean = mean(Length_cm),
                p25 = quantile(Length_cm, 0.25),
                p75 = quantile(Length_cm, 0.75),
                p97.5 = quantile(Length_cm, 0.975),
                p2.5 = quantile(Length_cm, 0.025))
    
    graph <- ggplot(data = summary_stats, aes(x = Year, y = mean)) +
      geom_errorbar(aes(ymin = p2.5, ymax = p97.5), width = 0.5, linewidth = 0.5, color = "grey") +
      geom_errorbar(aes(ymin = p25, ymax = p75), width = 0.5, linewidth = 1) +
      geom_point(shape = 21, fill = "grey", color = "black", size = 1.5, stroke = 1.2) +
      theme_bw() +
      labs(x = "", y = "Mean length (cm)", title = "Length Frequency",
           caption = "Error bars show 50% and 95% quantiles.") +
      theme(panel.grid = element_blank())
    
    return(graph)
  }
}
