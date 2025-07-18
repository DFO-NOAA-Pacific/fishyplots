#' Function to plot length frequency of fish species
#'
#' @param data biological fisheries data containing length information
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
#' # US West Coast
#' load("data/nwfsc_bio.rda")
#' data <- nwfsc_bio |> filter(common_name == "arrowtooth flounder")
#' length_frequency(data)
#' 
#' # Canada 
#' load("data/pbs_bio.rda")
#' data <- pbs_bio |> filter(common_name == "dover sole")
#' length_frequency(data, time_series = TRUE)
#' 
#' # Alaska
#' load("data/afsc_bio.rda")
#' data <- afsc_bio |> filter(common_name == "rex sole")
#' length_frequency(data, time_series = TRUE)
#' 
#' }
length_frequency <- function(data, time_series = FALSE) {
  if (time_series == FALSE) {
    data_clean <- data |>
      filter(!is.na(length_cm)) |>
      complete(year = full_seq(year, 1))
    
    graph <- data_clean |>
      ggplot(aes(x = length_cm)) +
      geom_histogram(binwidth = 4) +
      facet_wrap(~year, nrow = 3, drop = FALSE) +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      labs(title = "Length Frequency", x = "Length (cm)", y = "Count")
    return(graph)
  }
  if (time_series == TRUE) {
    data_clean <- data |>
      filter(!is.na(length_cm))
    
    summary_stats <- data_clean |>
      group_by(year) |>
      summarize(median = quantile(length_cm, 0.5),
                mean = mean(length_cm),
                p25 = quantile(length_cm, 0.25),
                p75 = quantile(length_cm, 0.75),
                p97.5 = quantile(length_cm, 0.975),
                p2.5 = quantile(length_cm, 0.025))
    
    graph <- ggplot(data = summary_stats, aes(x = year, y = mean)) +
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

