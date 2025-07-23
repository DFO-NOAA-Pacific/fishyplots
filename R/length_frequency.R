#' Function to plot length frequency of fish species
#'
#' @param data biological fisheries data containing length information
#' @param region choose a science center (AFSC, NWFSC, PBS)
#' @param time_series TRUE or FALSE 
#' @return a ggplot object
#' @importFrom dplyr filter group_by summarize
#' @importFrom tidyr complete full_seq
#' @importFrom stats quantile
#' @importFrom ggplot2 ggplot aes geom_histogram facet_wrap theme_bw labs geom_errorbar geom_point theme
#' @importFrom ggsidekick theme_sleek
#' @export
#'
#' @examples
#' \dontrun{
#' data(nwfsc_bio)
#' data(afsc_bio)
#' data(pbs_bio)
#' all_data <- rbind(nwfsc_bio, afsc_bio, pbs_bio)
#' data <- all_data |> filter(common_name == "arrowtooth flounder")
#' 
#' length_frequency(data, region = c("AFSC", "PBS", "NWFSC"))
#' 
#' }

length_frequency <- function(data, region, time_series = TRUE) {
  if (time_series == FALSE) {
    data_clean <- data |>
      filter(!is.na(length_cm)) |>
      filter(science_center %in% region) |>
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
      filter(!is.na(length_cm)) |>
      filter(science_center %in% region)
    
    data_clean$science_center <- factor(data_clean$science_center, levels = c("AFSC", "PBS", "NWFSC"),
                                        labels = c("Alaska", "Canada", "U.S. West Coast"))
    
    summary_stats <- data_clean |>
      group_by(year, science_center) |>
      summarize(median = quantile(length_cm, 0.5),
                mean = mean(length_cm),
                p25 = quantile(length_cm, 0.25),
                p75 = quantile(length_cm, 0.75),
                p97.5 = quantile(length_cm, 0.975),
                p2.5 = quantile(length_cm, 0.025),
                .groups = "drop")
    
    graph <- ggplot(data = summary_stats, aes(x = year, y = mean)) +
      geom_errorbar(aes(ymin = p2.5, ymax = p97.5), width = 0.5, linewidth = 0.5, color = "grey") +
      geom_errorbar(aes(ymin = p25, ymax = p75), width = 0.5, linewidth = 1) +
      geom_point(shape = 21, fill = "grey", color = "black", size = 1.5, stroke = 1.2) +
      labs(x = "", y = "Mean length (cm)", title = "",
           caption = "Error bars show 50% and 95% quantiles.") +
      theme(panel.grid = element_blank())
    
    graph <- suppressWarnings(graph + theme_sleek())
    
    if (length(region) > 1) {
      graph <- graph +
        facet_wrap(~science_center)
    }
    
    return(graph)
  }
}