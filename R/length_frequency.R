#' Function to plot length frequency of fish species
#'
#' @param data biological fisheries data containing length information
#' @param subregions choose NWFSC, PBS, AK Gulf, and/or AK BSAI
#' @param common species common name
#' @param time_series TRUE or FALSE 
#' @param facet_all if TRUE this will facet all surveys regardless of missing data, if FALSE then only the region(s) with data will be faceted 
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
#' nwfsc_bio <- nwfsc_bio |> select(-otosag_id)
#' all_data <- rbind(nwfsc_bio, afsc_bio, pbs_bio)
#' 
#' length_frequency(all_data, c("AK BSAI", "AK GULF", "PBS", "NWFSC"), "arrowtooth flounder")
#' 
#' }

length_frequency <- function(data, subregions, common, time_series = TRUE, facet_all = TRUE) {
  # Clean data
  data_clean <- data |>
    filter(!is.na(length_cm)) |>
    filter(survey %in% subregions) |>
    filter(common_name == common)
  
  # Exit if no data
  if (nrow(data_clean) == 0) {
    #message(paste0("No age data available for ", common, " in ", paste(subregions, collapse = ","), "."))
    return(ggplot() + theme_void() + ggtitle("No length frequency data available."))
  }
  
  if (time_series == FALSE) {
    data_clean <- data_clean |>
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
    
    data_clean$survey <- factor(data_clean$survey, levels = c("AK BSAI", "AK GULF", "PBS", "NWFSC"),
                                        labels = c("Aleutians/Bering Sea", "Gulf of Alaska", "Canada", "U.S. West Coast"))
    
    summary_stats <- data_clean |>
      group_by(year, survey) |>
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
      theme_bw() +
      theme(panel.grid = element_blank()) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    #graph <- suppressWarnings(graph + theme_sleek())
    
    if (length(subregions) > 1) {
      if (facet_all == TRUE) {
        graph <- graph + facet_wrap(~survey, ncol = 4, drop = FALSE)
      } else if (facet_all == FALSE) {
        graph <- graph + facet_wrap(~survey, ncol = 4)
      }
      
    }
    
    return(graph)
  }
}
