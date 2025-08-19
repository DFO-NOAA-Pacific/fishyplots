#' Function and formatted data to plot standardized design based index
#'
#' @param species common or scientific name of species of interest. 
#' @param surveys region or surveys to be plotted. Takes a character list of survey names OR a single region ( "NWFSC", "AFSC", or "PBS").
#' @return a ggplot object, plot of standardized design based indicies of 1+ surveys. 
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point theme_bw scale_y_continuous xlab ylab
#' @importFrom dplyr %>% rename
#' @importFrom stats setNames
#' @importFrom grDevices palette.colors
#' @importFrom utils data
#' @export
#'
#' @examples
#' \dontrun{
#' plot_stan_dbi("sablefish", "PBS")
#' plot_stan_dbi("sablefish", c("U.S. West Coast", "U.S. Gulf of Alaska"))
#' }
#silence notes
utils::globalVariables("region")

plot_stan_dbi <- function(species, surveys) {
  
  
  # Assign surveys to larger region center if argument is a single center name
  if (is.character(surveys) && length(surveys) == 1) {
    if (surveys == "AK BSAI") {
      surveys <- c("U.S. Aleutian Islands", "U.S. Eastern Bering Sea Slope", 
                     "U.S. Eastern Bering Sea Standard Plus NW Region", "U.S. Northern Bering Sea")
    } else if (surveys == "AK GULF") {
      surveys <- "U.S. Gulf of Alaska"
    }else if (surveys == "PBS") {
      surveys <- c("SYN QCS","SYN WCVI", "SYN HS", "SYN WCHG")
    } else if (surveys == "NWFSC") {
      surveys <- c("U.S. West Coast")
    } else (surveys == surveys)
  }
  
  # Create empty df to store plotting data
  combined_df <- data.frame()
  
  # for each region (all data...)
  for (center in unique(fishyplots::all.dbi$region)) {
    
    data <- subset(fishyplots::all.dbi, region == center)
    
    # Calculate standardized index
    stand_data <- data %>%
      dplyr::group_by(.data$common_name, .data$survey) %>%
      dplyr::filter(!is.na(.data$est)) %>%
      dplyr::mutate(stand_est = .data$est / mean(.data$est), 
        stand_lwr = .data$lwr / mean(.data$est),
        stand_upr = .data$upr / mean(.data$est))
      
    
    # Filter for selected species and selected surveys
    filtered <- stand_data %>%
      dplyr::filter(.data$common_name == species|.data$scientific_name == species, .data$survey %in% surveys)
    
    # Add to combined dataframe
    combined_df <- dplyr::bind_rows(combined_df, filtered)
  }
  
  #manually extend okabe ito  coloblind pallete
  ok_base <- palette.colors(palette = "Okabe-Ito")[2:9] #skip black as first color
  ok_extend <- c(ok_base, "#555555", "#999933") #extend palette to fit length of surveys
  
  # Assign to surveys
  ok_colors <- setNames(ok_extend[seq_along(unique(combined_df$survey))],
                        unique(combined_df$survey))

    # Plot using said df
  plot <- ggplot2::ggplot(data = combined_df, 
                          ggplot2::aes(x = .data$year, y = .data$stand_est, color = .data$survey)) +
    ggplot2::geom_line(linewidth = 1, alpha = 0.6) +
    ggplot2::geom_point(size = 4) +
    ggplot2::geom_ribbon(ggplot2::aes(x = .data$year, ymin = .data$stand_lwr, ymax = .data$stand_upr, fill = .data$survey), color = NA, alpha = 0.1) +
    ggplot2::scale_color_manual(values = ok_colors) +
    ggplot2::scale_fill_manual(values = ok_colors) +
    scale_x_continuous(minor_breaks = 1990:2024,
                       breaks = seq(1990, 2024, by = 5),
                       labels = function(x) ifelse(x %% 5 == 0, x, "")) +
    ggplot2::ylab("Standardized Biomass Index") +
    ggplot2::xlab("Year") +
    #ggplot2::ggtitle(species)  +
    ggplot2::theme_bw() +
    labs(caption = "Note: Each survey is standardized to its own mean.") +
    theme(legend.position="bottom",panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
          plot.caption.position = "plot",         # places it below the plot area
          plot.caption = element_text(hjust = 0, size = 12),
          axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          panel.grid.major.x = element_line(size = 1.5, color = "grey95"), # thicker for major
          panel.grid.minor.x = element_line(size = 0.7, color = "grey95"))
  
  return(plot)
  
}
