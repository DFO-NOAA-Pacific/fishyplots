#' Function and formatted data to plot standardized design based index
#'
#' @param species common or scientific name of species of interest. 
#' @param surveys region or surveys to be plotted. Takes a character list of survey names OR a single region ( "NWFSC", "AFSC", or "PBS").
#' @return a ggplot object, plot of standardized design based indicies of 1+ surveys. 
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point theme_bw scale_y_continuous xlab ylab
#' @importFrom dplyr %>% select rename
#' @export
#'
#' @examples
#' \dontrun{
#' plot_stan_dbi("sablefish", "PBS")
#' plot_stan_dbi("sablefish", c("U.S. West Coast", "U.S. Gulf of Alaska"))
#' }

plot_stan_dbi <- function(species, surveys) {
  
 data(all.dbi)
 
  
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
  for (center in unique(all.dbi$region)) {
    
    data <- subset(all.dbi, all.dbi$region == center)
    
    # Calculate standardized index
    stand_data <- data %>%
      dplyr::group_by(common_name, survey) %>%
      dplyr::filter(!is.na(est)) %>%
      dplyr::mutate(stand_est = est / mean(est), 
        stand_lwr = lwr / mean(est),
        stand_upr = upr / mean(est))
      
    
    # Filter for selected species and selected surveys
    filtered <- stand_data %>%
      dplyr::filter(common_name == species|scientific_name == species, survey %in% surveys)
    
    # Add to combined dataframe
    combined_df <- dplyr::bind_rows(combined_df, filtered)
  }
  
  #set colors: Okabe-ito coloblind pallete
  ok_colors <- setNames(palette.colors(palette = "Okabe-Ito")[2:(length(unique(combined_df$survey))+1)], unique(combined_df$survey))
  
  # Plot using said df
  plot <- ggplot2::ggplot(data = combined_df, 
                          ggplot2::aes(x = year, y = stand_est, color = survey)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point() +
    ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = stand_lwr, ymax = stand_upr, fill = survey), color = NA, alpha = 0.1) +
    ggplot2::scale_color_manual(values = ok_colors) +
    ggplot2::scale_fill_manual(values = ok_colors) +
    ggplot2::ylab("Standardized Biomass Index") +
    ggplot2::xlab("Year") +
    #ggplot2::ggtitle(species)  +
    ggplot2::theme_bw() +
    theme(legend.position="bottom")
  
  return(plot)
}
