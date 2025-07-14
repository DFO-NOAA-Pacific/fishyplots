#' Function and formatted data to plot standardized design based index
#'
#' @param species common name of species of interest. See unique(data$common_name) for options per dataset.
#' @param regions center or subregions to be plotted. Takes a character list of region names OR a single center name ( "NWFSC", "AFSC", or "PBS") . See unique(data$region) for subregion options per center dataset.
#' @return a ggplot object, plot of standardized design based indicies of 1+ surveys. 
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point theme_bw scale_y_continuous xlab ylab
#' @importFrom here here
#' @import dplyr %>% select rename
#' @export
#'
#' @examples
#' \dontrun{
#' plot_stan_dbi("sablefish", "PBS")
#' plot_stan_dbi("sablefish", c("U.S. West Coast", "U.S. Gulf of Alaska"))
#' }


plot_stan_dbi <- function(species, regions) {
  
  # Make meta list of all centers and data
  biomass_list <- list(
    NWFSC = nwfsc_biomass,
    AFSC = afsc_biomass,
    PBS = pbs_biomass
  )
  
  # Assign subregions to larger survey center if argument is a single center name
  if (is.character(regions) && length(regions) == 1) {
    if (regions == "AFSC") {
      regions <- c("U.S. Gulf of Alaska", "U.S. Aleutian Islands", "U.S. Eastern Bering Sea Slope", 
                   "U.S. Eastern Bering Sea Standard Plus NW Region", 
                   "U.S. Eastern Bering Sea Standard Region", "U.S. Northern Bering Sea")
    } else if (regions == "PBS") {
      regions <- c("SYN QCS","SYN WCVI", "SYN HS", "SYN WCHG")
    } else if (regions == "NWFSC") {
      regions <- c("U.S. West Coast")
    }
  }
  
  # Create empty df to store plotting data
  combined_df <- data.frame()
  
  # for each center (all data...)
  for (center in names(biomass_list)) {
    
    data <- biomass_list[[center]]
    
    # Calculate standardized index
    stand_data <- data %>%
      dplyr::group_by(common_name, region) %>%
      dplyr::filter(!is.na(est)) %>%
      dplyr::mutate(stand_est = est / mean(est))
    
    # Filter for selected species and selected regions
    filtered <- stand_data %>%
      dplyr::filter(common_name == species, region %in% regions)
    
    # Add to combined dataframe
    combined_df <- dplyr::bind_rows(combined_df, filtered)
  }
  
  # Plot using said df
  plot <- ggplot2::ggplot(data = combined_df, 
                          ggplot2::aes(x = year, y = stand_est, color = region)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point() +
    ggplot2::ylab("Standardized Biomass Index") +
    ggplot2::xlab("Year") +
    ggplot2::ggtitle(species)  +
    ggplot2::theme_bw()
  
  return(plot)
}
