#' Function and formatted data to plot design based index
#'
#' @param species common or scientific name of species of interest. See examples for options per dataset.
#' @param surveys surveys or survey group, required to specify servey/region to be plotted. See examples for options per dataset.
#' @param facet_all if TRUE this will facet all surveys regardless of missing data, if FALSE then only the region(s) with data will be faceted
#' @return a ggplot object, plot of design based indicies
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point theme_bw scale_y_continuous xlab ylab ggtitle element_line element_text element_blank theme facet_wrap
#' @importFrom dplyr %>% select rename
#' @importFrom utils data
#' @export
#'
#' @examples
#' \dontrun{
#' # To see options for species and surveys in regions: "NWFSC", "AFSC", "PBS"
#' data(all_dbi)
#' 
#' all_dbi %>%
#' filter(region == "AFSC") %>%
#'  distinct(common_name)
#' 
#' all_dbi %>%
#' filter(region == "AFSC") %>%
#' distinct(survey) 
#' 
#' 
#' # usage examples
#' plot_dbi("sablefish", "U.S. West Coast")
#' plot_dbi("dover sole", "U.S. Gulf of Alaska")
#' plot_dbi("arrowtooth flounder", "SYN QCS")
#' }

# DBI plot function
plot_dbi <- function(species, surveys, facet_all = TRUE) {

  
  # Assign surveys to larger region center if argument is a single center name (ex input "PBS" will plot c("SYN QCS","SYN WCVI", "SYN HS", "SYN WCHG"))
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
    }
  }
  
  # subset dbi data to species and region/survey
    subset <- fishyplots::all_dbi %>% 
    filter(.data$common_name == species | .data$scientific_name == species) %>%
    filter(.data$survey %in% surveys | .data$region %in% surveys) 
    
    if (nrow(subset) == 0) {
      return(
        ggplot2::ggplot() + 
          ggplot2::theme_void() + 
          ggplot2::ggtitle(paste0("No biomass data available for ", species, " in this region."))
      ) # avoid error if no data for given spec/survey
    }
    else{
  #plot
    plot <- ggplot2::ggplot(data = subset) +
      ggplot2::geom_ribbon(ggplot2::aes(x =.data$year, ymin = .data$lwr, ymax = .data$upr), fill = "gray75", alpha = 0.7) +
      ggplot2::geom_line(ggplot2::aes(x = .data$year, y = .data$est)) +
      ggplot2::geom_point(ggplot2::aes(x = .data$year, y = .data$est), size = 2.5) +
      ggplot2::ylab("Biomass (mt)") +
      ggplot2::xlab("Year") +
      #ggplot2::ggtitle(unique(subset$common_name)) +
      ggplot2::theme_bw() +
      scale_x_continuous(minor_breaks = seq(1990, max(all_dbi$year), by=1), # 1990 is earliest year, change if needed and adjust breaks accordingly
                         breaks = seq(1990, max(subset$year), by = 5),
                         labels = function(x) ifelse(x %% 5 == 0, x, "")) + # only label every 5 years
      ggplot2::scale_y_continuous(
        labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
      theme(axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          panel.grid.major.x = element_line(size = 1.5, color = "grey95"),# thicker for major grid
          panel.grid.minor.x = element_line(size = 0.7, color = "grey95"),#thin lines for minor grid
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
    
    #facet wrap if plotting all regions
    if (length(surveys) > 1) {
      if (facet_all == TRUE) {
        plot <- plot + facet_wrap( ~ survey, nrow = 2, scales = "free_y", drop = FALSE) + theme(strip.background = element_blank())
        # find which regions have no data
        empty_surveys <- setdiff(surveys, unique(subset$survey))

        if (length(empty_surveys) > 0) {
          # plot "no data" message in empty facets
          plot <- plot +
            geom_text(
              data = data.frame(survey = factor(empty_surveys,
                                                levels = unique(surveys))),
              aes(x = mean(range(subset$year)), y = 0, label = "No data"),
              inherit.aes = FALSE) 
        }
      }
      else if (facet_all == FALSE) {
        plot <- plot + facet_wrap( ~ survey, nrow = 2, scales = "free_y", drop = FALSE) + theme(strip.background = element_blank())
      }}
    
    
    
    
    return(plot)
    }
}
   

