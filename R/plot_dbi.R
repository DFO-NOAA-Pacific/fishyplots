#' Function and formatted data to plot design based index
#'
#' @param species common or scientific name of species of interest. See examples for options per dataset.
#' @param surveys surveys or survey group, required to specify servey/region to be plotted. See examples for options per dataset.
#' @return a ggplot object, plot of design based indicies
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point theme_bw scale_y_continuous xlab ylab ggtitle
#' @importFrom dplyr %>% select rename
#' @export
#'
#' @examples
#' \dontrun{
#' # To see options for species and surveys in regions: "NWFSC", "AFSC", "PBS"
#' data(all.dbi)
#' 
#' all.dbi %>%
#' filter(region == "AFSC") %>%
#'  distinct(common_name)
#' 
#' all.dbi %>%
#' filter(region == "AFSC") %>%
#' distinct(survey) # do not use "U.S. Eastern Bering Sea Standard Region" ; use "U.S. Eastern Bering Sea Standard Plus NW Region"
#' 
#' 
#' # usage examples
#' plot_dbi("sablefish", "U.S. West Coast")
#' plot_dbi("dover sole", "U.S. Gulf of Alaska")
#' plot_dbi("arrowtooth flounder", "SYN QCS")
#' }


  
# DBI plot function
plot_dbi <- function(species, surveys) {
  
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
    }
  }
  
  
    subset <- all.dbi %>% 
    filter(common_name == species | scientific_name == species) %>%
    filter(survey %in% surveys | region %in% surveys) 
  
  
    plot <- ggplot2::ggplot(data = subset) +
      ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = lwr, ymax = upr), fill = "gray75", alpha = 0.7) +
      ggplot2::geom_line(ggplot2::aes(x = year, y = est)) +
      ggplot2::geom_point(ggplot2::aes(x = year, y = est), size = 2.5) +
      ggplot2::ylab("Biomass (mt)") +
      ggplot2::xlab("Year") +
      #ggplot2::ggtitle(unique(subset$common_name)) +
      ggplot2::theme_bw() +
      scale_x_continuous(minor_breaks = 1990:2024,
                         breaks = seq(1990, 2024, by = 5),
                         labels = function(x) ifelse(x %% 5 == 0, x, "")) +
      ggplot2::scale_y_continuous(
        labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
      theme(axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          panel.grid.major.x = element_line(size = 1.5, color = "grey95"),       # thicker for major
          panel.grid.minor.x = element_line(size = 0.7, color = "grey95"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
    
    if(length(surveys) > 1) {
      plot <- plot + facet_wrap( ~ survey, nrow = 2, scales = "free_y", drop = FALSE) + theme(strip.background = element_blank())
    }
    
    return(plot)
  }
   

