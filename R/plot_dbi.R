#' Function and formatted data to plot design based index
#'
#' @param species common or scientific name of species of interest. See examples for options per dataset.
#' @param subsurvey subsurvey or survey group, required to specify servey/region to be plotted. See examples for options per dataset.
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
plot_dbi <- function(species, subsurvey) {
  
  data(all.dbi)
  
  # Assign surveys to larger region center if argument is a single center name
  if (is.character(subsurvey) && length(subsurvey) == 1) {
    if (subsurvey == "AK BSAI") {
      subsurvey <- c("U.S. Aleutian Islands", "U.S. Eastern Bering Sea Slope", 
                   "U.S. Eastern Bering Sea Standard Plus NW Region", "U.S. Northern Bering Sea")
    } else if (subsurvey == "AK GULF") {
      subsurvey <- "U.S. Gulf of Alaska"
    }else if (subsurvey == "PBS") {
      subsurvey <- c("SYN QCS","SYN WCVI", "SYN HS", "SYN WCHG")
    } else if (subsurvey == "NWFSC") {
      subsurvey <- c("U.S. West Coast")
    }
  }
  
  
    subset <- all.dbi %>% 
    filter(common_name == species | scientific_name == species) %>%
    filter(survey %in% subsurvey | region %in% subsurvey) 
  
  
    plot <- ggplot2::ggplot(data = subset) +
      ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = lwr, ymax = upr), fill = "lightgray") +
      ggplot2::geom_line(ggplot2::aes(x = year, y = est)) +
      ggplot2::geom_point(ggplot2::aes(x = year, y = est)) +
      ggplot2::ylab("Biomass (mt)") +
      ggplot2::xlab("Year") +
      ggplot2::ggtitle(unique(subset$common_name)) +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(
        labels = function(x) format(x, big.mark = ",", scientific = FALSE))
    
    if(length(subsurvey) > 1) {
      plot <- plot + facet_wrap( ~ survey, nrow = 2, scales = "free_y", drop = FALSE) + theme(strip.background = element_blank())
    }
    
    return(plot)
  }
   

