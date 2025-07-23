#' Function and formatted data to plot design based index
#'
#' @param species common or scientific name of species of interest. See examples for options per dataset.
#' @param subsurvey subsurvey, required to specify subservey and region to be plotted. See examples for options per dataset.
#' @return a ggplot object, plot of design based indicies
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point theme_bw scale_y_continuous xlab ylab ggtitle
#' @importFrom dplyr %>% select rename
#' @export
#'
#' @examples
#' \dontrun{
#' # To see options for species and subsurveys in regions: "NWFSC", "AFSC", "PBS"
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
  
  # if (!species %in% species_list) stop("") expand later with error messages
  
  data(all.dbi)
  subset <- all.dbi %>% 
    filter(common_name == species | scientific_name == species) %>%
    filter(survey == subsurvey) 
  
  
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
    return(plot)
  }
   

