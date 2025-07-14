#' Function and formatted data to plot design based index
#'
#' @param data survey data options loaded in script: nwfsc_biomass, afsc_biomass, pbs_biomass
#' @param species common name of species of interest. See unique(data$common_name) for options per dataset.
#' @param subregion subregion required to specify subregion to be plotted. See unique(data$region) for options per dataset.
#' @return a ggplot object, plot of design based indicies
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point theme_bw scale_y_continuous xlab ylab
#' @importFrom here here
#' @importFrom dplyr %>% select rename
#' @export
#'
#' @examples
#' \dontrun{
#' plot_dbi(nwfsc_biomass, "sablefish", "U.S. West Coast")
#' plot_dbi(afsc_biomass, "sablefish", "U.S. Gulf of Alaska")
#' plot_dbi(pbs_biomass, "sablefish", "SYN QCS")
#' }


  
# DBI plot function
plot_dbi <- function(dbi_data, species, subregion) {
    
    plot <- ggplot2::ggplot(data = subset(dbi_data, dbi_data$common_name == species & dbi_data$region == subregion)) +
      ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = lwr, ymax = upr), fill = "lightgray") +
      ggplot2::geom_line(ggplot2::aes(x = year, y = est)) +
      ggplot2::geom_point(ggplot2::aes(x = year, y = est)) +
      ggplot2::ylab("Biomass (mt)") +
      ggplot2::xlab("Year") +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(
        labels = function(x) format(x, big.mark = ",", scientific = FALSE))
    return(plot)
  }
