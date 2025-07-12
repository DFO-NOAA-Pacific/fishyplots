#' Function and formatted data to plot design based index
#'
#' @param data survey data options loaded in script: nwfsc_biomass, afsc_biomass, pbs_biomass
#' @param species common name of species of interest. See unique(data$common_name) for options per dataset.
#' @param subregion subregion required if using data = afsc_biomass or data = pbs_biomass to specify subregion to be plotted. See unique(data$region) for options per dataset.
#' @return a ggplot object, plot of design based indicies
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point theme_bw scale_y_continuous xlab ylab
#' @importFrom here here
#' @import dplyr %>% select rename
#' @export
#'
#' @examples
#' \dontrun{
#' plot_dbi(nwfsc_biomass, "sablefish")
#' plot_dbi(afsc_biomass, "sablefish", "U.S. Gulf of Alaska")
#' plot_dbi(pbs_biomass, "sablefish", "SYN QCS")
#' }

#pull data and reformat: 
load(here::here("data-raw", "nwfsc_biomass.rda")) # nwfsc index data: object nwfsc_biomass
load(here::here("data-raw", "afsc_biomass.rda")) # afsc index data: object afsc_biomass
pbs_biomass <- readRDS(here::here("data-raw", "pbs-gfdata.rds")) #read in pbs index data
pbs_biomass <- do.call(rbind, lapply(names(pbs_biomass),function(spec){
  df <- pbs_biomass[[spec]]$survey_index #pull $survey_index df from each species list
  return(df)})) %>%  #apply function to each species data in list, bind together
  select(survey_abbrev, species_common_name, species_science_name, year, biomass, lowerci, upperci) %>%
  rename (common_name = species_common_name,
          region = survey_abbrev, 
          scientific_name = species_science_name ,
          est = biomass,
          lwr = lowerci,
          upr = upperci) #rename to have same format as NOAA data : object pbs_biomass

# DBI plot func (adapted from shiny version):
plot_dbi <- function(dbi_data, species, subregion) {
    plot <- ggplot2::ggplot(data = if (any(dbi_data$region == "U.S. West Coast")) {subset(dbi_data, dbi_data$common_name == species)}else {subset(dbi_data, dbi_data$common_name == species & dbi_data$region == subregion)}) +
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

## standardized index TBD